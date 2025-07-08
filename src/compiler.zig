pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/TargetMachine.h");
});
const AST = @import("ast.zig").AST;
const std = @import("std");
const Type = @import("./type_inference.zig").Type;

pub const Compiler = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    function: c.LLVMValueRef,
    powFunction: c.LLVMValueRef,
    powType: c.LLVMTypeRef,
    idToValue: std.StringHashMap(c.LLVMValueRef),
    currentFunction: usize,
    allocator: std.mem.Allocator,
    targetTriple: [*c]u8,

    fn impToLLVMType(self: *Compiler, t: *Type, structArgs: *usize) c.LLVMTypeRef {
        switch (t.data) {
            .typeVar => |tV| {
                if (tV.subst) |subst| {
                    return self.impToLLVMType(subst, structArgs);
                } else {
                    // If a type could be any type, set it to Int by default
                    return c.LLVMInt64Type();
                }
            },
            .primitive => |prim| {
                switch (prim) {
                    .Bool => {
                        return c.LLVMInt1Type();
                    },
                    .Int => {
                        return c.LLVMInt64Type();
                    },
                    .Float => {
                        return c.LLVMDoubleType();
                    },
                    .Char => {
                        return c.LLVMInt8Type();
                    },
                }
            },
            .number => |num| {
                // If no substitution, by default Int which is a number
                return self.impToLLVMType(num.variable, structArgs);
            },
            .function => |function| {
                var param_types = [3]c.LLVMTypeRef{
                    // To simplify the case of outputting structs,
                    // all values are returned by changing the value at a given pointer
                    c.LLVMPointerType(self.impToLLVMType(function.to, structArgs), 0),
                    self.impToLLVMType(function.from, structArgs),
                    c.LLVMPointerType(c.LLVMInt8Type(), 0),
                };
                const fn_type = c.LLVMFunctionType(c.LLVMVoidType(), &param_types, 3, 0);
                var struct_types = [2]c.LLVMTypeRef{
                    c.LLVMPointerType(fn_type, 0),
                    c.LLVMPointerType(c.LLVMInt8Type(), 0),
                };
                structArgs.* = 2;
                return c.LLVMStructType(&struct_types, 2, 0);
            },
            else => {
                return null;
            },
        }
    }

    pub fn compileExpr(self: *Compiler, ast: *AST) !c.LLVMValueRef {
        switch (ast.*) {
            .lambda => |lambda| {
                var structArgs: usize = 0;
                const argType = self.impToLLVMType(lambda.type.?.data.function.from, &structArgs);
                structArgs = 0;
                const returnType = self.impToLLVMType(lambda.type.?.data.function.to, &structArgs);
                const prevBuilder = self.builder;
                var impEnclosed = std.ArrayList(c.LLVMTypeRef).init(self.allocator);
                defer impEnclosed.deinit();
                for (lambda.enclosesTypes.?.items) |t| {
                    var _structArgs: usize = 0;
                    try impEnclosed.append(self.impToLLVMType(t, &_structArgs));
                }
                const boundStruct = c.LLVMStructType(impEnclosed.items.ptr, @intCast(impEnclosed.items.len), 0);
                const boundPtr = c.LLVMPointerType(boundStruct, 0);
                var functionArgs = [3]c.LLVMTypeRef{ c.LLVMPointerType(returnType, 0), argType, boundPtr };
                const functionType = c.LLVMFunctionType(c.LLVMVoidType(), &functionArgs, 3, 0);
                const functionName = try std.fmt.allocPrint(self.allocator, "fun_{d}", .{self.currentFunction});
                defer self.allocator.free(functionName);
                self.currentFunction += 1;
                const function = c.LLVMAddFunction(self.module, functionName.ptr, functionType);
                const returnValue = c.LLVMGetParam(function, 0);
                if (structArgs != 0) {
                    c.LLVMAddAttributeAtIndex(function, 1, c.LLVMCreateTypeAttribute(
                        self.context,
                        c.LLVMGetEnumAttributeKindForName("sret", 4),
                        returnType,
                    ));
                }
                const param = c.LLVMGetParam(function, 1);
                c.LLVMSetValueName2(param, lambda.argname.lexeme.ptr, lambda.argname.lexeme.len);
                try self.idToValue.put(lambda.argname.lexeme, param);
                defer _ = self.idToValue.remove(lambda.argname.lexeme);
                const boundParam = c.LLVMGetParam(function, 2);
                c.LLVMSetValueName(boundParam, "bound");
                const functionBB = c.LLVMAppendBasicBlock(function, "entry");
                const functionBuilder = c.LLVMCreateBuilder();
                defer c.LLVMDisposeBuilder(functionBuilder);
                c.LLVMPositionBuilderAtEnd(functionBuilder, functionBB);
                self.builder = functionBuilder;
                var preBoundVals = std.ArrayList(c.LLVMValueRef).init(self.allocator);
                defer preBoundVals.deinit();
                for (impEnclosed.items, lambda.encloses.?.items, 0..) |t, name, i| {
                    try preBoundVals.append(self.idToValue.get(name).?);
                    const varPtr = c.LLVMBuildStructGEP2(
                        self.builder,
                        boundStruct,
                        boundParam,
                        @intCast(i),
                        "boundPtr",
                    );
                    const loadedVar = c.LLVMBuildLoad2(self.builder, t, varPtr, "boundVar");
                    try self.idToValue.put(name, loadedVar);
                }
                const result = try self.compileExpr(lambda.expr);
                _ = c.LLVMBuildStore(self.builder, result, returnValue);
                _ = c.LLVMBuildRetVoid(self.builder);
                self.builder = prevBuilder;
                for (lambda.encloses.?.items, preBoundVals.items) |enclosed, value| {
                    try self.idToValue.put(enclosed, value);
                }
                const closureType = self.impToLLVMType(lambda.type.?, &structArgs);
                const closurePtr = c.LLVMBuildAlloca(self.builder, closureType, "closure");
                const boundAlloca = c.LLVMBuildMalloc(self.builder, boundStruct, "bound");
                for (lambda.encloses.?.items, 0..) |name, i| {
                    const addr = c.LLVMBuildStructGEP2(self.builder, boundStruct, boundAlloca, @intCast(i), "var");
                    _ = c.LLVMBuildStore(self.builder, self.idToValue.get(name).?, addr);
                }
                const funAddr = c.LLVMBuildStructGEP2(self.builder, closureType, closurePtr, 0, "function");
                _ = c.LLVMBuildStore(self.builder, function, funAddr);
                const boundAddr = c.LLVMBuildStructGEP2(self.builder, closureType, closurePtr, 1, "boundAddr");
                _ = c.LLVMBuildStore(self.builder, boundAlloca, boundAddr);
                const closure = c.LLVMBuildLoad2(self.builder, closureType, closurePtr, "loadClosure");
                return closure;
            },
            .call => |call| {
                const closure = try self.compileExpr(call.function);
                var _structArgs: usize = 0;
                const returnType = self.impToLLVMType(call.functionType.?.data.function.to, &_structArgs);
                const function = c.LLVMBuildExtractValue(self.builder, closure, 0, "functionPointer");
                const boundPtr = c.LLVMBuildExtractValue(self.builder, closure, 1, "bound");
                const result = c.LLVMBuildAlloca(self.builder, returnType, "result");
                const argument = try self.compileExpr(call.arg);
                var args = [3]c.LLVMValueRef{
                    result,
                    argument,
                    boundPtr,
                };
                var param_types = [3]c.LLVMTypeRef{
                    // To simplify the case of outputting structs,
                    // all values are returned by changing the value at a given pointer
                    c.LLVMPointerType(self.impToLLVMType(call.functionType.?.data.function.to, &_structArgs), 0),
                    self.impToLLVMType(call.functionType.?.data.function.from, &_structArgs),
                    c.LLVMPointerType(c.LLVMInt8Type(), 0),
                };
                const fn_type = c.LLVMFunctionType(c.LLVMVoidType(), &param_types, 3, 0);
                _ = c.LLVMBuildCall2(self.builder, fn_type, function, &args, 3, "");
                const loadedResult = c.LLVMBuildLoad2(self.builder, returnType, result, "value");
                return loadedResult;
            },
            .boolConstant => |b| {
                return c.LLVMConstInt(
                    c.LLVMInt1Type(),
                    if (b.value) 1 else 0,
                    0,
                );
            },
            .intConstant => |i| {
                return c.LLVMConstInt(
                    c.LLVMInt64Type(),
                    @bitCast(i.value),
                    0,
                );
            },
            .floatConstant => |f| {
                return c.LLVMConstReal(
                    c.LLVMDoubleType(),
                    f.value,
                );
            },
            .charConstant => |char| {
                return c.LLVMConstInt(c.LLVMInt8Type(), char.value, 0);
            },
            .operator => |op| {
                const left = try self.compileExpr(op.left);
                const right = try self.compileExpr(op.right);
                var innerMost = op.argType;
                var foundInner = true;
                while (foundInner) {
                    switch (innerMost.?.data) {
                        .typeVar => |tV| {
                            if (tV.subst) |subst| {
                                innerMost = subst;
                                foundInner = true;
                            } else {
                                foundInner = false;
                            }
                        },
                        .number => |number| {
                            innerMost = number.variable;
                            foundInner = true;
                        },
                        else => {
                            foundInner = false;
                        },
                    }
                }
                var isInt = false;
                var isFloat = false;
                var isChar = false;
                var isBool = false;
                switch (innerMost.?.data) {
                    .primitive => |prim| {
                        switch (prim) {
                            .Int => {
                                isInt = true;
                            },
                            .Float => {
                                isFloat = true;
                            },
                            .Char => {
                                isChar = true;
                            },
                            .Bool => {
                                isBool = true;
                            },
                        }
                    },
                    else => {},
                }
                switch (op.token.lexeme[0]) {
                    '^' => {
                        var args = [2]c.LLVMValueRef{ left, right };
                        return c.LLVMBuildCall2(self.builder, self.powType, self.powFunction, &args, 2, "exp");
                    },
                    '+' => {
                        if (isInt) {
                            return c.LLVMBuildAdd(self.builder, left, right, "add");
                        } else if (isFloat) {
                            return c.LLVMBuildFAdd(self.builder, left, right, "add");
                        }
                        unreachable;
                    },
                    '-' => {
                        if (isInt) {
                            return c.LLVMBuildSub(self.builder, left, right, "sub");
                        } else if (isFloat) {
                            return c.LLVMBuildFSub(self.builder, left, right, "sub");
                        }
                        unreachable;
                    },
                    '*' => {
                        if (isInt) {
                            return c.LLVMBuildMul(self.builder, left, right, "mul");
                        } else if (isFloat) {
                            return c.LLVMBuildFMul(self.builder, left, right, "mul");
                        }
                        unreachable;
                    },
                    '/' => {
                        if (isInt) {
                            return c.LLVMBuildSDiv(self.builder, left, right, "div");
                        } else if (isFloat) {
                            return c.LLVMBuildFDiv(self.builder, left, right, "div");
                        }
                        unreachable;
                    },
                    '<' => {
                        if (op.token.lexeme.len != 1) {
                            if (isInt) {
                                return c.LLVMBuildICmp(self.builder, c.LLVMIntSLE, left, right, "lt");
                            } else if (isFloat) {
                                return c.LLVMBuildFCmp(self.builder, c.LLVMRealOLE, left, right, "lt");
                            }
                        } else {
                            if (isInt) {
                                return c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, left, right, "lt");
                            } else if (isFloat) {
                                return c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, left, right, "lt");
                            }
                        }
                        unreachable;
                    },
                    '>' => {
                        if (op.token.lexeme.len != 1) {
                            if (isInt) {
                                return c.LLVMBuildICmp(self.builder, c.LLVMIntSGE, left, right, "lt");
                            } else if (isFloat) {
                                return c.LLVMBuildFCmp(self.builder, c.LLVMRealOGE, left, right, "lt");
                            }
                        } else {
                            if (isInt) {
                                return c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, left, right, "lt");
                            } else if (isFloat) {
                                return c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, left, right, "lt");
                            }
                        }
                        unreachable;
                    },
                    '=' => {
                        if (isInt or isChar or isBool) {
                            return c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, left, right, "lt");
                        } else if (isFloat) {
                            return c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, left, right, "lt");
                        }
                        unreachable;
                    },
                    '!' => {
                        if (isInt or isChar or isBool) {
                            return c.LLVMBuildICmp(self.builder, c.LLVMIntNE, left, right, "lt");
                        } else if (isFloat) {
                            return c.LLVMBuildFCmp(self.builder, c.LLVMRealUNE, left, right, "lt");
                        }
                        unreachable;
                    },
                    'a' => {
                        return c.LLVMBuildAnd(self.builder, left, right, "and");
                    },
                    'o' => {
                        return c.LLVMBuildOr(self.builder, left, right, "or");
                    },
                    ';' => {
                        return right;
                    },
                    else => {
                        return null;
                    },
                }
            },
            .prefixOp => |op| {
                var innerMost = op.argType;
                var foundInner = true;
                while (foundInner) {
                    switch (innerMost.?.data) {
                        .typeVar => |tV| {
                            if (tV.subst) |subst| {
                                innerMost = subst;
                                foundInner = true;
                            }
                            foundInner = false;
                        },
                        .number => |number| {
                            innerMost = number.variable;
                            foundInner = true;
                        },
                        else => {
                            foundInner = false;
                        },
                    }
                }
                var isInt = false;
                var isFloat = false;
                switch (innerMost.?.data) {
                    .primitive => |prim| {
                        switch (prim) {
                            .Int => {
                                isInt = true;
                            },
                            .Float => {
                                isFloat = true;
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
                const arg = try self.compileExpr(op.expr);
                switch (op.token.lexeme[0]) {
                    '!' => {
                        return c.LLVMBuildNot(self.builder, arg, "not");
                    },
                    '-' => {
                        if (isInt) {
                            return c.LLVMBuildNeg(self.builder, arg, "negate");
                        } else if (isFloat) {
                            return c.LLVMBuildFNeg(self.builder, arg, "negate");
                        }
                        unreachable;
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .let => |let| {
                const value = try self.compileExpr(let.be);
                try self.idToValue.put(let.name.lexeme, value);
                defer _ = self.idToValue.remove(let.name.lexeme);
                return self.compileExpr(let.in);
            },
            .identifier => |id| {
                return self.idToValue.get(id.token.lexeme).?;
            },
            else => {
                return null;
            },
        }
    }

    pub fn init(allocator: std.mem.Allocator) Compiler {
        const context = c.LLVMContextCreate();
        const module = c.LLVMModuleCreateWithNameInContext("Imp", context);
        const targetTriple = c.LLVMGetDefaultTargetTriple();
        c.LLVMSetTarget(module, targetTriple);
        var topLevelArgs = [0]c.LLVMTypeRef{};
        const topLevelType = c.LLVMFunctionType(c.LLVMVoidType(), &topLevelArgs, 0, 0);
        const topLevel = c.LLVMAddFunction(module, "topLevel", topLevelType);
        const topLevelBlock = c.LLVMAppendBasicBlock(topLevel, "entry");
        const builder = c.LLVMCreateBuilderInContext(context);
        c.LLVMPositionBuilderAtEnd(builder, topLevelBlock);
        var param_types = [2]c.LLVMTypeRef{ c.LLVMDoubleType(), c.LLVMDoubleType() };
        const fn_type = c.LLVMFunctionType(c.LLVMDoubleType(), &param_types, 2, 0);
        const pow_intrinsic = c.LLVMAddFunction(module, "llvm.pow.f64", fn_type);
        return .{
            .context = context,
            .module = module,
            .builder = builder,
            .function = topLevel,
            .powFunction = pow_intrinsic,
            .powType = fn_type,
            .idToValue = .init(allocator),
            .currentFunction = 0,
            .allocator = allocator,
            .targetTriple = targetTriple,
        };
    }

    pub fn deinit(self: *Compiler) void {
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
        c.LLVMDisposeMessage(self.targetTriple);
        c.LLVMShutdown();
        self.idToValue.deinit();
    }
};
