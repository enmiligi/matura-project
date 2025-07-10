pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/TargetMachine.h");
});
const AST = @import("ast.zig").AST;
const std = @import("std");
const Type = @import("./type_inference.zig").Type;
const AlgorithmJ = @import("./type_inference.zig").AlgorithmJ;

const MaybeRecursive = struct {
    value: c.LLVMValueRef,
    recursive: bool,
};

pub const Compiler = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    function: c.LLVMValueRef,
    powFunction: c.LLVMValueRef,
    powType: c.LLVMTypeRef,
    idToValue: std.StringHashMap(MaybeRecursive),
    idToFunctionNumber: std.StringHashMap(usize),
    currentFunction: usize,
    allocator: std.mem.Allocator,
    targetTriple: [*c]u8,
    algorithmJ: *AlgorithmJ,

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
                const prevFunction = self.function;
                var impEnclosed = std.ArrayList(c.LLVMTypeRef).init(self.allocator);
                defer impEnclosed.deinit();
                for (lambda.enclosesTypes.?.items) |t| {
                    var _structArgs: usize = 0;
                    try impEnclosed.append(self.impToLLVMType(t, &_structArgs));
                }
                const boundStruct = c.LLVMStructType(impEnclosed.items.ptr, @intCast(impEnclosed.items.len), 0);
                const boundPtr = c.LLVMPointerType(boundStruct, 0);
                var functionType: c.LLVMTypeRef = undefined;
                if (structArgs == 0) {
                    var functionArgs = [2]c.LLVMTypeRef{ argType, boundPtr };
                    functionType = c.LLVMFunctionType(returnType, &functionArgs, 2, 0);
                } else {
                    var functionArgs = [3]c.LLVMTypeRef{
                        c.LLVMPointerType(returnType, 0),
                        argType,
                        boundPtr,
                    };
                    functionType = c.LLVMFunctionType(c.LLVMVoidType(), &functionArgs, 3, 0);
                }
                const functionName = try std.fmt.allocPrint(self.allocator, "fun_{d}", .{self.currentFunction});
                defer self.allocator.free(functionName);
                self.currentFunction += 1;
                const function = c.LLVMAddFunction(self.module, functionName.ptr, functionType);
                if (structArgs != 0) {
                    c.LLVMAddAttributeAtIndex(function, 1, c.LLVMCreateTypeAttribute(
                        self.context,
                        c.LLVMGetEnumAttributeKindForName("sret", 4),
                        returnType,
                    ));
                    const resultArg = c.LLVMGetParam(function, 0);
                    c.LLVMSetValueName2(resultArg, "result", 6);
                }
                const param = c.LLVMGetParam(function, 0 + @as(c_uint, @intFromBool(structArgs != 0)));
                c.LLVMSetValueName2(param, lambda.argname.lexeme.ptr, lambda.argname.lexeme.len);
                try self.idToValue.put(lambda.argname.lexeme, .{ .value = param, .recursive = false });
                defer _ = self.idToValue.remove(lambda.argname.lexeme);
                const boundParam = c.LLVMGetParam(function, 1 + @as(c_uint, @intFromBool(structArgs != 0)));
                c.LLVMSetValueName(boundParam, "bound");
                const functionBB = c.LLVMAppendBasicBlock(function, "entry");
                const originalBuilderPos = c.LLVMGetInsertBlock(self.builder);
                c.LLVMPositionBuilderAtEnd(self.builder, functionBB);
                self.function = function;
                var preBoundVals = std.ArrayList(MaybeRecursive).init(self.allocator);
                defer preBoundVals.deinit();
                for (lambda.encloses.?.items) |name| {
                    try preBoundVals.append(self.idToValue.get(name).?);
                }
                for (impEnclosed.items, lambda.encloses.?.items, 0..) |t, name, i| {
                    var varPtr = c.LLVMBuildStructGEP2(
                        self.builder,
                        boundStruct,
                        boundParam,
                        @intCast(i),
                        "boundPtr",
                    );
                    if (self.idToValue.get(name).?.recursive == true) {
                        varPtr = c.LLVMBuildLoad2(
                            self.builder,
                            c.LLVMPointerType(t, 0),
                            varPtr,
                            "recursivePtr",
                        );
                    }
                    const loadedVar = c.LLVMBuildLoad2(self.builder, t, varPtr, "boundVar");
                    try self.idToValue.put(name, .{ .value = loadedVar, .recursive = false });
                }
                const result = try self.compileExpr(lambda.expr);
                if (structArgs != 0) {
                    const returnValue = c.LLVMGetParam(function, 0);
                    _ = c.LLVMBuildStore(self.builder, result, returnValue);
                    _ = c.LLVMBuildRetVoid(self.builder);
                } else {
                    _ = c.LLVMBuildRet(self.builder, result);
                }
                c.LLVMPositionBuilderAtEnd(self.builder, originalBuilderPos);
                self.function = prevFunction;
                for (lambda.encloses.?.items, preBoundVals.items) |enclosed, value| {
                    try self.idToValue.put(enclosed, value);
                }
                const closureType = self.impToLLVMType(lambda.type.?, &structArgs);
                const closurePtr = c.LLVMBuildAlloca(self.builder, closureType, "closure");
                const boundAlloca = c.LLVMBuildMalloc(self.builder, boundStruct, "bound");
                for (lambda.encloses.?.items, 0..) |name, i| {
                    const addr = c.LLVMBuildStructGEP2(self.builder, boundStruct, boundAlloca, @intCast(i), "var");
                    _ = c.LLVMBuildStore(self.builder, self.idToValue.get(name).?.value, addr);
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
                var structArgs: usize = 0;
                const returnType = self.impToLLVMType(call.functionType.?.data.function.to, &structArgs);
                var function: c.LLVMValueRef = undefined;
                switch (call.function.*) {
                    .identifier => |id| {
                        if (self.idToFunctionNumber.get(id.token.lexeme)) |functionNumber| {
                            const functionName = try std.fmt.allocPrint(self.allocator, "fun_{d}", .{functionNumber});
                            defer self.allocator.free(functionName);
                            function = c.LLVMGetNamedFunction(self.module, functionName.ptr);
                        } else {
                            function = c.LLVMBuildExtractValue(self.builder, closure, 0, "functionPointer");
                        }
                    },
                    else => {
                        function = c.LLVMBuildExtractValue(self.builder, closure, 0, "functionPointer");
                    },
                }
                const boundPtr = c.LLVMBuildExtractValue(self.builder, closure, 1, "bound");
                const argument = try self.compileExpr(call.arg);
                if (structArgs != 0) {
                    const result = c.LLVMBuildAlloca(self.builder, returnType, "result");
                    var args = [3]c.LLVMValueRef{
                        result,
                        argument,
                        boundPtr,
                    };
                    var param_types = [3]c.LLVMTypeRef{
                        // To simplify the case of outputting structs,
                        // they are returned by changing the value at a given pointer
                        c.LLVMPointerType(self.impToLLVMType(call.functionType.?.data.function.to, &structArgs), 0),
                        self.impToLLVMType(call.functionType.?.data.function.from, &structArgs),
                        c.LLVMPointerType(c.LLVMInt8Type(), 0),
                    };
                    const fn_type = c.LLVMFunctionType(c.LLVMVoidType(), &param_types, 3, 0);
                    _ = c.LLVMBuildCall2(self.builder, fn_type, function, &args, 3, "");
                    const loadedResult = c.LLVMBuildLoad2(self.builder, returnType, result, "value");
                    return loadedResult;
                } else {
                    var args = [2]c.LLVMValueRef{
                        argument,
                        boundPtr,
                    };
                    var param_types = [2]c.LLVMTypeRef{
                        self.impToLLVMType(call.functionType.?.data.function.from, &structArgs),
                        c.LLVMPointerType(c.LLVMInt8Type(), 0),
                    };
                    const fn_type = c.LLVMFunctionType(returnType, &param_types, 2, 0);
                    return c.LLVMBuildCall2(self.builder, fn_type, function, &args, 2, "");
                }
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
                const isLambda = switch (let.be.*) {
                    .lambda => true,
                    else => false,
                };
                var valueType: *Type = undefined;
                if (isLambda) {
                    switch (let.actualType.?.*) {
                        .type => |typeOfId| {
                            typeOfId.rc += 1;
                            valueType = typeOfId;
                        },
                        .forall => |*forall| {
                            valueType = try self.algorithmJ.instantiate(forall);
                        },
                    }
                }
                defer if (isLambda) valueType.deinit(self.allocator);
                var indirectionPointer: c.LLVMValueRef = undefined;
                if (isLambda) {
                    var _structArgs: usize = 0;
                    indirectionPointer = c.LLVMBuildMalloc(
                        self.builder,
                        self.impToLLVMType(valueType, &_structArgs),
                        "indirectionPtr",
                    );
                    try self.idToValue.put(let.name.lexeme, .{ .value = indirectionPointer, .recursive = true });
                    try self.idToFunctionNumber.put(let.name.lexeme, self.currentFunction);
                }
                defer if (isLambda) {
                    _ = self.idToFunctionNumber.remove(let.name.lexeme);
                };
                defer _ = self.idToValue.remove(let.name.lexeme);
                const value = try self.compileExpr(let.be);
                if (isLambda) _ = c.LLVMBuildStore(self.builder, value, indirectionPointer);
                try self.idToValue.put(let.name.lexeme, .{ .value = value, .recursive = false });
                return self.compileExpr(let.in);
            },
            .identifier => |id| {
                if (self.idToValue.get(id.token.lexeme) == null) {
                    // TODO: Print error message
                    return error.UnknownIdentifier;
                }
                return self.idToValue.get(id.token.lexeme).?.value;
            },
            .ifExpr => |ifExpr| {
                const condition = try self.compileExpr(ifExpr.predicate);
                const originalPos = c.LLVMGetInsertBlock(self.builder);
                var thenBlock = c.LLVMAppendBasicBlock(self.function, "then");
                c.LLVMPositionBuilderAtEnd(self.builder, thenBlock);
                const thenValue = try self.compileExpr(ifExpr.thenExpr);
                thenBlock = c.LLVMGetInsertBlock(self.builder);
                var elseBlock = c.LLVMAppendBasicBlock(self.function, "else");
                c.LLVMPositionBuilderAtEnd(self.builder, elseBlock);
                const elseValue = try self.compileExpr(ifExpr.elseExpr);
                elseBlock = c.LLVMGetInsertBlock(self.builder);
                c.LLVMPositionBuilderAtEnd(self.builder, originalPos);
                _ = c.LLVMBuildCondBr(self.builder, condition, thenBlock, elseBlock);
                const ifContinueBlock = c.LLVMAppendBasicBlock(self.function, "ifContinue");
                c.LLVMPositionBuilderAtEnd(self.builder, thenBlock);
                _ = c.LLVMBuildBr(self.builder, ifContinueBlock);
                c.LLVMPositionBuilderAtEnd(self.builder, elseBlock);
                _ = c.LLVMBuildBr(self.builder, ifContinueBlock);
                c.LLVMPositionBuilderAtEnd(self.builder, ifContinueBlock);
                var _structArgs: usize = 0;
                const resultType = self.impToLLVMType(ifExpr.resultType.?, &_structArgs);
                const phi = c.LLVMBuildPhi(self.builder, resultType, "ifResult");
                var incomingValues = [2]c.LLVMValueRef{ thenValue, elseValue };
                var incomingBlocks = [2]c.LLVMBasicBlockRef{ thenBlock, elseBlock };
                c.LLVMAddIncoming(phi, &incomingValues, &incomingBlocks, 2);
                return phi;
            },
            else => {
                return null;
            },
        }
    }

    pub fn init(allocator: std.mem.Allocator, algorithmJ: *AlgorithmJ) Compiler {
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
            .idToFunctionNumber = .init(allocator),
            .currentFunction = 0,
            .allocator = allocator,
            .targetTriple = targetTriple,
            .algorithmJ = algorithmJ,
        };
    }

    pub fn deinit(self: *Compiler) void {
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
        c.LLVMDisposeMessage(self.targetTriple);
        c.LLVMShutdown();
        self.idToValue.deinit();
        self.idToFunctionNumber.deinit();
    }
};
