pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Target.h");
});
const AST = @import("ast.zig").AST;
const Statement = @import("ast.zig").Statement;
const Pattern = @import("ast.zig").Pattern;
const std = @import("std");
const Type = @import("./type_inference.zig").Type;
const AlgorithmJ = @import("./type_inference.zig").AlgorithmJ;
const equalInstantiation = @import("monomorphization.zig").Monomorphizer.equalInstantiation;

pub const Compiler = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    function: c.LLVMValueRef,
    targetMachine: c.LLVMTargetMachineRef,
    dataLayout: c.LLVMTargetDataRef,
    powFunction: c.LLVMValueRef,
    powType: c.LLVMTypeRef,
    idToValue: std.StringHashMap(c.LLVMValueRef),
    idToFunctionNumber: std.StringHashMap(usize),
    currentFunction: usize,
    allocator: std.mem.Allocator,
    targetTriple: [*c]u8,
    algorithmJ: *AlgorithmJ,
    namesToFree: std.ArrayList([]const u8),
    mainName: []const u8,
    gcMalloc: c.LLVMValueRef,
    gcMallocType: c.LLVMTypeRef,

    // User-defined types
    nameToMonomorphizations: std.StringHashMap(union(enum) {
        mono: struct {
            monoType: c.LLVMTypeRef,
            constructors: std.ArrayList(Statement.Constructor),
        },
        monos: struct {
            insts: std.ArrayList(Statement.TypeMonomorphization),
            llvmTypes: std.ArrayList(c.LLVMTypeRef),
        },
    }),
    nameToTagInfo: std.StringHashMap(struct {
        tag: usize,
        tagSize: usize,
        constructType: c.LLVMTypeRef,
        constructorType: c.LLVMTypeRef,
    }),
    stringType: c.LLVMTypeRef,

    fn impToLLVMType(self: *Compiler, t: *Type) c.LLVMTypeRef {
        switch (t.data) {
            .typeVar => |tV| {
                if (tV.subst) |subst| {
                    return self.impToLLVMType(subst);
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
                return self.impToLLVMType(num.variable);
            },
            .function => |function| {
                var param_types = [3]c.LLVMTypeRef{
                    // To simplify the case of outputting structs,
                    // all values are returned by changing the value at a given pointer
                    c.LLVMPointerType(self.impToLLVMType(function.to), 0),
                    self.impToLLVMType(function.from),
                    c.LLVMPointerType(c.LLVMInt8Type(), 0),
                };
                const fn_type = c.LLVMFunctionType(c.LLVMVoidType(), &param_types, 3, 0);
                var struct_types = [2]c.LLVMTypeRef{
                    c.LLVMPointerType(fn_type, 0),
                    c.LLVMPointerType(c.LLVMInt8Type(), 0),
                };
                return c.LLVMStructType(&struct_types, 2, 0);
            },
            .composite => |comp| {
                const monos = self.nameToMonomorphizations.get(comp.name).?;
                switch (monos) {
                    .mono => |mono| {
                        return mono.monoType;
                    },
                    .monos => |ms| {
                        const insts = ms.insts;
                        const llvmTypes = ms.llvmTypes;
                        for (insts.items, llvmTypes.items) |inst, llvmType| {
                            if (equalInstantiation(comp.args.items, inst.inst))
                                return llvmType;
                        }
                        return c.LLVMStructType(null, 0, 0);
                    },
                }
            },
        }
    }

    fn putValue(self: *Compiler, expr: *AST, name: []const u8) anyerror!void {
        const isLambda = switch (expr.*) {
            .lambda => true,
            else => false,
        };
        if (isLambda) {
            try self.idToFunctionNumber.put(name, self.currentFunction);
        }
        const value = try self.compileExpr(expr);
        c.LLVMSetValueName2(value, name.ptr, name.len);
        if (isLambda) {
            _ = self.idToFunctionNumber.remove(name);
        }
        try self.idToValue.put(name, value);
    }

    fn compileLet(
        self: *Compiler,
        name: []const u8,
        be: *AST,
        maybeMonomorphizations: ?std.ArrayList(*AST),
    ) !?std.ArrayList([]const u8) {
        if (maybeMonomorphizations) |monomorphizations| {
            var monomorphizationNames = try std.ArrayList([]const u8).initCapacity(
                self.allocator,
                monomorphizations.items.len,
            );
            errdefer monomorphizationNames.deinit(self.allocator);
            errdefer for (monomorphizationNames.items) |monomorphizationName| {
                self.allocator.free(monomorphizationName);
            };
            for (0..monomorphizations.items.len) |i| {
                const monomorphizationName = try std.fmt.allocPrint(
                    self.allocator,
                    "_{d}{s}",
                    .{ i, name },
                );
                errdefer self.allocator.free(monomorphizationName);
                try monomorphizationNames.append(self.allocator, monomorphizationName);
            }
            for (monomorphizations.items, 0..) |monomorphization, i| {
                try self.putValue(monomorphization, monomorphizationNames.items[i]);
            }
            return monomorphizationNames;
        }
        try self.putValue(be, name);
        return null;
    }

    fn innermostType(t: *Type) *Type {
        var innerMost = t;
        var foundInner = true;
        while (foundInner) {
            switch (innerMost.data) {
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
        return innerMost;
    }

    pub fn compileExpr(self: *Compiler, ast: *AST) anyerror!c.LLVMValueRef {
        switch (ast.*) {
            .lambda => |lambda| {
                const argType = self.impToLLVMType(lambda.type.?.data.function.from);
                const returnType = self.impToLLVMType(lambda.type.?.data.function.to);
                const prevFunction = self.function;
                var impEnclosed = std.ArrayList(c.LLVMTypeRef).empty;
                defer impEnclosed.deinit(self.allocator);
                const originalFunctionNumber = self.currentFunction;
                for (lambda.enclosesTypes.?.items, lambda.encloses.?.items) |t, id| {
                    if (self.idToFunctionNumber.get(id)) |functionNumber| {
                        if (functionNumber != self.currentFunction) {
                            try impEnclosed.append(
                                self.allocator,
                                c.LLVMPointerType(c.LLVMInt8Type(), 0),
                            );
                        }
                        continue;
                    }
                    try impEnclosed.append(self.allocator, self.impToLLVMType(t));
                }
                const boundStruct = c.LLVMStructType(impEnclosed.items.ptr, @intCast(impEnclosed.items.len), 0);
                const boundPtr = c.LLVMPointerType(boundStruct, 0);
                var functionType: c.LLVMTypeRef = undefined;
                var functionArgs = [2]c.LLVMTypeRef{ argType, boundPtr };
                functionType = c.LLVMFunctionType(returnType, &functionArgs, 2, 0);
                const functionName = try std.fmt.allocPrint(self.allocator, "fun_{d}", .{self.currentFunction});
                defer self.allocator.free(functionName);
                self.currentFunction += 1;
                const function = c.LLVMAddFunction(self.module, functionName.ptr, functionType);
                const param = c.LLVMGetParam(function, 0);
                c.LLVMSetValueName2(param, lambda.argname.lexeme.ptr, lambda.argname.lexeme.len);
                try self.idToValue.put(lambda.argname.lexeme, param);
                defer _ = self.idToValue.remove(lambda.argname.lexeme);
                const boundParam = c.LLVMGetParam(function, 1);
                c.LLVMSetValueName(boundParam, "bound");
                const functionBB = c.LLVMAppendBasicBlock(function, "entry");
                const originalBuilderPos = c.LLVMGetInsertBlock(self.builder);
                c.LLVMPositionBuilderAtEnd(self.builder, functionBB);
                self.function = function;
                var preBoundVals = std.ArrayList(c.LLVMValueRef).empty;
                defer preBoundVals.deinit(self.allocator);
                for (lambda.encloses.?.items) |name| {
                    try preBoundVals.append(self.allocator, self.idToValue.get(name) orelse undefined);
                }
                var enclosedRecursion: usize = 0;
                for (lambda.encloses.?.items, 0..) |name, i| {
                    if (self.idToFunctionNumber.get(name)) |functionNumber| {
                        if (functionNumber == originalFunctionNumber) {
                            try self.idToValue.put(name, boundParam);
                            enclosedRecursion += 1;
                            continue;
                        }
                    }
                    const varPtr = c.LLVMBuildStructGEP2(
                        self.builder,
                        boundStruct,
                        boundParam,
                        @intCast(i - enclosedRecursion),
                        "",
                    );
                    const loadedVar = c.LLVMBuildLoad2(self.builder, impEnclosed.items[i - enclosedRecursion], varPtr, "");
                    c.LLVMSetValueName2(loadedVar, name.ptr, name.len);
                    try self.idToValue.put(name, loadedVar);
                }
                const result = try self.compileExpr(lambda.expr);
                _ = c.LLVMBuildRet(self.builder, result);
                c.LLVMPositionBuilderAtEnd(self.builder, originalBuilderPos);
                self.function = prevFunction;
                for (lambda.encloses.?.items, preBoundVals.items) |enclosed, value| {
                    if (self.idToFunctionNumber.get(enclosed)) |functionNumber| {
                        if (functionNumber == originalFunctionNumber) {
                            continue;
                        }
                    }
                    try self.idToValue.put(enclosed, value);
                }
                const closureType = self.impToLLVMType(lambda.type.?);
                var closure = c.LLVMGetUndef(closureType);
                const boundMalloc = self.malloc(boundStruct);
                c.LLVMSetValueName(boundMalloc, "bound");
                enclosedRecursion = 0;
                for (lambda.encloses.?.items, 0..) |name, i| {
                    if (self.idToFunctionNumber.get(name)) |functionNumber| {
                        if (functionNumber == originalFunctionNumber) {
                            enclosedRecursion += 1;
                            continue;
                        }
                    }
                    const addr = c.LLVMBuildStructGEP2(self.builder, boundStruct, boundMalloc, @intCast(i - enclosedRecursion), "");
                    _ = c.LLVMBuildStore(self.builder, self.idToValue.get(name).?, addr);
                }
                closure = c.LLVMBuildInsertValue(self.builder, closure, function, 0, "");
                closure = c.LLVMBuildInsertValue(self.builder, closure, boundMalloc, 1, "");
                return closure;
            },
            .call => |call| {
                const closure = try self.compileExpr(call.function);
                const returnType = self.impToLLVMType(call.functionType.?.data.function.to);
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
                var args = [2]c.LLVMValueRef{
                    argument,
                    boundPtr,
                };
                var param_types = [2]c.LLVMTypeRef{
                    self.impToLLVMType(call.functionType.?.data.function.from),
                    c.LLVMPointerType(c.LLVMInt8Type(), 0),
                };
                const fn_type = c.LLVMFunctionType(returnType, &param_types, 2, 0);
                return c.LLVMBuildCall2(self.builder, fn_type, function, &args, 2, "");
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
                const innerMost = innermostType(op.argType.?);
                var isInt = false;
                var isFloat = false;
                var isChar = false;
                var isBool = false;
                switch (innerMost.data) {
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
                        return c.LLVMBuildCall2(self.builder, self.powType, self.powFunction, &args, 2, "");
                    },
                    '+' => {
                        if (isInt) {
                            return c.LLVMBuildAdd(self.builder, left, right, "");
                        } else if (isFloat) {
                            return c.LLVMBuildFAdd(self.builder, left, right, "");
                        }
                        unreachable;
                    },
                    '-' => {
                        if (isInt) {
                            return c.LLVMBuildSub(self.builder, left, right, "");
                        } else if (isFloat) {
                            return c.LLVMBuildFSub(self.builder, left, right, "");
                        }
                        unreachable;
                    },
                    '*' => {
                        if (isInt) {
                            return c.LLVMBuildMul(self.builder, left, right, "");
                        } else if (isFloat) {
                            return c.LLVMBuildFMul(self.builder, left, right, "");
                        }
                        unreachable;
                    },
                    '/' => {
                        if (isInt) {
                            return c.LLVMBuildSDiv(self.builder, left, right, "");
                        } else if (isFloat) {
                            return c.LLVMBuildFDiv(self.builder, left, right, "");
                        }
                        unreachable;
                    },
                    '<' => {
                        if (op.token.lexeme.len != 1) {
                            if (isInt) {
                                return c.LLVMBuildICmp(self.builder, c.LLVMIntSLE, left, right, "");
                            } else if (isFloat) {
                                return c.LLVMBuildFCmp(self.builder, c.LLVMRealOLE, left, right, "");
                            }
                        } else {
                            if (isInt) {
                                return c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, left, right, "");
                            } else if (isFloat) {
                                return c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, left, right, "");
                            }
                        }
                        unreachable;
                    },
                    '>' => {
                        if (op.token.lexeme.len != 1) {
                            if (isInt) {
                                return c.LLVMBuildICmp(self.builder, c.LLVMIntSGE, left, right, "");
                            } else if (isFloat) {
                                return c.LLVMBuildFCmp(self.builder, c.LLVMRealOGE, left, right, "");
                            }
                        } else {
                            if (isInt) {
                                return c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, left, right, "");
                            } else if (isFloat) {
                                return c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, left, right, "");
                            }
                        }
                        unreachable;
                    },
                    '=' => {
                        if (isInt or isChar) {
                            return c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, left, right, "");
                        } else if (isFloat) {
                            return c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, left, right, "");
                        }
                        unreachable;
                    },
                    '!' => {
                        if (isInt or isChar) {
                            return c.LLVMBuildICmp(self.builder, c.LLVMIntNE, left, right, "");
                        } else if (isFloat) {
                            return c.LLVMBuildFCmp(self.builder, c.LLVMRealUNE, left, right, "");
                        }
                        unreachable;
                    },
                    'a' => {
                        return c.LLVMBuildAnd(self.builder, left, right, "");
                    },
                    'o' => {
                        return c.LLVMBuildOr(self.builder, left, right, "");
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
                const innerMost = innermostType(op.argType.?);
                var isInt = false;
                var isFloat = false;
                switch (innerMost.data) {
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
                        return c.LLVMBuildNot(self.builder, arg, "");
                    },
                    '-' => {
                        if (isInt) {
                            return c.LLVMBuildNeg(self.builder, arg, "");
                        } else if (isFloat) {
                            return c.LLVMBuildFNeg(self.builder, arg, "");
                        }
                        unreachable;
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .let => |let| {
                var toFreeAndDelete = try self.compileLet(
                    let.name.lexeme,
                    let.be,
                    let.monomorphizations,
                );
                defer if (toFreeAndDelete) |*names| {
                    for (names.items) |name| {
                        _ = self.idToValue.remove(name);
                        self.allocator.free(name);
                    }
                    names.deinit(self.allocator);
                };
                return self.compileExpr(let.in);
            },
            .identifier => |id| {
                if (self.idToFunctionNumber.get(id.token.lexeme)) |functionNumber| {
                    var closArgs = [2]c.LLVMTypeRef{
                        c.LLVMPointerType(c.LLVMInt8Type(), 0),
                        c.LLVMPointerType(c.LLVMInt8Type(), 0),
                    };
                    const closStruct = c.LLVMStructType(&closArgs, 2, 0);
                    var clos = c.LLVMGetUndef(closStruct);
                    const functionName = try std.fmt.allocPrint(self.allocator, "fun_{d}", .{functionNumber});
                    defer self.allocator.free(functionName);
                    const function = c.LLVMGetNamedFunction(self.module, functionName.ptr);
                    clos = c.LLVMBuildInsertValue(self.builder, clos, function, 0, "");
                    clos = c.LLVMBuildInsertValue(self.builder, clos, self.idToValue.get(id.token.lexeme).?, 1, "");
                    return clos;
                }
                return self.idToValue.get(id.token.lexeme).?;
            },
            .ifExpr => |ifExpr| {
                const condition = try self.compileExpr(ifExpr.predicate);
                var thenBlock = c.LLVMAppendBasicBlock(self.function, "then");
                var elseBlock = c.LLVMAppendBasicBlock(self.function, "else");
                _ = c.LLVMBuildCondBr(self.builder, condition, thenBlock, elseBlock);
                c.LLVMPositionBuilderAtEnd(self.builder, thenBlock);
                const thenValue = try self.compileExpr(ifExpr.thenExpr);
                thenBlock = c.LLVMGetInsertBlock(self.builder);
                c.LLVMPositionBuilderAtEnd(self.builder, elseBlock);
                const elseValue = try self.compileExpr(ifExpr.elseExpr);
                elseBlock = c.LLVMGetInsertBlock(self.builder);
                const ifContinueBlock = c.LLVMAppendBasicBlock(self.function, "ifContinue");
                c.LLVMPositionBuilderAtEnd(self.builder, thenBlock);
                _ = c.LLVMBuildBr(self.builder, ifContinueBlock);
                c.LLVMPositionBuilderAtEnd(self.builder, elseBlock);
                _ = c.LLVMBuildBr(self.builder, ifContinueBlock);
                c.LLVMPositionBuilderAtEnd(self.builder, ifContinueBlock);
                const resultType = self.impToLLVMType(ifExpr.resultType.?);
                const phi = c.LLVMBuildPhi(self.builder, resultType, "ifResult");
                var incomingValues = [2]c.LLVMValueRef{ thenValue, elseValue };
                var incomingBlocks = [2]c.LLVMBasicBlockRef{ thenBlock, elseBlock };
                c.LLVMAddIncoming(phi, &incomingValues, &incomingBlocks, 2);
                return phi;
            },
            .case => |case| {
                const typeName = case.valueType.?.data.composite.name;
                const typeMonomorphizations = self.nameToMonomorphizations.get(typeName).?;
                const value = try self.compileExpr(case.value);
                switch (typeMonomorphizations) {
                    .mono => |mono| {
                        return self.compileCase(
                            case.bodies.items.len,
                            typeName,
                            mono.monoType,
                            value,
                            ast,
                        );
                    },
                    .monos => |monos| {
                        var mono: Statement.TypeMonomorphization = undefined;
                        var found = false;
                        for (monos.insts.items) |candidateMono| {
                            if (equalInstantiation(
                                case.valueType.?.data.composite.args.items,
                                candidateMono.inst,
                            )) {
                                mono = candidateMono;
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            return c.LLVMBuildUnreachable(self.builder);
                        }
                        for (case.patterns.items) |*pattern| {
                            for (mono.constructors.items) |constructor| {
                                const baseConstructorName = baseName(constructor.name.lexeme);
                                if (std.mem.eql(u8, pattern.name.lexeme, baseConstructorName)) {
                                    pattern.name.lexeme = constructor.name.lexeme;
                                }
                            }
                        }
                        const constructType = self.nameToTagInfo.get(
                            mono.constructors.items[0].name.lexeme,
                        ).?.constructType;
                        return self.compileCase(
                            case.bodies.items.len,
                            typeName,
                            constructType,
                            value,
                            ast,
                        );
                    },
                }
            },
            else => {
                return null;
            },
        }
    }

    fn baseName(monomorphized: []const u8) []const u8 {
        var i: usize = 1;
        while (std.ascii.isDigit(monomorphized[i])) {
            i += 1;
        }
        return monomorphized[i..];
    }

    fn compileCase(
        self: *Compiler,
        numCases: usize,
        typeName: []const u8,
        constructType: c.LLVMTypeRef,
        value: c.LLVMValueRef,
        caseExpr: *AST,
    ) !c.LLVMValueRef {
        var valueLoc = c.LLVMBuildAlloca(self.builder, constructType, "");
        _ = c.LLVMBuildStore(self.builder, value, valueLoc);
        const case = caseExpr.case;
        if (numCases == 1) {
            for (case.patterns.items, case.bodies.items) |pattern, body| {
                // All non-existent constructors have only the base name
                // meaning they are not in the list, since they only exist
                // in theory, but in reality they need to be monomorphized
                if (self.nameToTagInfo.contains(pattern.name.lexeme)) {
                    return self.compilePattern(
                        pattern,
                        typeName,
                        body,
                        valueLoc,
                    );
                }
            }
            return null;
        }
        const originalBB = c.LLVMGetInsertBlock(self.builder);
        const defaultBB = c.LLVMAppendBasicBlock(self.function, "default");
        c.LLVMPositionBuilderAtEnd(self.builder, defaultBB);
        _ = c.LLVMBuildUnreachable(self.builder);
        const resumeBB = c.LLVMAppendBasicBlock(self.function, "resumeSwitch");
        c.LLVMPositionBuilderAtEnd(self.builder, resumeBB);
        const phi = c.LLVMBuildPhi(
            self.builder,
            self.impToLLVMType(case.resultType.?),
            "caseResult",
        );
        c.LLVMPositionBuilderAtEnd(self.builder, originalBB);
        valueLoc = c.LLVMBuildStructGEP2(
            self.builder,
            constructType,
            valueLoc,
            1,
            "",
        );
        const tag = c.LLVMBuildExtractValue(self.builder, value, 0, "tag");
        const switchInst = c.LLVMBuildSwitch(
            self.builder,
            tag,
            defaultBB,
            @intCast(case.bodies.items.len),
        );
        for (case.patterns.items, case.bodies.items) |pattern, body| {
            if (self.nameToTagInfo.get(pattern.name.lexeme)) |tagInfo| {
                const copiedName = try self.allocator.dupeZ(u8, pattern.name.lexeme);
                defer self.allocator.free(copiedName);
                const caseBB = c.LLVMAppendBasicBlock(self.function, copiedName);
                _ = c.LLVMAddCase(
                    switchInst,
                    c.LLVMConstInt(
                        c.LLVMIntType(@intCast(tagInfo.tagSize)),
                        @intCast(tagInfo.tag),
                        0,
                    ),
                    caseBB,
                );
                c.LLVMPositionBuilderAtEnd(self.builder, caseBB);
                var bodyResult = try self.compilePattern(
                    pattern,
                    typeName,
                    body,
                    valueLoc,
                );
                var currentBB = c.LLVMGetInsertBlock(self.builder);
                _ = c.LLVMBuildBr(self.builder, resumeBB);
                c.LLVMAddIncoming(phi, &bodyResult, &currentBB, 1);
            }
        }
        c.LLVMPositionBuilderAtEnd(self.builder, resumeBB);
        return phi;
    }

    fn compilePattern(
        self: *Compiler,
        pattern: Pattern,
        typeName: []const u8,
        body: *AST,
        valueLoc: c.LLVMValueRef,
    ) !c.LLVMValueRef {
        const tagInfo = self.nameToTagInfo.get(pattern.name.lexeme).?;
        const constructorStruct = tagInfo.constructorType;
        for (pattern.values.items, pattern.types.?.items, 0..) |valueToken, valueType, i| {
            const valuePtr = c.LLVMBuildStructGEP2(
                self.builder,
                constructorStruct,
                valueLoc,
                @intCast(i),
                "",
            );
            var value = c.LLVMBuildLoad2(
                self.builder,
                c.LLVMStructGetTypeAtIndex(constructorStruct, @intCast(i)),
                valuePtr,
                "",
            );
            // Dereference recursion pointer
            if (self.containsType(typeName, valueType)) {
                value = c.LLVMBuildLoad2(
                    self.builder,
                    self.impToLLVMType(valueType),
                    value,
                    "",
                );
            }
            c.LLVMSetValueName2(
                value,
                valueToken.lexeme.ptr,
                valueToken.lexeme.len,
            );
            try self.idToValue.put(valueToken.lexeme, value);
        }
        defer for (pattern.values.items) |valueToken| {
            _ = self.idToValue.remove(valueToken.lexeme);
        };
        return self.compileExpr(body);
    }

    fn containsType(self: *Compiler, typeName: []const u8, t: *Type) bool {
        switch (t.data) {
            .typeVar => |tV| {
                return self.containsType(typeName, tV.subst.?);
            },
            .primitive => {
                return false;
            },
            .composite => |comp| {
                if (std.mem.eql(u8, typeName, comp.name)) {
                    return true;
                }
                for (comp.args.items) |arg| {
                    if (self.containsType(typeName, arg)) {
                        return true;
                    }
                }
                return false;
            },
            .function => |function| {
                const containsFrom = self.containsType(typeName, function.from);
                return containsFrom or self.containsType(typeName, function.to);
            },
            .number => {
                return false;
            },
        }
    }

    fn constructorType(
        self: *Compiler,
        typeName: []const u8,
        constructor: Statement.Constructor,
    ) !struct { type: c.LLVMTypeRef, args: []c.LLVMTypeRef } {
        const constructorStructArgs = try self.allocator.alloc(
            c.LLVMTypeRef,
            constructor.args.items.len,
        );
        errdefer self.allocator.free(constructorStructArgs);
        for (constructor.args.items, 0..) |arg, i| {
            if (self.containsType(typeName, arg)) {
                constructorStructArgs[i] = c.LLVMPointerType(
                    c.LLVMInt8Type(),
                    0,
                );
                continue;
            }
            constructorStructArgs[i] = self.impToLLVMType(arg);
        }
        const constructorStruct = c.LLVMStructType(
            constructorStructArgs.ptr,
            @intCast(constructorStructArgs.len),
            0,
        );
        return .{ .type = constructorStruct, .args = constructorStructArgs };
    }

    fn enumType(
        self: *Compiler,
        typeName: []const u8,
        constructors: std.ArrayList(Statement.Constructor),
    ) !c.LLVMTypeRef {
        var maxAlignment: c_uint = 0;
        var maxSize: c_ulonglong = 0;
        for (constructors.items) |constructor| {
            const constructorStructAndArgs = try self.constructorType(typeName, constructor);
            const constructorStruct = constructorStructAndArgs.type;
            self.allocator.free(constructorStructAndArgs.args);
            const alignment = c.LLVMABIAlignmentOfType(
                self.dataLayout,
                constructorStruct,
            );
            const size = c.LLVMABISizeOfType(
                self.dataLayout,
                constructorStruct,
            );
            if (alignment >= maxAlignment) {
                maxAlignment = alignment;
            }
            if (size > maxSize) {
                maxSize = size;
            }
        }
        const paddingLength = try std.math.divCeil(
            c_ulonglong,
            maxSize,
            maxAlignment,
        );
        const paddingList = c.LLVMArrayType(
            c.LLVMIntType(maxAlignment * 8),
            @intCast(paddingLength),
        );
        const indexOffset = std.math.log2_int_ceil(
            usize,
            constructors.items.len,
        );
        const structType = structType: {
            if (constructors.items.len == 1) {
                break :structType paddingList;
            } else {
                var members = [2]c.LLVMTypeRef{
                    c.LLVMIntType(@intCast(indexOffset)),
                    paddingList,
                };
                break :structType c.LLVMStructType(&members, 2, 0);
            }
        };
        return structType;
    }

    fn compileConstructor(
        self: *Compiler,
        typeName: []const u8,
        structType: c.LLVMTypeRef,
        constructor: Statement.Constructor,
        constructorNum: usize,
        numConstructors: usize,
        originalBB: c.LLVMBasicBlockRef,
    ) !void {
        const constructorStructAndArgs = try self.constructorType(
            typeName,
            constructor,
        );
        const constructorStruct = constructorStructAndArgs.type;
        const constructorStructArgs = constructorStructAndArgs.args;
        defer self.allocator.free(constructorStructArgs);
        var result: c.LLVMValueRef = undefined;

        if (constructor.args.items.len == 0) {
            // Put the number of the constructor in the value (tag)
            if (numConstructors != 1) {
                result = c.LLVMBuildInsertValue(
                    self.builder,
                    c.LLVMGetUndef(structType),
                    c.LLVMConstInt(
                        c.LLVMIntType(@intCast(std.math.log2_int_ceil(
                            usize,
                            numConstructors,
                        ))),
                        constructorNum,
                        0,
                    ),
                    0,
                    "",
                );
            } else {
                result = c.LLVMGetUndef(structType);
            }
        } else {

            // Create last function responsible for actually creating the value
            var argTypes = [2]c.LLVMTypeRef{
                self.impToLLVMType(
                    constructor.args.items[constructor.args.items.len - 1],
                ),
                c.LLVMPointerType(c.LLVMInt8Type(), 0),
            };
            var functionType = c.LLVMFunctionType(structType, &argTypes, 2, 0);
            const originalFunctionType = functionType;
            const constructorFunctionName = try std.fmt.allocPrint(
                self.allocator,
                "fun_{d}",
                .{self.currentFunction},
            );
            self.currentFunction += 1;
            defer self.allocator.free(constructorFunctionName);
            var function = c.LLVMAddFunction(self.module, constructorFunctionName.ptr, functionType);
            var bb = c.LLVMAppendBasicBlock(function, "entry");
            c.LLVMPositionBuilderAtEnd(self.builder, bb);

            // Reserve space for the value so pointer manipulation can be done
            const construct = c.LLVMBuildAlloca(self.builder, structType, "");

            var insertPoint = construct;

            // Put the number of the constructor in the value (tag)
            // and get the rest to put the actual values
            if (numConstructors != 1) {
                _ = c.LLVMBuildStore(
                    self.builder,
                    c.LLVMConstInt(
                        c.LLVMIntType(@intCast(std.math.log2_int_ceil(
                            usize,
                            numConstructors,
                        ))),
                        constructorNum,
                        0,
                    ),
                    construct,
                );
                insertPoint = c.LLVMBuildStructGEP2(
                    self.builder,
                    structType,
                    construct,
                    1,
                    "",
                );
            }

            // Copy all values over from the bound variable
            for (0..constructor.args.items.len - 1) |i| {
                const arg = constructor.args.items[i];
                const pointer = c.LLVMBuildStructGEP2(
                    self.builder,
                    constructorStruct,
                    c.LLVMGetParam(function, 1),
                    @intCast(i),
                    "",
                );
                var argType: c.LLVMTypeRef = self.impToLLVMType(arg);
                if (self.containsType(typeName, arg)) {
                    argType = c.LLVMPointerType(argType, 0);
                }

                const value = c.LLVMBuildLoad2(self.builder, argType, pointer, "");
                const resultPointer = c.LLVMBuildStructGEP2(
                    self.builder,
                    constructorStruct,
                    insertPoint,
                    @intCast(i),
                    "",
                );
                _ = c.LLVMBuildStore(
                    self.builder,
                    value,
                    resultPointer,
                );
            }

            // Create the pointer where the last value is needed
            const resultPointer = c.LLVMBuildStructGEP2(
                self.builder,
                constructorStruct,
                insertPoint,
                @intCast(constructor.args.items.len - 1),
                "",
            );

            // Get the value to insert
            var insertValue = c.LLVMGetParam(function, 0);

            // Allocate memory for the value in case of (optionally wrapped) recursion
            const typeValue = constructor.args.items[constructor.args.items.len - 1];
            if (self.containsType(typeName, typeValue)) {
                const allocInsert = self.malloc(argTypes[0]);
                _ = c.LLVMBuildStore(self.builder, insertValue, allocInsert);
                insertValue = allocInsert;
            }

            // Store the last value and return
            _ = c.LLVMBuildStore(
                self.builder,
                insertValue,
                resultPointer,
            );
            _ = c.LLVMBuildRet(self.builder, c.LLVMBuildLoad2(
                self.builder,
                structType,
                construct,
                "",
            ));

            // Create the curried functions needed for the constructor
            for (1..constructor.args.items.len) |i| {
                // Create the function
                const prevConstructorFunctionName = try std.fmt.allocPrint(
                    self.allocator,
                    "fun_{d}",
                    .{self.currentFunction},
                );
                defer self.allocator.free(prevConstructorFunctionName);
                self.currentFunction += 1;
                argTypes[0] = self.impToLLVMType(
                    constructor.args.items[constructor.args.items.len - i - 1],
                );
                var resultVals = [2]c.LLVMTypeRef{
                    c.LLVMPointerType(functionType, 0),
                    c.LLVMPointerType(c.LLVMInt8Type(), 0),
                };
                const resultType = c.LLVMStructType(&resultVals, 2, 0);
                functionType = c.LLVMFunctionType(resultType, &argTypes, 2, 0);
                const prevFunction = function;
                function = c.LLVMAddFunction(self.module, prevConstructorFunctionName.ptr, functionType);
                bb = c.LLVMAppendBasicBlock(function, "entry");
                c.LLVMPositionBuilderAtEnd(self.builder, bb);

                // All previous types are bound
                const newBoundType = c.LLVMStructType(
                    constructorStructArgs.ptr,
                    @intCast(constructor.args.items.len - i),
                    0,
                );
                const newBoundAlign = c.LLVMABIAlignmentOfType(
                    self.dataLayout,
                    newBoundType,
                );
                const newBound = self.malloc(newBoundType);
                c.LLVMSetValueName(newBound, "bound");
                // Unless nothing is bound, copy all bound arguments
                if (i != constructor.args.items.len - 1) {
                    const boundType = c.LLVMStructType(
                        constructorStructArgs.ptr,
                        @intCast(constructor.args.items.len - i - 1),
                        0,
                    );
                    const boundAlign = c.LLVMABIAlignmentOfType(
                        self.dataLayout,
                        boundType,
                    );
                    const boundSize = c.LLVMSizeOf(boundType);
                    _ = c.LLVMBuildMemCpy(
                        self.builder,
                        newBound,
                        newBoundAlign,
                        c.LLVMGetParam(function, 1),
                        boundAlign,
                        boundSize,
                    );
                }

                // Put the last value
                const newBoundElementPtr = c.LLVMBuildStructGEP2(
                    self.builder,
                    newBoundType,
                    newBound,
                    @intCast(constructor.args.items.len - i - 1),
                    "",
                );
                insertValue = c.LLVMGetParam(function, 0);
                const newValueType = constructor.args.items[constructor.args.items.len - i - 1];
                if (self.containsType(typeName, newValueType)) {
                    const allocInsert = self.malloc(argTypes[0]);
                    _ = c.LLVMBuildStore(self.builder, insertValue, allocInsert);
                    insertValue = allocInsert;
                }
                _ = c.LLVMBuildStore(
                    self.builder,
                    insertValue,
                    newBoundElementPtr,
                );

                // Create the closure
                var clos = c.LLVMBuildInsertValue(
                    self.builder,
                    c.LLVMGetUndef(resultType),
                    prevFunction,
                    0,
                    "",
                );
                clos = c.LLVMBuildInsertValue(
                    self.builder,
                    clos,
                    newBound,
                    1,
                    "clos",
                );
                _ = c.LLVMBuildRet(self.builder, clos);
            }

            // Create the final closure
            c.LLVMPositionBuilderAtEnd(self.builder, originalBB);
            var closElements = [2]c.LLVMTypeRef{
                c.LLVMPointerType(originalFunctionType, 0),
                c.LLVMPointerType(c.LLVMInt8Type(), 0),
            };
            const closType = c.LLVMStructType(&closElements, 2, 0);
            var clos = c.LLVMBuildInsertValue(
                self.builder,
                c.LLVMGetUndef(closType),
                function,
                0,
                "",
            );
            clos = c.LLVMBuildInsertValue(
                self.builder,
                clos,
                // Constructors never bind a value if not partially instantiated
                c.LLVMConstNull(c.LLVMPointerType(c.LLVMInt8Type(), 0)),
                1,
                "",
            );
            result = clos;
        }

        try self.idToValue.put(constructor.name.lexeme, result);
        try self.nameToTagInfo.put(constructor.name.lexeme, .{
            .tag = constructorNum,
            .tagSize = @intCast(std.math.log2_int_ceil(
                usize,
                numConstructors,
            )),
            .constructType = structType,
            .constructorType = constructorStruct,
        });
    }

    pub fn compile(self: *Compiler, statements: *std.ArrayList(Statement)) !void {
        for (statements.items) |statement| {
            switch (statement) {
                .let => |let| {
                    var toFreeAndDelete = try self.compileLet(
                        let.name.lexeme,
                        let.be,
                        let.monomorphizations,
                    );
                    if (toFreeAndDelete) |*names| {
                        defer names.deinit(self.allocator);
                        errdefer for (names.items) |name| {
                            self.allocator.free(name);
                        };
                        try self.namesToFree.appendSlice(self.allocator, names.items);
                    }
                },
                .type => |t| {
                    if (t.monomorphizations) |monos| {
                        var structTypes = try std.ArrayList(c.LLVMTypeRef).initCapacity(
                            self.allocator,
                            monos.items.len,
                        );
                        errdefer structTypes.deinit(self.allocator);
                        for (monos.items) |mono| {
                            const structType = try self.enumType(
                                t.name.lexeme,
                                mono.constructors,
                            );
                            try structTypes.append(self.allocator, structType);
                        }
                        try self.nameToMonomorphizations.put(
                            t.name.lexeme,
                            .{ .monos = .{ .insts = monos, .llvmTypes = structTypes } },
                        );
                        for (monos.items, structTypes.items) |mono, structType| {
                            const originalBB = c.LLVMGetInsertBlock(self.builder);
                            for (mono.constructors.items, 0..) |constructor, constructorNum| {
                                try self.compileConstructor(
                                    t.name.lexeme,
                                    structType,
                                    constructor,
                                    constructorNum,
                                    mono.constructors.items.len,
                                    originalBB,
                                );
                            }
                        }
                    } else {
                        const structType = try self.enumType(
                            t.name.lexeme,
                            t.constructors,
                        );
                        try self.nameToMonomorphizations.put(
                            t.name.lexeme,
                            .{ .mono = .{
                                .monoType = structType,
                                .constructors = t.constructors,
                            } },
                        );
                        const originalBB = c.LLVMGetInsertBlock(self.builder);
                        for (t.constructors.items, 0..) |constructor, constructorNum| {
                            try self.compileConstructor(
                                t.name.lexeme,
                                structType,
                                constructor,
                                constructorNum,
                                t.constructors.items.len,
                                originalBB,
                            );const
                        }
                    }
                },
            }
        }

        // Call the main function
        const voidType = c.LLVMStructType(null, 0, 0);
        var mainArgs = [2]c.LLVMTypeRef{
            voidType,
            c.LLVMPointerType(c.LLVMInt8Type(), 0),
        };
        const mainType = c.LLVMFunctionType(voidType, &mainArgs, 2, 0);

        const mainClosure = self.idToValue.get(self.mainName).?;
        const mainFunction = c.LLVMBuildExtractValue(
            self.builder,
            mainClosure,
            0,
            "main",
        );
        const mainBound = c.LLVMBuildExtractValue(
            self.builder,
            mainClosure,
            1,
            "mainBound",
        );
        const voidValue = c.LLVMGetUndef(voidType);
        var args = [2]c.LLVMValueRef{
            voidValue,
            mainBound,
        };

        _ = c.LLVMBuildCall2(self.builder, mainType, mainFunction, &args, 2, "");

        _ = c.LLVMBuildRet(
            self.builder,
            c.LLVMConstInt(c.LLVMInt64Type(), 0, 0),
        );
    }

    const initError = error{TargetError};

    fn malloc(self: Compiler, t: c.LLVMTypeRef) c.LLVMValueRef {
        var size = c.LLVMSizeOf(t);
        const call = c.LLVMBuildCall2(
            self.builder,
            self.gcMallocType,
            self.gcMalloc,
            &size,
            1,
            "",
        );
        return call;
    }

    pub fn init(
        allocator: std.mem.Allocator,
        algorithmJ: *AlgorithmJ,
        mainName: []const u8,
    ) !Compiler {
        c.LLVMInitializeAllTargetInfos();
        c.LLVMInitializeAllTargets();
        c.LLVMInitializeAllTargetMCs();
        c.LLVMInitializeAllAsmParsers();
        c.LLVMInitializeAllAsmPrinters();
        const context = c.LLVMContextCreate();
        const module = c.LLVMModuleCreateWithNameInContext("Imp", context);
        const targetTriple = c.LLVMGetDefaultTargetTriple();
        c.LLVMSetTarget(module, targetTriple);
        var mainArgs = [0]c.LLVMTypeRef{};
        const mainType = c.LLVMFunctionType(c.LLVMInt64Type(), &mainArgs, 0, 0);
        const topLevel = c.LLVMAddFunction(module, "IMP_main", mainType);
        const topLevelBlock = c.LLVMAppendBasicBlock(topLevel, "entry");
        const builder = c.LLVMCreateBuilderInContext(context);
        c.LLVMPositionBuilderAtEnd(builder, topLevelBlock);
        var target: c.LLVMTargetRef = undefined;
        var errRef: [*c]u8 = undefined;
        const result = c.LLVMGetTargetFromTriple(targetTriple, &target, &errRef);
        if (result != 0) {
            return error.TargetError;
        }
        const targetMachine = c.LLVMCreateTargetMachine(
            target,
            targetTriple,
            "generic",
            "",
            c.LLVMCodeGenLevelDefault,
            c.LLVMRelocDefault,
            c.LLVMCodeModelDefault,
        );
        const dataLayout = c.LLVMCreateTargetDataLayout(targetMachine);
        var param_types = [2]c.LLVMTypeRef{ c.LLVMDoubleType(), c.LLVMDoubleType() };
        const fn_type = c.LLVMFunctionType(c.LLVMDoubleType(), &param_types, 2, 0);
        const pow_intrinsic = c.LLVMAddFunction(module, "llvm.pow.f64", fn_type);

        const layoutStr = c.LLVMCopyStringRepOfTargetData(dataLayout);
        defer c.LLVMDisposeMessage(layoutStr);
        c.LLVMSetDataLayout(module, layoutStr);

        const pointerSize = c.LLVMPointerSize(
            dataLayout,
        );
        const pointerAlignment = c.LLVMABIAlignmentOfType(
            dataLayout,
            c.LLVMPointerType(c.LLVMInt8Type(), 0),
        );
        const stringContents = c.LLVMArrayType(
            c.LLVMIntType(pointerAlignment * 8),
            try std.math.divCeil(
                c_uint,
                @intCast(pointerSize + 1),
                pointerAlignment,
            ),
        );
        var stringItems = [2]c.LLVMTypeRef{
            c.LLVMInt1Type(),
            stringContents,
        };
        const stringType = c.LLVMStructType(&stringItems, 2, 0);

        var size_t = c.LLVMIntType(pointerSize * 8);
        const mallocType = c.LLVMFunctionType(
            c.LLVMPointerType(c.LLVMInt8Type(), 0),
            &size_t,
            1,
            0,
        );
        const gcMalloc = c.LLVMAddFunction(module, "GC_malloc", mallocType);

        const gcInitType = c.LLVMFunctionType(
            c.LLVMVoidType(),
            null,
            0,
            0,
        );
        const gcInit = c.LLVMAddFunction(module, "GC_init", gcInitType);
        _ = c.LLVMBuildCall2(builder, gcInitType, gcInit, null, 0, "");

        return .{
            .context = context,
            .module = module,
            .builder = builder,
            .function = topLevel,
            .targetMachine = targetMachine,
            .dataLayout = dataLayout,
            .powFunction = pow_intrinsic,
            .powType = fn_type,
            .stringType = stringType,
            .idToValue = .init(allocator),
            .idToFunctionNumber = .init(allocator),
            .namesToFree = .empty,
            .nameToMonomorphizations = .init(allocator),
            .nameToTagInfo = .init(allocator),
            .currentFunction = 0,
            .allocator = allocator,
            .targetTriple = targetTriple,
            .algorithmJ = algorithmJ,
            .mainName = mainName,
            .gcMalloc = gcMalloc,
            .gcMallocType = mallocType,
        };
    }

    pub fn initBuiltins(self: *Compiler) !void {
        var optionArgs = [2]c.LLVMTypeRef{
            c.LLVMInt1Type(),
            c.LLVMArrayType(c.LLVMInt64Type(), 1),
        };
        const option = c.LLVMStructType(&optionArgs, 2, 0);
        try self.initBuiltin(
            "IMP_parseInt\x00",
            self.stringType,
            option,
        );
        try self.initBuiltin(
            "IMP_parseFloat\x00",
            self.stringType,
            option,
        );

        try self.initBuiltin(
            "IMP_showInt\x00",
            c.LLVMInt64Type(),
            self.stringType,
        );
        try self.initBuiltin(
            "IMP_showFloat\x00",
            c.LLVMDoubleType(),
            self.stringType,
        );
        try self.initBuiltin(
            "IMP_read\x00",
            c.LLVMStructType(null, 0, 0),
            self.stringType,
        );
        try self.initBuiltin(
            "IMP_print\x00",
            self.stringType,
            c.LLVMStructType(null, 0, 0),
        );
    }

    // Name should be null terminated
    pub fn initBuiltin(
        self: *Compiler,
        name: []const u8,
        argType: c.LLVMTypeRef,
        returnType: c.LLVMTypeRef,
    ) !void {
        var functionArgs = [2]c.LLVMTypeRef{
            c.LLVMPointerType(argType, 0),
            c.LLVMPointerType(returnType, 0),
        };
        const functionType = c.LLVMFunctionType(
            c.LLVMVoidType(),
            &functionArgs,
            2,
            0,
        );
        const function = c.LLVMAddFunction(self.module, name.ptr, functionType);

        var wrapperArgs = [2]c.LLVMTypeRef{
            argType,
            c.LLVMPointerType(c.LLVMInt8Type(), 0),
        };
        const wrapperType = c.LLVMFunctionType(returnType, &wrapperArgs, 2, 0);
        const wrapperName = try std.fmt.allocPrint(
            self.allocator,
            "fun_{d}\x00",
            .{self.currentFunction},
        );
        self.currentFunction += 1;
        defer self.allocator.free(wrapperName);
        const wrapper = c.LLVMAddFunction(
            self.module,
            wrapperName.ptr,
            wrapperType,
        );
        const originalBB = c.LLVMGetInsertBlock(self.builder);
        const entryBB = c.LLVMAppendBasicBlock(wrapper, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entryBB);

        const returnPtr = c.LLVMBuildAlloca(
            self.builder,
            returnType,
            "returnPtr",
        );
        const argPtr = c.LLVMBuildAlloca(
            self.builder,
            argType,
            "argPtr",
        );
        _ = c.LLVMBuildStore(self.builder, c.LLVMGetParam(wrapper, 0), argPtr);
        var args = [2]c.LLVMValueRef{ argPtr, returnPtr };
        _ = c.LLVMBuildCall2(self.builder, functionType, function, &args, 2, "");
        const result = c.LLVMBuildLoad2(
            self.builder,
            returnType,
            returnPtr,
            "result",
        );
        _ = c.LLVMBuildRet(self.builder, result);

        var closureArgs = [2]c.LLVMTypeRef{
            c.LLVMPointerType(functionType, 0),
            c.LLVMPointerType(c.LLVMInt8Type(), 0),
        };
        const closureType = c.LLVMStructType(&closureArgs, 2, 0);

        c.LLVMPositionBuilderAtEnd(self.builder, originalBB);

        var closure = c.LLVMGetUndef(closureType);
        closure = c.LLVMBuildInsertValue(
            self.builder,
            closure,
            wrapper,
            0,
            "",
        );
        closure = c.LLVMBuildInsertValue(
            self.builder,
            closure,
            c.LLVMConstNull(c.LLVMPointerType(c.LLVMInt8Type(), 0)),
            1,
            name.ptr,
        );

        // Cut off null and the "IMP_" prefix
        try self.idToValue.put(name[4 .. name.len - 1], closure);
    }

    pub fn deinit(self: *Compiler) void {
        c.LLVMDisposeTargetData(self.dataLayout);
        c.LLVMDisposeTargetMachine(self.targetMachine);
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
        c.LLVMDisposeMessage(self.targetTriple);
        c.LLVMShutdown();
        self.idToValue.deinit();
        self.idToFunctionNumber.deinit();
        for (self.namesToFree.items) |name| {
            self.allocator.free(name);
        }
        self.namesToFree.deinit(self.allocator);
        self.nameToTagInfo.deinit();
        var iterator = self.nameToMonomorphizations.valueIterator();
        while (iterator.next()) |val| {
            switch (val.*) {
                .monos => |*monos| {
                    monos.llvmTypes.deinit(self.allocator);
                },
                else => {},
            }
        }
        self.nameToMonomorphizations.deinit();
    }
};
