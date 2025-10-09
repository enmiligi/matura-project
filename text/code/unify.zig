pub fn unify(self: *AlgorithmJ, typeA: *Type, typeB: *Type) !void {
    switch (typeA.data) {
        .typeVar => |*typeVarA| {
            // If A is a typeVar, unify its substitution if present
            // else add a Substitution unless 'a' should be replaced with
            // something containing 'a'
            if (typeVarA.subst) |substitutedA| {
                try self.unify(substitutedA, typeB);
            } else {
                try addSubstitution(typeVarA, typeB);
            }
        },
        ...,
        .number => |numA| {
            switch (typeB.data) {
                .primitive => |prim| {
                    switch (prim) {
                        .Int, .Float => {
                            try self.unify(numA.variable, typeB);
                        },
                        .Bool, .Char => {
                            return error.CouldNotUnify;
                        },
                    }
                },
                ...,
            }
        },
    }
}
