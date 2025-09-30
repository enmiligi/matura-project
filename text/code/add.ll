define { ptr, ptr } @add(i64 %x, ptr %bound) {
entry:
  %int_ptr = getelementptr { i64 }, ptr null, i32 1
  %sizeof_int = ptrtoint ptr %int_ptr to i64
  %new_bound = call ptr @GC_malloc(i64 %sizeof_int)
  %x_location = getelementptr { i64 }, ptr %new_bound, i32 0, i32 0
  store i64 %x, ptr %x_location
  %add_const_closure = insertvalue { ptr, ptr } { ptr @add_const, ptr undef }, ptr %new_bound, 1
  ret { ptr, ptr } %add_const_closure
}

define i64 @add_const(i64 %y, ptr %bound) {
entry:
  %x_location = getelementptr { i64 }, ptr %bound, i32 0, i32 0
  %x = load i64, ptr %x_location
  %result = add i64 %x, %y
  ret i64 %result
}
