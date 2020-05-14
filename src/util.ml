
let get_unique_label count prefix =
  let label = prefix ^ (string_of_int !count) in
  count := !count + 1;
  label

