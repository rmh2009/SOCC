let get_unique_label count prefix =
  let label = prefix ^ string_of_int !count in
  count := !count + 1;
  label

let make_aligned_number number align =
  if number = 0 then 0
  else if number > 0 then
    let r = number mod align in
    if r = 0 then number else number + align - r
  else
    let r = (-number) mod align in
    if r = 0 then number else number - align + r
