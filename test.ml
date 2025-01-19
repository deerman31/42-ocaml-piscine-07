let () =
  let f1 = 1.5 in
  (* let f2 = 1.5 in *)
  print_endline (string_of_bool ((Float.rem f1 1.0) = 0.))
