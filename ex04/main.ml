let () =
  let alkanes = [ new Alkane.methane; new Alkane.ethane; new Alkane.octane ] in
  let r = new Reaction.alkane_combustion alkanes in

  print_endline (string_of_bool r#is_balanced);
  print_endline "start----";
  try
    List.iter
      (fun p -> match p with s, n -> Printf.printf "%s : %d\n" s#formula n)
      r#get_start;

    print_endline "result----";
    List.iter
      (fun p -> match p with s, n -> Printf.printf "%s : %d\n" s#formula n)
      r#get_result
  with Failure msg -> (
    print_endline ("Error: " ^ msg);

    let n = r#balance in

    print_endline (string_of_bool n#is_balanced);
    print_endline "start----";
    try
      List.iter
        (fun p -> match p with s, n -> Printf.printf "%s : %d\n" s#formula n)
        n#get_start;

      print_endline "result----";
      List.iter
        (fun p -> match p with s, n -> Printf.printf "%s : %d\n" s#formula n)
        n#get_result
    with Failure msg -> print_endline ("Error: " ^ msg))
