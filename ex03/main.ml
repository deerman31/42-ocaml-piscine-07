let () =
  print_endline "TEST <methane>";
  let m = new Alkane.methane in
  let m2 = new Alkane.methane in
  let other = new Alkane.ethane in
  print_endline m#name;
  print_endline m#formula;
  print_endline m#to_string;
  print_endline (string_of_bool (m#equals m2));
  print_endline (string_of_bool (m#equals other));
  print_endline "------------------------";

  print_endline "TEST <ethane>";
  let e = new Alkane.ethane in
  let e2 = new Alkane.ethane in
  print_endline e#name;
  print_endline e#formula;
  print_endline e#to_string;
  print_endline (string_of_bool (e#equals e2));
  print_endline (string_of_bool (e#equals m));
  print_endline "------------------------";

  print_endline "TEST <octane>";
  let o = new Alkane.octane in
  let o2 = new Alkane.octane in
  print_endline o#name;
  print_endline o#formula;
  print_endline o#to_string;
  print_endline (string_of_bool (o#equals o2));
  print_endline (string_of_bool (o#equals other));
  print_endline "------------------------"
