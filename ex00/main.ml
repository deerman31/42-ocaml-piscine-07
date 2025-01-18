let () =
  print_endline "TEST <hydrogen>";
  let h = new Atom.hydrogen in
  print_endline h#name;
  print_endline h#symbol;
  print_endline (string_of_int h#atomic_number);

  print_endline "TEST <carbon>";
  let c = new Atom.carbon in
  print_endline c#name;
  print_endline c#symbol;
  print_endline (string_of_int c#atomic_number);

  print_endline "TEST <oxygen>";
  let o = new Atom.oxygen in
  print_endline o#name;
  print_endline o#symbol;
  print_endline (string_of_int o#atomic_number);

  print_endline "TEST <fluorine>";
  let f = new Atom.fluorine in
  print_endline f#name;
  print_endline f#symbol;
  print_endline (string_of_int f#atomic_number);

  print_endline "TEST <sodium>";
  let na = new Atom.sodium in
  print_endline na#name;
  print_endline na#symbol;
  print_endline (string_of_int na#atomic_number);

  print_endline "TEST <magnesium>";
  let mg = new Atom.magnesium in
  print_endline mg#name;
  print_endline mg#symbol;
  print_endline (string_of_int mg#atomic_number)
