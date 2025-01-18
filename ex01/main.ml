let () =
  print_endline "TEST <water>";
  let w = new Molecule.water in
  let w2 = new Molecule.water in
  let other = new Molecule.carbon_dioxide in
  print_endline w#name;
  print_endline w#formula;
  print_endline w#to_string;
  print_endline (string_of_bool (w#equals w2));
  print_endline (string_of_bool (w#equals other));
  print_endline "-----------------";

  print_endline "TEST <carbon dioxide>";
  let cd1 = new Molecule.carbon_dioxide in
  let cd2 = new Molecule.carbon_dioxide in
  print_endline cd1#name;
  print_endline cd1#formula;
  print_endline cd1#to_string;
  print_endline (string_of_bool (cd1#equals cd2));
  print_endline (string_of_bool (cd1#equals w));
  print_endline "-----------------";

  print_endline "TEST <ethylene>";
  let et1 = new Molecule.ethylene in
  let et2 = new Molecule.ethylene in
  print_endline et1#name;
  print_endline et1#formula;
  print_endline et1#to_string;
  print_endline (string_of_bool (et1#equals et2));
  print_endline (string_of_bool (et1#equals w));
  print_endline "-----------------";

  print_endline "TEST <glucose>";
  let g1 = new Molecule.glucose in
  let g2 = new Molecule.glucose in
  print_endline g1#name;
  print_endline g1#formula;
  print_endline g1#to_string;
  print_endline (string_of_bool (g1#equals g2));
  print_endline (string_of_bool (g1#equals w));
  print_endline "-----------------";

  print_endline "TEST <benzene>";
  let b1 = new Molecule.benzene in
  let b2 = new Molecule.benzene in
  print_endline b1#name;
  print_endline b1#formula;
  print_endline b1#to_string;
  print_endline (string_of_bool (b1#equals b2));
  print_endline (string_of_bool (b1#equals w));
  print_endline "-----------------"
