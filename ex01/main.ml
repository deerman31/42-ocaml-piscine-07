let () =
  let h1 = new Hydrogen.hydrogen in
  let h2 = new Hydrogen.hydrogen in
  let o = new Oxygen.oxygen in

  let lst = [ o; h1; h2 ] in

  let w = new Water.water "water" lst in
  print_endline w#formula
