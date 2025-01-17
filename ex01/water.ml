class water (input_name : string) (input_atoms : Atom.atom list) =
  object (self)
    inherit Molecule.molecule
    val atoms = input_atoms
    method name = input_name

    method formula =
      let atoms_sort_compare a1 a2 =
        let symbol1 = a1#symbol in
        let symbol2 = a2#symbol in
        compare (String.get symbol1 0) (String.get symbol2 0)
      in
      let set_symbol_cnt cnt = if cnt = 1 then "" else string_of_int cnt in

      let get_formula lst =
        let rec loop acc symbol cnt lst =
          match lst with
          | [] -> acc ^ set_symbol_cnt cnt
          | first :: rest ->
              if acc = "" then loop first#symbol first#symbol 1 rest
              else if symbol = first#symbol then loop acc symbol (cnt + 1) rest
              else
                loop
                  (acc ^ set_symbol_cnt cnt ^ first#symbol)
                  first#symbol 1 rest
        in
        loop "" "" 0 lst
      in

      get_formula (List.sort atoms_sort_compare atoms)

    method to_string =
      "It is a compound of hydrogen and oxygen, represented by the chemical \
       formula H2O."

    method equals m = input_name = m#name
  end
