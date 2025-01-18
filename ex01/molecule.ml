class virtual molecule (input_name : string) (input_atoms : Atom.atom list) =
  object (self)
    val atoms = input_atoms
    method name = input_name

    method formula : string =
      let atoms_sort_compare a1 a2 =
        let symbol1 = a1#symbol in
        let symbol2 = a2#symbol in
        compare (String.get symbol1 0) (String.get symbol2 0)
      in
      let set_symbol_cnt cnt = if cnt = 1 then "" else string_of_int cnt in

      let get_formula lst =
        if lst = [] then ""
        else
          let rec loop acc symbol cnt lst =
            match lst with
            | [] -> acc ^ set_symbol_cnt cnt
            | first :: rest ->
                if acc = "" then loop first#symbol first#symbol 1 rest
                else if symbol = first#symbol then
                  loop acc symbol (cnt + 1) rest
                else
                  loop
                    (acc ^ set_symbol_cnt cnt ^ first#symbol)
                    first#symbol 1 rest
          in
          loop "" "" 0 lst
      in

      let lst_c = List.filter (fun atom -> atom#symbol = "C") input_atoms in
      if List.length lst_c = 0 then
        get_formula (List.sort atoms_sort_compare atoms)
      else
        let lst_h = List.filter (fun atom -> atom#symbol = "H") input_atoms in
        if List.length lst_h = 0 then
          get_formula lst_c
          ^ get_formula
              (List.sort atoms_sort_compare
                 (List.filter (fun atom -> atom#symbol <> "C") input_atoms))
        else
          get_formula lst_c ^ get_formula lst_h
          ^ get_formula
              (List.filter
                 (fun atom -> atom#symbol <> "H" && atom#symbol <> "C")
                 input_atoms)


    method virtual to_string : string

    (* method virtual equals : molecule -> bool *)
    method equals (m : molecule) : bool =
      self#name = m#name && self#formula = m#formula
  end

class water =
  object (self)
    inherit
      molecule "water" [ new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen ]

    method to_string =
      "It is a compound of hydrogen and oxygen, represented by the chemical \
       formula H2O."
  end

class carbon_dioxide =
  object (self)
    inherit
      molecule
        "carbon dioxide"
        [ new Atom.carbon; new Atom.oxygen; new Atom.oxygen ]

    method to_string =
      "Carbon dioxide (CO2) is an inorganic compound with the chemical formula \
       CO2. It is also called CO2 because of its chemical formula."
  end

class ethylene =
  object (self)
    inherit
      molecule
        "ethylene"
        [
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.carbon;
          new Atom.carbon;
        ]

    method to_string =
      "It has the molecular formula C2H4 and the structural formula CH2=CH2, \
       and is a hydrocarbon with two carbons connected by a double bond. It is \
       a straightforward alkene. The volume of ethylene used in is larger than \
       that of other organic compounds."
  end

class glucose =
  object (self)
    inherit
      molecule
        "glucose"
        [
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.oxygen;
          new Atom.oxygen;
          new Atom.oxygen;
          new Atom.oxygen;
          new Atom.oxygen;
          new Atom.oxygen;
        ]

    method to_string =
      "Glucose is a simple sugar with the molecular formula C6H12O6. It is \
       also called glucose (grape sugar)."
  end

class benzene =
  object (self)
    inherit
      molecule
        "benzene"
        [
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.carbon;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
          new Atom.hydrogen;
        ]

    method to_string =
      "It is found in crude oil and is one of the basic compounds in \
       petrochemistry. In some fields, it is colloquially called benzol, after \
       the German word Benzol."
  end
