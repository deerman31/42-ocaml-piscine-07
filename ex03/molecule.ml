class virtual molecule (input_name : string) (input_atoms : Atom.atom list) =
  object (self)
    val atoms = input_atoms
    method name = input_name

    method formula : string =
      let generate_formula lst =
        let set_symbol_cnt cnt = if cnt = 1 then "" else string_of_int cnt in
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

      let atoms_sort_compare a1 a2 =
        let symbol1 = a1#symbol in
        let symbol2 = a2#symbol in
        compare (String.get symbol1 0) (String.get symbol2 0)
      in

      let filter_atoms atoms filter_symbols =
        List.filter (fun a -> not (List.mem a#symbol filter_symbols)) atoms
      in

      let c_lst = List.filter (fun x -> x#symbol = "C") input_atoms in
      let h_lst = List.filter (fun x -> x#symbol = "H") input_atoms in
      match (c_lst, h_lst) with
      | [], [] -> generate_formula (List.sort atoms_sort_compare input_atoms)
      | [], h -> generate_formula (List.sort atoms_sort_compare input_atoms)
      | c, [] ->
          generate_formula c
          ^ generate_formula
              (filter_atoms input_atoms [ "C" ] |> List.sort atoms_sort_compare)
      | c, h ->
          generate_formula c ^ generate_formula h
          ^ generate_formula
              (filter_atoms input_atoms [ "C"; "H" ]
              |> List.sort atoms_sort_compare)

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

class o2 =
  object (self)
    inherit molecule "molecular oxygen" [ new Atom.oxygen; new Atom.oxygen ]

    method to_string =
      "Oxygen molecules (O₂) are diatomic molecules consisting of two oxygen \
       atoms covalently bonded together. In Earth's atmosphere, this form of \
       oxygen is essential for life, being crucial for cellular respiration in \
       most organisms. The bond between the atoms is a double covalent bond, \
       making the molecule relatively stable at room temperature. Molecular \
       oxygen (O₂) is a colorless, odorless gas at room temperature and makes \
       up about 21% of Earth's atmosphere. In chemical reactions, particularly \
       in combustion, O₂ serves as a key oxidizing agent, readily reacting \
       with other substances to form new compounds."
  end
