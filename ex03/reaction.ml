type molecule_list = (Molecule.molecule * int) list

class virtual reaction =
  object
    method virtual get_start : molecule_list
    method virtual get_result : molecule_list
    method virtual balance : reaction
    method virtual is_balanced : bool
  end
