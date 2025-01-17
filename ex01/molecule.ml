class virtual molecule =
  object (self)
    val virtual atoms : Atom.atom list
    method virtual name : string
    method virtual formula : string
    method virtual to_string : string
    method virtual equals : molecule -> bool
  end
