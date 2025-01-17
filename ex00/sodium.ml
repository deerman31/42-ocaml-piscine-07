
(* 11	Na	ナトリウム	Sodium *)
class sodium =
  object (self)
    inherit Atom.atom
    method name = "sodium"
    method symbol = "Na"
    method atomic_number = 11
  end