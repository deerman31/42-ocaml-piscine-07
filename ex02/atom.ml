class virtual atom (input_name : string) (input_symbol : string)
  (input_number : int) =
  object (self)
    method name = input_name
    method symbol = input_symbol
    method atomic_number = input_number
  end

class hydrogen =
  object (self)
    inherit atom "hydrogen" "H" 1
  end

class carbon =
  object (self)
    inherit atom "carbon" "C" 6
  end

class oxygen =
  object (self)
    inherit atom "oxygen" "O" 8
  end

class fluorine =
  object (self)
    inherit atom "fluorine" "F" 9
  end

class sodium =
  object (self)
    inherit atom "sodium" "Na" 11
  end

class magnesium =
  object (self)
    inherit atom "magnesium" "Mg" 12
  end
