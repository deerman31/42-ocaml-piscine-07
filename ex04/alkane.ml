class virtual alkane (input_name : string) (input_n : int) =
  let () =
    if input_n <= 0 then
      raise (Invalid_argument "Number of carbon atoms must be positive")
  in
  object (self)
    method name = input_name

    method formula : string =
      if input_n = 1 then "CH4"
      else "C" ^ string_of_int input_n ^ "H" ^ string_of_int ((2 * input_n) + 2)

    method virtual to_string : string

    method equals (a : alkane) : bool =
      self#name = a#name && self#formula = a#formula
  end

class methane =
  object (self)
    inherit alkane "methane" 1

    method to_string =
      "A colorless, transparent, odorless gas (at room temperature). It is the \
       main component of natural gas and is used for city gas, etc."
  end

class ethane =
  object (self)
    inherit alkane "ethane" 2

    method to_string =
      "It is an organic compound with two carbon atoms that belongs to the \
       alkane group. Its molecular formula is C2H6 and structural formula is \
       CH3-CH3. It is the second simplest alkane after methane and has no \
       isomers. It is poorly soluble in water and easily soluble in organic \
       solvents."
  end

class octane =
  object (self)
    inherit alkane "octane" 8

    method to_string =
      "Octane is a saturated hydrocarbon with eight carbon atoms. It is found \
       in petroleum (and its fractional distillate, gasoline). There are 18 \
       structural isomers, and 24 if stereoisomers are taken into account."
  end
