type molecule_list = (Molecule.molecule * int) list

class virtual reaction =
  object
    method virtual get_start : molecule_list
    method virtual get_result : molecule_list
    method virtual balance : reaction
    method virtual is_balanced : bool
  end

class alkane_combustion (input_alkanes : Alkane.alkane list) =
  object (self)
    inherit reaction
    val mutable alkane_nums : int list = []
    val mutable o2_num = 0
    val mutable co2_num = 0
    val mutable h2o_num = 0
    val mutable flag_balanced = false (* 変数名のtypo修正 *)

    method get_start =
      let get_alkanes lst =
        let rec loop acc acc_n lst =
          match lst with
          | [] -> acc
          | first :: rest ->
              loop ((first, List.nth alkane_nums acc_n) :: acc) (acc_n + 1) rest
        in
        loop [] 0 input_alkanes
      in

      if not flag_balanced then raise (Failure "Reaction is not balanced")
      else List.rev ((new Molecule.o2, o2_num) :: get_alkanes input_alkanes)

    method get_result =
      if not flag_balanced then raise (Failure "Reaction is not balanced")
      else
        [
          (new Molecule.carbon_dioxide, co2_num); (new Molecule.water, h2o_num);
        ]

    method balance =
      let get_alkane_c_h_nums alkane =
        let split_string s =
          List.filter
            (fun x -> x <> "")
            (s |> String.split_on_char 'C'
            |> List.map (String.split_on_char 'H')
            |> List.flatten)
        in

        let c_o_count lst symbol =
          if List.length lst = 1 then
            if symbol = "C" then 1. else float_of_string (List.hd lst) /. 2.
          else if symbol = "C" then float_of_string (List.hd lst)
          else float_of_string (List.hd (List.rev lst)) /. 2.
        in

        let alkane_lst =
          List.filter (fun x -> x <> "") (split_string alkane#formula)
        in
        (* let co2_count = float_of_string (List.hd alkane_lst) in *)
        let co2_count = c_o_count alkane_lst "C" in
        (* print_endline alkane#formula;
        List.iter
          (fun x -> Printf.printf "[%s]\n" x)
          (split_string alkane#formula); *)
        (* print_endline ("CO2: " ^ string_of_float co2_count); *)
        (* let h2o_count = float_of_string (List.hd (List.rev alkane_lst)) in *)
        let h2o_count = c_o_count alkane_lst "H" in
        (* print_endline ("H2O: " ^ string_of_float h2o_count); *)
        let o2_count = ((co2_count *. 2.) +. (h2o_count)) /. 2. in
        (* print_endline ("O2: " ^ string_of_float o2_count); *)
        (co2_count, h2o_count, o2_count)
      in

      let stoichiometric_coefficients_balance counts =
        match counts with
        | co2, h2o, o2 ->
            if Float.rem o2 1.0 <> 0. then (
              alkane_nums <- 2 :: alkane_nums;
              o2_num <- o2_num + int_of_float (o2 *. 2.);
              co2_num <- co2_num + int_of_float (co2 *. 2.);
              h2o_num <- h2o_num + int_of_float (h2o *. 2.))
            else (
              alkane_nums <- 1 :: alkane_nums;
              o2_num <- o2_num + int_of_float o2;
              co2_num <- co2_num + int_of_float co2;
              h2o_num <- h2o_num + int_of_float h2o)
      in
      List.iter stoichiometric_coefficients_balance
        (List.map get_alkane_c_h_nums input_alkanes);
      flag_balanced <- true;
      alkane_nums <- List.rev alkane_nums;
      (self :> reaction)

    method is_balanced = flag_balanced
  end
