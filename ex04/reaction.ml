(* 反応の状態を表す型 *)
type reaction_state = {
  alkane_nums : int list;
  o2_num : int;
  co2_num : int;
  h2o_num : int;
  is_balanced : bool;
}

(* 分子とその数量のペアのリスト *)
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

    (* 初期状態 *)
    val state =
      {
        alkane_nums = [];
        o2_num = 0;
        co2_num = 0;
        h2o_num = 0;
        is_balanced = false;
      }

    (* アルカンのCとHの数を計算する補助関数 *)
    method private get_alkane_c_h_nums alkane =
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
      let co2_count = c_o_count alkane_lst "C" in
      let h2o_count = c_o_count alkane_lst "H" in
      let o2_count = ((co2_count *. 2.) +. h2o_count) /. 2. in
      (co2_count, h2o_count, o2_count)

    (* 係数を計算して新しい状態を返す *)
    method private calculate_coefficients =
      let calculate_single_alkane ((state : reaction_state), (co2, h2o, o2)) =
        if Float.rem o2 1.0 <> 0. then
          {
            alkane_nums = 2 :: state.alkane_nums;
            o2_num = state.o2_num + int_of_float (o2 *. 2.);
            co2_num = state.co2_num + int_of_float (co2 *. 2.);
            h2o_num = state.h2o_num + int_of_float (h2o *. 2.);
            is_balanced = true;
          }
        else
          {
            alkane_nums = 1 :: state.alkane_nums;
            o2_num = state.o2_num + int_of_float o2;
            co2_num = state.co2_num + int_of_float co2;
            h2o_num = state.h2o_num + int_of_float h2o;
            is_balanced = true;
          }
      in

      let counts = List.map self#get_alkane_c_h_nums input_alkanes in
      let init_state = { state with alkane_nums = [] } in
      List.fold_left
        (fun acc count -> calculate_single_alkane (acc, count))
        init_state counts

    (* 入力分子のリストを生成 *)
    method private create_start_list alkane_nums =
      let create_alkane_pairs lst =
        let rec loop acc acc_n = function
          | [] -> acc
          | first :: rest ->
              loop ((first, List.nth alkane_nums acc_n) :: acc) (acc_n + 1) rest
        in
        loop [] 0 lst
      in
      List.rev
        ((new Molecule.o2, state.o2_num) :: create_alkane_pairs input_alkanes)

    (* パブリックメソッド *)
    method get_start =
      if not state.is_balanced then raise (Failure "Reaction is not balanced")
      else self#create_start_list state.alkane_nums

    method get_result =
      if not state.is_balanced then raise (Failure "Reaction is not balanced")
      else
        [
          (new Molecule.carbon_dioxide, state.co2_num);
          (new Molecule.water, state.h2o_num);
        ]

    method balance =
      let new_state = self#calculate_coefficients in

      (*
         1. オブジェクトのクローン構文 {< ... >}:
         オブジェクトの新しいコピーを作成する特別な構文
         インスタンス変数の値を変更した新しいオブジェクトを生成
         元のオブジェクトは変更されない(immutable)
         2. 新しい状態の設定:
         new_state: calculate_coefficientsメソッドで計算された新しい反応状態
         with: レコードの一部フィールドを更新する構文
         alkane_nums = List.rev new_state.alkane_nums: alkane_numsフィールドを逆順にして更新
         3. アップキャスト演算子 :>:
         具象クラスのインスタンスを基底クラス型にキャストする
         この場合、alkane_combustionオブジェクトをreaction型にキャスト
         これにより、balanceメソッドの戻り値型がreactionインターフェースに適合
      *)
      ({<state = { new_state with alkane_nums = List.rev new_state.alkane_nums }>}
        :> reaction)

    method is_balanced = state.is_balanced
  end
