(*EXERCISE 6*)
(* exercise: spec game*)



   module type S = sig
       val num_vowels : string -> int
  
  
    val is_sorted : 'a list -> bool
  
    val sort : 'a list -> 'a list
  
   
    val max : 'a list -> 'a
  
    val is_prime : int -> bool
  end
  
  (** exercise: poly spec*)
  
 
  module type Poly = sig
    
    type t
  
   
    val eval : int -> t -> int
  
    val from_coefficients : int list -> t
  
  
    (* # Querying *)
  
    (* [coefficient i p] returns the coefficient [c_i] of the term with power
     *   [i]. That is, if $p = c_n x^n + ... + c_i x^i + ... + c_0$, then
     *   [coefficient i p = c_i]. If $i > degree p$, then [coefficient i p = 0].
     * requires: i >= 0
    *)
    val coefficient : int -> t -> int
  
    (* [degree p] returns the largest [i] such that [coefficient i p <> 0].
     * The result is [-1] if there is no such [i].
    *)
    val degree : t -> int
  
    val scale : int -> t -> t
  
    val add : t -> t -> t
  
    val multiply : t -> t -> t
  end
  
  
  (* exercise: poly impl**)
  
  module PolyImpl : Poly = struct
   
    type t = int list
  
    let rec eval x = function
      | [] -> 0
      | c :: rest -> c + x * eval x rest
  
  
    let from_coefficients p =
      p
  
  
    let coefficient i p =
      if i < List.length p then List.nth p i else 0
  
    let degree p =
      
      let rec drop_while f = function
        | [] -> []
        | x :: xs -> if f x then drop_while f xs else x :: xs
      in
      List.length (drop_while (fun x -> x = 0) (List.rev p)) - 1
  
  
    let scale c =
      List.map (fun c' -> c * c')
  
    let rec add p1 p2 =
      match p1, p2 with
      | [], _ -> p2 
      | _, [] -> p1 
      | c1 :: p1, c2 :: p2 -> c1 + c2 :: add p1 p2
  
 
    let rec multiply p1 p2 =
     
      let rec tabulate f s = function
        | n when n <= 0 -> []
        | n -> f s :: tabulate f (s + 1) (n - 1)
      in
  
      
      let rec take l n =
        match l, n with
        | _      , 0 -> []
        | []     , n -> 0 :: take [] (n - 1)
        | x :: xs, n -> x :: take xs (n - 1)
      in
  
      
      let rec dotProduct l1 l2 =
        match l1, l2 with
        | [], _ -> 0
        | _, [] -> 0
        | x1 :: l1, x2 :: l2 -> x1 * x2 + dotProduct l1 l2
      in
  
     
      let ith i =
     
        let p1_up_to_ith_power = take p1 (i + 1) in
        let p2_up_to_ith_power = take p2 (i + 1) in
       
        dotProduct p1_up_to_ith_power (List.rev p2_up_to_ith_power)
      in
      tabulate ith 0 (degree p1 + degree p2 + 1)
  end
  
  (*exercise: function maps*)
  
  module FnMap = struct
  
    
    type ('k,'v) t = 'k -> 'v
  
    let empty _ = raise Not_found
  
    let mem k m =
      try
        let _ = m k in true
      with
        Not_found -> false
  
    let find = (|>)
  
    let add k v m =
      fun key -> if key=k then v else m key
  
    let remove k m =
      fun key -> if key=k then raise Not_found else m key
  
    let rep_ok m = m
  
  end
  
  
 
  
  (* exercise: qcheck odd divisor*)
  
  (** [odd_divisor x] is an odd divisor of [x].
      Requires: [x >= 0]. *)
      let odd_divisor x =
        if x < 3 then 1 else
          let rec search y =
            if y >= x then y 
            else if x mod y = 0 then y 
            else search (y + 2) 
          in search 3
  
  let is_odd n = n mod 2 = 1
  let is_divisor_of d n = n mod d = 0
  let t = QCheck.Test.make QCheck.small_int
      (fun i ->
         let d = odd_divisor i
         in is_odd d && is_divisor_of d i)
  
  (* exercise: qcheck avg*)
  
  (** [avg [x1; ...; xn]] is [(x1 + ... + xn) / n].
       Requires: the input list is not empty. *)
  let avg lst =
  let rec loop (s, n) = function
    | [] -> (s, n)
    | [ h ] -> (s + h, n + 1)
    | h1 :: h2 :: t -> if h1 = h2 then loop (s + h1, n + 1) t
      else loop (s + h1 + h2, n + 2) t
  in
  let (s, n) = loop (0, 0) lst
  in float_of_int s /. float_of_int n
  
  let ref_avg lst =
    (float_of_int (List.fold_left (+) 0 lst))
    /. (float_of_int (List.length lst))
  
  let t = QCheck.Test.make
      (QCheck.list_of_size (QCheck.Gen.int_range 1 10) QCheck.small_int)
      (fun lst -> avg lst = ref_avg lst)
  
  let _ = QCheck_runner.run_tests_main [t]

  