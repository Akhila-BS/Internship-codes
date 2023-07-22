(*EXERCISE 8*)

(* exercise: hashtbl usage*)

 let ( -- ) i j =
 let rec from i j l =
   if i > j then l
   else from i (j - 1) (j :: l)
 in
 from i j []

let tab = Hashtbl.create 16

let ints = List.map (fun x -> (x, string_of_int x)) (1 -- 31)

let () = List.iter (fun (k, v) -> Hashtbl.add tab k v) ints

let () = assert (Hashtbl.find tab 1 = "1")
let () = assert ((try Hashtbl.find tab 0 with Not_found -> "") = "")


(*exercise: hashtbl bindings*)

let bindings h =
 Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []


(* exercise: hashtbl stats*)


let buckets h =
 (Hashtbl.stats h).num_buckets

let () = assert (buckets tab = 16)

let single_binding h =
 (Hashtbl.stats h).bucket_histogram.(1)

let () = assert (single_binding tab = 3)

(* exercise: hashtbl load factor*)

let (/..) x y =
 float_of_int x /. float_of_int y


let load_factor h =
 let stats = Hashtbl.stats h in
 stats.num_bindings /.. stats.num_buckets


(*exercise: hashtbl load factor*)

let epsilon = 0.1

let close_to x y =
 abs_float (x -. y) <= epsilon



let () = Hashtbl.add tab 0 "0"; assert (not (close_to (load_factor tab) 1.0))
let () = Hashtbl.add tab ~-1 "-1"; assert (close_to (load_factor tab) 1.0)

(* exercise: functorial interface*)

module CaseInsensitiveHashtbl =
 Hashtbl.Make (struct
   type t = string

   let equal s1 s2 =
     String.lowercase_ascii s1 = String.lowercase_ascii s2

   let hash s =
     Hashtbl.hash (String.lowercase_ascii s)
 end)




(*exercise: bad hash*)

module BadHashtbl =
 Hashtbl.Make (struct
   type t = int
   let equal = (=)
   let hash _ = 0
 end)

let bad = BadHashtbl.create 16

let () =
 1--100
 |> List.map (fun x -> x, string_of_int x)
 |> List.iter (fun (k,v) -> BadHashtbl.add bad k v)


let () = assert ((BadHashtbl.stats bad).bucket_histogram.(100) = 1)






(* exercise: functorized BST*)

module type Set = sig
 
 type elt

 
 type t

 val empty    : t

 val insert   : elt -> t -> t

 val mem      : elt -> t -> bool
 val of_list  : elt list -> t
 val elements : t -> elt list
end

module type Ordered = sig
 type t
 val compare : t -> t -> int
end

module BstSet (Ord : Ordered) : Set = struct

 type elt = Ord.t

 type t = Leaf | Node of t * elt * t

 let empty = Leaf

 let rec mem x = function
   | Leaf -> false
   | Node (l, v, r) ->
     begin
       match compare x v with
       | ord when ord < 0 -> mem x l
       | ord when ord > 0 -> mem x r
       | _                -> true
     end

 let rec insert x = function
   | Leaf -> Node (Leaf, x, Leaf)
   | Node (l, v, r) ->
     begin
       match compare x v with
       | ord when ord < 0 -> Node(insert x l, v, r         )
       | ord when ord > 0 -> Node(l,          v, insert x r)
       | _                -> Node(l,          x, r         )
     end

 let of_list lst =
   List.fold_left (fun s x -> insert x s) empty lst

 let rec elements = function
   | Leaf -> []
   | Node (l, v, r) -> (elements l) @ [v] @ (elements r)
end


(* exercise: efficient traversals*)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec preorder t =
 let rec go acc = function
   | Leaf -> acc
   | Node (l,v,r) -> go (go (v :: acc) l) r
 in
 List.rev (go [] t)

let rec inorder t =
 let rec go acc = function
   | Leaf -> acc
   | Node (l,v,r) -> go (v :: go acc l) r
 in
 List.rev (go [] t)

let rec postorder t =
 let rec go acc = function
   | Leaf -> acc
   | Node (l,v,r) -> v :: go (go acc l) r
 in
 List.rev (go [] t)

let t =
 Node(Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
      4,
      Node(Node(Leaf, 5, Leaf), 6, Node(Leaf, 7, Leaf)))



let () = assert (preorder t  = [4;2;1;3;6;5;7])
let () = assert (inorder t   = [1;2;3;4;5;6;7])
let () = assert (postorder t = [1;3;2;5;7;6;4])




(*exercise: RB draw insert*)
type 'a sequence =
    Cons of 'a * (unit -> 'a sequence)

let rec from n =
  Cons (n, fun () -> from (n+1))

let nats = from 0

let hd (Cons (h, _)) = h
let tl (Cons (_, tf)) = tf ()

let rec take n s =
  if n=0 then []
  else hd s :: take (n-1) (tl s)
let rec drop n s =
  if n = 0 then s
  else drop (n-1) (tl s)

(* exercise: pow2*)

let rec pow2from n =
  Cons (n, fun () -> pow2from (2 * n))

let pow2 = pow2from 1

(*exercise: nth*)

let rec nth (Cons (h, tf)) n =
  if n=0 then
    h
  else
    nth (tf ()) (n - 1)


(*exercise: filter*)

let rec filter p (Cons (h, tf)) =
  if p h then
    Cons (h, fun () -> filter p (tf ()))
  else
    filter p (tf ())


(*exercise: interleave*)

let rec interleave (Cons (h1, tf1)) (Cons (h2, tf2)) =
  Cons (h1, fun () ->
      Cons (h2, fun () ->
          interleave (tf1 ()) (tf2 ())))


let rec interleave_more_lazy (Cons (h1, tf1)) s2 =
  Cons (h1, fun () ->
      interleave_more_lazy s2 (tf1 ()))

(*exercise: sift*)
let rec sift n =
  filter (fun x -> x mod n <> 0)

(*exercise: primes*)

let rec sieve (Cons (h, tf)) =
  Cons (h, fun () -> tf () |> sift h |> sieve)

let primes =
  sieve (from 2)


(*exercise: approximately e*)


let rec fact (n: int) : int =
  if (n = 0 || n = 1) then 1
  else n * (fact (n-1))

let kth_term (x: float) (k: int) : float =
  (x ** (float_of_int k)) /. (float_of_int (fact k))


let rec e_terms_from (x: float) (k: int): float sequence =
  Cons(kth_term x k, fun () ->
      e_terms_from x (k+1))

let e_terms (x: float) : float sequence = e_terms_from x 0


let rec running_total (s: float sequence) (r: float) : float sequence =
  match s with
  | Cons(h, tf) -> Cons (h +. r, fun () -> running_total (tf ()) (h +. r))

let total (s: float sequence): float sequence = running_total s 0.0

let rec within_helper (eps: float) (prev: float) (s: float sequence): float =
  match s with
  | Cons(h, tf) -> (
      if (abs_float (prev -. h)) < eps then h
      else within_helper eps h (tf ())
    )


let within (eps: float) (s: float sequence) : float =
  within_helper eps max_float s


let e (x: float) (eps: float) : float =
  e_terms x |> total |> within eps

(*exercise: different sequence rep*)

module DifferentsequenceRep = struct
  type 'a sequence = Cons of (unit -> 'a * 'a sequence)

  let hd (Cons th) =
    th () |> fst

  let tl (Cons th) =
    th () |> snd

  let rec from n =
    Cons (fun () -> (n, from (n + 1)))

  let rec nats =
    from 0

  let rec map f (Cons th) =
    Cons begin fun () ->
      let h, t = th () in
      (f h, map f t)
    end
end


(*exercise: lazy hello*)

let hello =
  lazy (print_endline "Hello lazy world")


(*exercise: lazy and*)


let (&&&) b1 b2 =
  if Lazy.force b1 then
    Lazy.force b2
  else
    false

(*exercise: lazy sequence*)

module Lazysequence = struct
  type 'a lazysequence =
    | Cons of 'a * 'a lazysequence Lazy.t

  let rec map f (Cons (h, t)) =
    Cons (f h, lazy (map f (Lazy.force t)))

  let rec filter p (Cons (h, t)) =
    let tl = lazy (filter p (Lazy.force t)) in
    if p h then
      Cons (h, tl)
    else
      Lazy.force tl
end

(*exercise: lazy list*)

module type LazyList = sig
  type 'a lazylist

  val hd : 'a lazylist -> 'a

  val tl : 'a lazylist -> 'a lazylist

  val take : int -> 'a lazylist -> 'a list

  val from : int -> int lazylist

  val map : ('a -> 'b) -> 'a lazylist -> 'b lazylist

  val filter : ('a -> bool) -> 'a lazylist -> 'a lazylist
end


module LazyListImpl : LazyList = struct
  type 'a lazylist = Cons of 'a * 'a lazylist Lazy.t

  let hd (Cons (h, _)) =
    h

  let tl (Cons (_, t)) =
    Lazy.force t

  let rec take n (Cons (h, t)) =
    if n = 0 then
      []
    else
      h :: take (n-1) (Lazy.force t)

  let rec from n =
    Cons (n, lazy (from (n+1)))

  let rec map f (Cons (h, t)) =
    Cons (f h, lazy (map f (Lazy.force t)))

  let rec filter p (Cons (h, t)) =
    if p h then
      Cons (h, lazy (filter p (Lazy.force t)))
    else
      filter p (Lazy.force t)
end

(*exercise: promise and resolve*)

module type Promise = sig

  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a promise
  type 'a resolver

  val make : unit -> 'a promise * 'a resolver

  val return : 'a -> 'a promise

  val state : 'a pro
  val resolve : 'a resolver -> 'a -> unit

  val reject : 'a resolver -> exn -> unit

  val (>>=) : 'a promise -> ('a -> 'b promise) -> 'b promise
end

module Promise : Promise = struct
  type 'a state = Pending | Resolved of 'a | Rejected of exn

  type 'a promise = {
    mutable state : 'a state;
    mutable callbacks : ('a -> unit) list
  }

  type 'a resolver = 'a promise
  let write_once p s =
    if p.state = Pending
    then p.state <- s
    else invalid_arg "cannot write twice"

  let make () =
    let p = {state = Pending; callbacks = []} in
    p, p

  let return x =
    {state = Resolved x; callbacks = []}

  let state p = p.state

  let reject r x =
    write_once r (Rejected x);
    r.callbacks <- []

  let run_callbacks callbacks x =
    List.iter (fun f -> f x) callbacks

  let resolve r x =
    write_once r (Resolved x);
    let callbacks = r.callbacks in
    r.callbacks <- [];
    run_callbacks callbacks x

  let (>>=) (p : 'a promise) (c : 'a -> 'b promise) : 'b promise =
    match p.state with
    | Resolved x -> c x
    | Rejected x -> {state = Rejected x; callbacks = []}
    | Pending ->
      let bind_promise, bind_resolver = make () in
      let f x : unit =
        let callback_promise = c x in
        match callback_promise.state with
        | Resolved x -> resolve bind_resolver x
        | Rejected x -> reject bind_resolver x
        | Pending -> failwith "impossible"
      in
      p.callbacks <- f :: p.callbacks;
      bind_promise
end

let _ =
  let open Promise in
  let p, r = make () in
  let _ = p >>= (fun i -> Printf.printf "%i\n" i; return ()) in
  resolve r 42

module LwtExercises = struct

  open Lwt.Infix  
  (*exercise: promise and resolve lwt*)


  let _ =
    let p, r = Lwt.wait () in
    let _ = p >>= (fun i -> Lwt_io.printf "%i\n" i) in
    Lwt.wakeup r 42

  (*exercise: timing challenge 1*)

  let delay (sec : float) : unit Lwt.t =
    Lwt_unix.sleep sec

  let delay_then_print () =
    delay 3. >>= fun () ->
    Lwt_io.printl "done"

  (* exercise: timing challenge 2*)

  let timing2 () =
    let _t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
    let _t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
    let _t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
    Lwt_io.printl "all done"


  (*exercise: timing challenge 3*)

  let timing3 () =
    delay 1. >>= fun () ->
    Lwt_io.printl "1" >>= fun () ->
    delay 10. >>= fun () ->
    Lwt_io.printl "2" >>= fun () ->
    delay 20. >>= fun () ->
    Lwt_io.printl "3" >>= fun () ->
    Lwt_io.printl "all done"


  (*exercise: timing challenge 4*)

  let timing4 () =
    let t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
    let t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
    let t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
    Lwt.join [t1; t2; t3] >>= fun () ->
    Lwt_io.printl "all done"

 

  (* exercise: file monitor*)

  open Lwt_io
  open Lwt_unix

  let log () : input_channel Lwt.t =
    openfile "log" [O_RDONLY] 0 >>= fun fd ->
    Lwt.return (of_fd input fd)

  let rec loop (ic : input_channel) =
    read_line ic >>= fun str ->
    printlf "%s" str >>= fun () ->
    loop ic

  let monitor () : unit Lwt.t =
    log () >>= loop

  let handler : exn -> unit Lwt.t = function
    | End_of_file -> Lwt.return ()
    | exc -> Lwt.fail exc

  let main () : unit Lwt.t =
    Lwt.catch monitor handler


end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad =
struct
  type 'a t = 'a option

  let return x = Some x

  let (>>=) m f =
    match m with
    | Some x -> f x
    | None -> None

end

open Maybe

(*exercise: add opt*)

let add (x : int t) (y : int t) : int t =
  x >>= fun a ->
  y >>= fun b ->
  return (a + b)

(* exercise: fmap and join*)

module type ExtMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module Maybe : ExtMonad =
struct
  type 'a t = 'a option

  let return x = Some x

  let (>>=) m f =
    match m with
    | Some x -> f x
    | None -> None

  let (>>|) m f =
    match m with
    | Some x -> return (f x)
    | None -> None

  let join = function
    | Some m -> m
    | None -> None

end

(*exercise: fmap and join again*)

module Maybe : ExtMonad =
struct
  type 'a t = 'a option

  let return x = Some x

  let (>>=) m f =
    match m with
    | Some x -> f x
    | None -> None

  let (>>|) x f =
    x >>= fun a ->
    return (f a)

  let join x =
    x >>= fun y ->
    y

end

(*exercise: bind from fmap+join*)

module type FmapJoinMonad = sig
  type 'a t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val return : 'a -> 'a t
end

module type BindMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
  include M
  let (>>=) m f =
    m >>| f |> join
end

(*exercise: list monad*)

module ListMonad : ExtMonad = struct
  type 'a t = 'a list

  let return x =
    [x]

  let join =
    List.flatten

  let (>>|) m f =
    List.map f m

  let (>>=) m f =
    m |> List.map f |> join
    (* or, m >>| f |> join *)
end

(*exercise: trivial monad laws*)

module Trivial : Monad = struct
  type 'a t = Wrap of 'a
  let return x = Wrap x
  let (>>=) (Wrap x) f = f x
end

