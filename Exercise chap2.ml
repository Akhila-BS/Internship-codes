(*****chapter 2*****)

(*Q1*)
(*What is the type and value of each of the following OCaml expressions?
1. 7 * (1 + 2 + 3)
2. "CS " ^ string_of_int 3110*)

let a=7*(1+2+3);;
let b="cs"^string_of_int 3110;;

(*output:
   1. val a : int = 42
   2. val b : string = "cs3110"*)

(*Q2*)
(*1. Write an expression that multiplies 42 by 10.
2. Write an expression that divides 3.14 by 2.0.
3. Write an expression that computes 4.2 raised to the seventh power*)

let a=42/10;;
let b=3.14/.2.0;;
let c=4.2**7.0;;

(*Output:
   1. val a : int = 4
   2. val b : float = 1.57
   3. val c : float = 23053.933324800004*)

(*Q3*)
(*1. Write an expression that compares 42 to 42 using structural equality.
2. Write an expression that compares "hi" to "hi" using structural equality. What is the result?
3. Write an expression that compares "hi" to "hi" using physical equality. What is the result?*)

let a=42=42;;
let b="hi"="hi";;
let c="hi"=="hi";;

(*Output:
   1. val a : bool = true
   2. val b : bool = true
   3. val c : bool = false *)

(*Q4*)
(*1. Enter assert true;; into utop and see what happens.
2. Enter assert false;; into utop and see what happens.
3. Write an expression that asserts 2110 is not (structurally) equal to 3110*)  

assert true;;
assert false;;
let a =assert (not(2110=3110));;

(*Output:
   1. unit = () 
   2. Assert_failure ("//toplevel//", 1, 0).
   3. val a : unit = () *)

(*Q5*)
(*Write an if expression that evaluates to 42 if 2 is greater than 1 and otherwise evaluates to 7*)

let a=if 2>1 then 42 else 7;;

(*Output:
   val a : int = 42 *) 

(*Q6*)
(*Using the increment function from above as a guide, define a function double that multiplies its input by 2. For example, double 7 would be 14. Test your function by applying it to a few inputs. Turn those test cases into assertions.*)

let double x=x*2 ;;
let x=5 in double x;;
let x=3 in double x;;
let a=assert (double 5=10);;
let b=assert (double 3=6);;

(*Output:
  val double : int -> int = <fun>
    int = 10
    int = 6
    val a : unit = ()
    val b : unit = () *)

(*Q7*)    
(*1. Define a function that computes the cube of a floating-point number. Test your function by applying it to a few inputs.
2. Define a function that computes the sign (1, 0, or -1) of an integer. Use a nested if expression. Test your function by applying it to a few inputs.
3. Define a function that computes the area of a circle given its radius. Test your function with assert.*)

let  cube x= x *. x *. x;;
let x=2.0 in cube x;;
let x=3.0 in cube x;;
let a = assert (cube 1.42 = 2.59);;
let b = assert (cube 2.0=8.0);;



let  sign x = 
if  x>0 then
  if x=0 then 0 
  else  1 
  else  -1;;
    let x= 6 in(sign x);;
    let x= 8 in(sign x);;
    let x= -6 in(sign x);;
    let a= assert (sign 0 = 0) ;; 
    let b= assert (sign 7 = 1);;
(*OR*)
 let sign x =
   if x > 0 then 1
   else if x <1 then -1
   else 0;; 
let x=6 in(sign x);;
let x= -6 in(sign x);; 
let a= assert (sign 0 = 0) ;; 
let b= assert (sign 7 = 1);;


let area r= Float.pi*.r*.r;;
let r=2.0 in area r;;
let a=assert (area 2.0=12.566370614359172);;


(*Output:
  1. val cube : float -> float = <fun>
     float = 8.
     float = 27. 
   
   2.int = 1
     int = -1
     Assert_failure ("//toplevel//", 1, 7)
     val b : unit = ()
   
   3.float = 12.566370614359172
     val a : unit = ()*)

(*Q8*)
(*Define a function that computes the root mean square of two numbers—i.e.,√(x2+y2)/2. Test your function with assert.*)

let rms x y=sqrt((x*.x +. y*.y)/.2.0);;
let a= rms 1.0 2.0;;
let a=assert (rms 1.0 2.0=1.5811388300841898);;

(*Output:
  val rms : float -> float -> float = <fun> 
  val a : float = 1.5811388300841898
  val a : unit = ()*)

(*Q9*)
(*Define a function that takes an integer d and string m as input and returns true just when d and m form a valid date*)

let date m d=
 if m="Jan" || m="Mar" || m="May" || m="July" || m="Aug" || m="Oct" || m="Dec"
then 1 <= d && d <= 31
else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov"
then 1 <= d && d <= 30
else if m = "Feb"
then 1 <= d && d <= 28
else  false;;

let a= date "Jan" 17;;
let a=assert (date "Feb" 2);;

(*Output:
   val a : bool = true
   val a : unit = ()*)

(*Q10*)
(*Define a recursive function fib : int -> int, such that fib n is the nth number in the Fibonacci sequence, which is 1, 1, 2, 3, 5, 8, 13, … *)   

let rec fib n =
if n = 0 then 0
else if n = 1 then 1
else fib (n-1) + fib (n-2);;

let a= fib 7;;
let b= fib 6;;

(*Output:
  val a : int = 13
  val b : int = 8 *)

(*Q11*)
(*1. Create a function fib_fast that requires only a linear amount of work.
  2. What is the first value of n for which fib_fast n is negative, indicating that integer overflow occurred?*)

let rec h n pp p =
   if n = 1 then p
   else h (n-1) p (pp+p)
 let fib_fast n = h n 0 1;;

 let a= h 6 0 1;;
 let b=h 67 0 1;;

 let rec first_negative f n0 =
   let res = f n0 in
     if res < 0 then n0
     else first_negative f (n0+1);;

 let a=first_negative fib_fast 1;;
 let b=first_negative fib_fast 91;;
 let c=first_negative fib_fast 94;;
 

(*Output:
 1.val a : int = 8
   val b : int = 44945570212853
 2.val a : int = 91
   val b : int = 91
   val c : int = 96*)

(*Q12*)
(*What is the type of each of the functions below? 
1. let f x = if x then x else x
2. let g x y = if y then x else x
3. let h x y z = if x then y else z
4. let i x y z = if x then y else y*)

let f x = if x then x else x;;
let g x y = if y then x else x;;
let h x y z = if x then y else z;;
let i x y z = if x then y else y;;

(*Output:
   1.val f : bool -> bool = <fun>
   2.val g : 'a -> bool -> 'a = <fun>
   3.val h : bool -> 'a -> 'a -> 'a = <fun>
   4.val i : bool -> 'a -> 'b -> 'a = <fun> *)

(*Q13*)
(*Write a function divide : numerator:float -> denominator:float -> float. Apply your function*)

let divide ~numerator:x ~denominator:y = x /. y;;

(*Output: 
 val divide : numerator:float -> denominator:float -> float = <fun>  *)

 (*Q14*)
 (*Suppose that we have defined let add x y = x + y.Which of the following produces an integer, which produces a function, and which produces an error? Decide on an answer, then check your answer in the toplevel.*)

let add x y=x +y;;
let a= add 5 1;;
let b=add 5;;
let c=(add 5) 1;;
let d=add (5 1);;

(*Output:
 1. val a : int = 6
 2. val b : int -> int = <fun>
 3. val c : int = 6  
 4. error*)

 (*Q15*)
 (*Define an infix operator +/. to compute the average of two floating-point numbers*)

 let (+.) x y= (x +. y)/. 2.0;;
 let a=(+.) 1.0 2.0 ;;
 let b=(+.) 0. 0.;;

 (*Output:
   val a : float = 0.75
   val b : float = 0. *)

(*Q16*)
(*Type the following in utop:
1. print_endline "Hello world!";;
2. print_string "Hello world!";;*)

print_endline "Hello world!";;
print_string "Hello world!";;

(*Output:
  Hello world!
  Hello world!- : unit = () *)



