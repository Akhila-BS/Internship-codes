(****HARDCAML CODE FOR HALF ADDER*****)

open HardCaml

module halfadder = struct
let halfadder (x : Sig.t) (y : Sig.t) : Sig.t * Sig.t =
  let open Sig in
  let sum = x ^: y in
  let cout = x &: y in
  sum,cout
end
let () =
  let x = Sig.input "x" 1 in
  let y = Sig.input "y" 1 in
  let sum, cout = halfadder x y in

  let circuit = Circuit.create_exn ~name:"halfadder" [x; y] [sum; cout] in
  let verilog_code = Circuit.to_verilog circuit in

  print_endline verilog_code