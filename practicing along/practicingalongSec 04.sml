fun mapper xs = map (fn x => (x, 1)) xs

signature TEST = sig
    val testing : int
    val error : exn
    datatype dt = T of string | NONE
end;

structure TestingSignature :> TEST = struct
    exception error
    datatype dt = T of string | NONE
    val testing = 5
end;





signature generic = sig
    type t
end;

structure DataType = struct
    datatype t = SKRRT of int | T of int * t
end;


(* Standard Equivalences*)

fun f x = x + x
fun h () = (print "hi " ; f)
fun g y = (h ()) y

fun f2 x = x + x
fun h2 () = (print "hi " ; f2)
val g2 = (h())