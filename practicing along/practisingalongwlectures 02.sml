val practising_record = {id = ("low", 1), ego = ("nonexistent", 0), superego = "high"};

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f x=
    case x of
        Pizza => 3
        | Str s => 8
        | TwoInts(i1,i2) => i1 + i2

exception ListMismatch

val triple_to_unzip = [(1,2,3),(4,5,6),(7,8,9)]

val triple_to_zip = ([1,4,7],[2,5,8],[3,6,9])

fun zip triple =
    case triple of
        ([],[],[]) => []
    |   (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip(tl1,tl2,tl3)
    | _ => raise ListMismatch


fun unzip triple =
    case triple of
        [] => ([],[],[])
    |   (a,b,c)::tl_list => let val (l1,l2,l3) = unzip tl_list 
                            in
                            (a::l1,b::l2,c::l3)
                            end


val nondecreasing_lst = [1,2,3,4,5,6,7,8,9]
val notnondecreasing_lst = [1,2,5,5,2]

(* int lst -> boolean *)
(* return true if elements in the list are nondecreasing from left (beginning) to right (ending)*)
(* fun is_nondecreasing lst = false *)

fun is_nondecreasing lst =
    case lst of
           [] => true
    |   _::[] => true
    | head::neck::rest => head <= neck andalso is_nondecreasing(neck::rest)
    
val test_nondecreasing1 = is_nondecreasing nondecreasing_lst
val test_nondecreasing2 = is_nondecreasing notnondecreasing_lst


datatype sign = P | N | Z

(* sign * sign -> sign *)
(* return the sign (charge) of the number resulting from multiplication of the two input numbers *)

fun multsign(x1,x2) = 
    let fun get_sign x = if x=0 then Z else if x>0 then P else N
    in
        case (get_sign x1, get_sign x2) of
         (_,Z) => Z
    |      (Z,_) => Z
    |      (P,P) => P
    |      (N,N) => P
    |      _ => N
    end

val test_multsign1 = multsign(1,2)
val test_multsign2 = multsign(~1,2)
val test_multsign3 = multsign(0,0)
