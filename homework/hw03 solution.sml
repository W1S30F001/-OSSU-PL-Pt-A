(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = 
           Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = 
            Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals los = List.filter (fn s => Char.isUpper(String.sub(s, 0))) los

fun longest_string1 los = foldl (fn(s, acc) => if String.size(s) > String.size(acc) then s else acc) "" los

fun longest_string2 los = foldl (fn(s, acc) => if String.size(s) >= String.size(acc) then s else acc) "" los

fun longest_string_helper function los = foldl (fn(s,acc) => if function(String.size(s), String.size(acc)) then s else acc) "" los

val longest_string3 = longest_string_helper (fn(s, acc) => s > acc)

val longest_string4 = longest_string_helper (fn(s, acc) => s >= acc)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer function list = case list of
                                    [] => raise NoAnswer
                               | x::xs => let val try = function x in case try of
                                            NONE => first_answer function xs
                                        |   SOME try => try end

fun all_answers function list =
    let fun acc_helper function acc list = 
            case list of 
                [] => SOME acc
           | x::xs => let val try = function x in case try of
                        NONE => NONE
                 |  SOME try => acc_helper function (acc @ try) xs end
    in acc_helper function [] list end


fun count_wildcards p = g (fn _ => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn _ => 1) (fn x => String.size x) p

fun count_some_var (s,p) = g (fn _ => 0) (fn x => if s = x then 1 else 0) p


fun check_pat p = let fun collect_strings p acc=
                        case p of
                            Variable x => acc @ [x]
                       |    TupleP ps  => List.foldr (fn(p,acc) => collect_strings p [] @ acc) [] ps
                       |    ConstructorP(x,p)=> acc @ [x] @ collect_strings p []
                       |    _          => acc
                      fun no_duplicates los acc =  case los of
                                                [] => acc
                                            |x::xs =>  if (List.exists (fn suspect => x = suspect) xs) then false else no_duplicates xs acc
in no_duplicates (collect_strings p []) true end


fun match (valu,pattern) = case (valu,pattern) of
                (_, Wildcard)         => SOME []
          |     (value,Variable s)    => SOME [(s,value)]
          |     (Unit,UnitP)          => SOME []
          |     (Const i1, ConstP i2) => if i1 = i2 then SOME [] else NONE
          |     (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps) (* andalso isSome(match(List.last(vs), List.last(ps))) *) then all_answers match (ListPair.zip(vs,ps)) else NONE
          |     (Constructor(s2,v), ConstructorP(s1, p)) => if s1 = s2 (* andalso isSome(match(v,p)) *) then match(v,p) else NONE
          |     _ => NONE

fun first_match valu lop =
        SOME (first_answer (fn x => match(valu, x)) lop)
        handle NoAnswer => NONE

