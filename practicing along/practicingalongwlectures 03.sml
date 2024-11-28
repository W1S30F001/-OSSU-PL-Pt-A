val triple = fn y => y + 1

fun f y =
    let
        val x = y + 1
    in
        fn z => x + y + z
    end
(* val g = f 4
val z = g 6 *)


fun f g = let val x = 9 in g end
val x = 7
fun h() = x+1
val y = f h

(* Combining Functions*)
fun backup1(f,g) = fn x => case f x of
                             NONE => g x
                          |  SOME y => y

fun backup2(f,g) = fn x => f x handle _ => g x

val test = fn x => fn y => (x,y)

fun f x y = (x,y)

fun curry f x y = f(x,y)

datatype BST_tree =       Null
                        | R_Node of int * BST_tree
                        | L_Node of int * BST_tree
fun fold_tree_acc f acc tree = case tree of
                            Null => acc
                         | R_Node(x,xs) => fold_tree_acc f (f(x, acc)) xs
                         | L_Node(x,xs) => fold_tree_acc f (f(x, acc)) xs

fun fold_tree_base f base tree = case tree of
                          Null => base
                        | R_Node(node, rest_tree) => f(node, fold_tree_base f base rest_tree)
                        | L_Node(node, rest_tree) => f(node, fold_tree_base f base rest_tree)
                        

val tree1 = R_Node(1, L_Node(2, Null))

fun count_tree_acc BST_tree = fold_tree_acc (fn (x, y) => x + y) 0 BST_tree
fun count_tree_base BST_tree = fold_tree_base (fn (x, y) => x + y) 0 BST_tree

fun fold_list_base f base list = case list of  
                     [] => base
            |     x::xs => f(x, fold_list_base f base xs)

fun foldl_list_acc f acc list = case list of  
                     [] => acc
            |     x::xs => foldl_list_acc f (f x acc)  xs

val boolean_list = [true,false,true,false,false]
val boolean_list_last_false = [true,true, true, true, false]
val boolean_list_all_true = [true,true, true, true, true]


val all_true = fn list => fold_list_base (fn(x,y) => x andalso y) true list
val all_true_acc = fold_list_base (fn(x,y) => x andalso y) true

fun andmap f list = case list of
                                [] => true
                            |   x::xs => f x andalso andmap f xs
val all_positive = andmap (fn x => x >= 0)

fun fold_andmap f base list = case list of
                                [] => base
                            |   x::xs => f(x,fold_andmap f base xs)

val all_positive_fold = fold_andmap (fn(x,y) => x > 0 andalso y) true

fun generic_fold_andmap g base list = fold_andmap (fn(x,y) => g x y) base
val raising_powers = fold_andmap (fn(x,y) => 2 * (x + y)) 0
(*-------------------------------------*)
(* COMBINING FUNCTIONS *)
fun compose(f, g) = fn x => f(g x)
val test_funcomposition = compose(Math.sqrt, (fn x => Real.fromInt(abs x)))
val test_o_composition = Math.sqrt o Real.fromInt o abs


val x = ref 42
val y = ref 42
val z = x
val _ = z := 41
val sum_mutables = !x + !y

(*-------------------------------------------*)
(*Callbacks*)
val cbs: (int -> unit) list ref = ref []

fun onKeyEvent f = cbs := f::(!cbs)

fun onEvent i =
    let fun loop fs =
        case fs of
            [] => ()
        |   f::fs => (f i; loop fs)
    in loop (!cbs) end