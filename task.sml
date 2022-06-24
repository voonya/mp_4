val only_capitals = List.filter (fn str => (Char.isUpper o String.sub) (str, 0))

val longest_string1 =
    List.foldl (fn (str, acc) =>
		   if (String.size str) > (String.size acc)
		   then str
		   else acc) ""


val longest_string2 =
    List.foldl (fn (str, acc) =>
		   if (String.size str) >= (String.size acc)
		   then str
		   else acc) ""

fun longest_string_helper f =
    List.foldl (fn (str, acc) =>
		   if f (String.size str, String.size acc)
		   then str
		   else acc) ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

exception NoAnswer

fun first_answer f lst = 
    case lst of
	[] => raise NoAnswer
      | x :: xs => case f x of
		       SOME v => v
		     | NONE => first_answer f xs

fun all_answers f lst =
    let
	fun all_answers_helper remaining acc = 
	    case (remaining, acc) of
		([], _) => acc
	      | (x :: xs, SOME v) => (case f x of
					  NONE => NONE
					| SOME xv => all_answers_helper xs (SOME (xv @ v)))
	      | _ => NONE
    in
	all_answers_helper lst (SOME [])
    end

datatype pattern = Wildcard | Variable of string | UnitP | ConstP of int | TupleP of pattern list | ConstructorP of string * pattern
datatype valu = Const of int | Unit | Tuple of valu list | Constructor of string * valu


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

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_some_var (str, p) = g (fn _ => 0) (fn x =>
						if String.isSubstring str x
						then 1
						else 0) p

fun check_pat p =
    let
	fun filterString pat acc = case pat of
				       Variable x => x :: acc
				     | ConstructorP (_, p) => filterString p acc
				     | TupleP ps =>
				       List.foldl
					   (fn (p, acc) => (filterString p []) @ acc) [] ps
				     | _ => []
    in
	let
	    val strList = filterString p []
	    fun checkDuplicate remList = 
		case remList of
		    [] => true
		  | x :: xs => if List.exists (fn item => item = x) xs
			       then false
			       else checkDuplicate xs
	in
	    checkDuplicate strList
	end
    end

fun match (v, pat) =
    case (v,pat) of  
      (_,Wildcard) => SOME []
     |(Const v1,ConstP p1) =>if v1 = p1 then SOME [] else NONE
     |(Unit,UnitP) =>SOME []
     |(Constructor (s ,v1),ConstructorP (s1, p1) ) => if s = s1 then match(v1,p1) else NONE
     |(Tuple vs,TupleP ps) => if List.length vs = List.length ps 
                              then case all_answers match (ListPair.zip(vs,ps))  of
                                    SOME v2=>SOME v2
                                   |_ => NONE
                              else NONE
     |(_, Variable s ) => SOME [(s,v)]
     |(_,_) => NONE

fun first_match v patlst =
    SOME (first_answer (fn p => match (v, p)) patlst)
    handle NoAnswer => NONE