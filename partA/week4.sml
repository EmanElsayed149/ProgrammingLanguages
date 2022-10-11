(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
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

(*val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s,0)));*)

fun  longest_string1 xs= List.foldl (fn (s,x) => if String.size s > String.size x then s else x)  "" xs ;

fun  longest_string2 xs= List.foldl (fn (s,x) => if String.size s >= String.size x then s else x) "" xs ;
fun longest_string_helper f = foldl (fn (s,x) => if  f(String.size s, String.size x) then s else x)  "" ;
			  
val longest_string3 = longest_string_helper ( fn (x,y) => x > y ); 

val longest_string4 = longest_string_helper (fn (x,y) => x >= y );

val longest_capitalized = longest_string1 o only_capitals 
fun rev_string s  =(implode o rev o  explode) s;

fun first_answer f ls =
    case ls of
	[] => raise  NoAnswer
      | x::xs  => case f(x) of
		      NONE => first_answer f xs
		    | SOME v => v

            
 fun all_answers f ls =
    let val has_none = List.exists (fn x=> f(x) = NONE)				      
    in
	 case ls of
	[] => SOME  []
     |  x::xs  => case has_none ls  of
		      true  => NONE
		    |false  => SOME (List.foldl(fn (xx,acc)=> case f(xx) of SOME v => v@acc ) [] ls)
						  
    end
   (*           			    fun all_answers f lst=  
    let 
      val has_none = List.exists (fn x=> f(x) = NONE)
       (*assumed that it won't be called if NONE present*)
      val final_result = List.foldl (fn(x,acc)  => (case f(x) of SOME v => v@acc ))
    in 
    case lst of
      [] => SOME []
      |_=>if has_none lst 
          then NONE
          else SOME (final_result [] lst)
    end*)  
	
 fun count_wildcards p =
     g (fn _ => 1) (fn x => 0) p;

fun count_wild_and_variable_lengths p =
    g (fn _ => 1) (fn s => String.size s) p;

fun count_some_var (st,p) =
    g (fn _ => 0) (fn s => if s= st then 1 else 0 ) p;


fun check_pat p =
    let fun  all_strings p =
	     case p of
		 Variable v => [v]
	       | TupleP ps  => foldl( fn (x,acc) => all_strings(x)  @ acc) [] ps
	       | _ => []
        fun all_same x  = List.exists (fn s=> (x=s))
	fun res lst acc =
	     case lst  of
	     []=> true
              | x::xs  => if (all_same acc lst)
		          then res xs acc
	                   else false

				   
    in
	 case all_strings p of
	 []=> true
         | x::xs  => 	res (all_strings p) x
    end;


fun match (v,p): (string * valu) list option =	
    case (v,p) of
	(_,Wildcard) => SOME []
      | (_,Variable s) => SOME [(s,v)]
      | (Unit,UnitP) => SOME []
      |(Const c1,ConstP c2)  => if c1=c2 then SOME [] else NONE
      | (Constructor (s1,v1),ConstructorP (s2,p1)) => if s1 = s2 then match(v1,p1) else NONE
      | (Tuple vs,TupleP ps) => (case  all_answers match (ListPair.zip (vs,ps)) of
				     SOME v2 => SOME v2
				       | _ => NONE)
      | (_,_) =>NONE
	
fun first_match v ps =
  SOME (first_answer (fn p => match (v,p)) ps)
  handle NoAnswer => NONE
