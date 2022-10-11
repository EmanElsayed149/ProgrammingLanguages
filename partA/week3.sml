fun same_string(s1:string,s2:string)=
    s1=s2;
fun all_except_option (s,ls)=
    case ls of
	[] => NONE
      | x::xs => case same_string(x,s) of
		     true => SOME xs
		   | false  => case all_except_option (s,xs) of
				   NONE => NONE
				 | SOME y => SOME(x::y);
fun get_substitutions1 (lls,s)=
    case lls of
	[] => []
      | (ls::xxs) => case all_except_option (s,ls) of
		       NONE => get_substitutions1 (xxs,s)
		     | SOME y =>  y @ get_substitutions1 (xxs,s)
fun get_substitutions2 (lls,s)=
    let fun aux(lls,s,acc)=
	    case lls of
		[] => acc
	      | (ls:: xxs)  => case all_except_option (s,ls) of
				   NONE => aux (xxs,s,acc)
				 | SOME y  => aux (xxs,s,acc @ y)
    in aux (lls,s,[])
    end

	
      
type Name={first:string , middle:string , last:string}

	      
fun similar_names (lls,name)=
    let val {first =f , middle =m , last =l}=name ; fun aux (subs,acc)=
	    case subs of
		[] => acc
	      | (x::xs)  => aux(xs,acc @ [{first=x ,middle=m ,last= l}])
    in
	aux (get_substitutions2 (lls,f),[name])
    end


datatype suit= Clubs | Diamonds | Hearts | Spades
datatype rank= Jack | Queen | King | Ace | Num of int
type card    = suit * rank


datatype color= Red | Black

exception IllegalMove

fun card_color (suit ,rank)=
    case suit of
	Clubs => Black
      | Spades => Black
      | Diamonds=> Red
      | Hearts =>  Red

fun card_value (suit,rank)=
    case rank of
	Ace => 11
      | Num (x)  => x
      | _ => 10
		 

fun remove_card (cs,c,e)=
    case cs of
	[] => raise e
      | x::xs  => case c=x of
		      true => xs
		    | false  => x :: remove_card (xs,c,e)
						 
fun all_same_color (cs)=
    case cs of
	[] => true
      | x:: [] => true
      | x::y::xs  => case card_color (x) = card_color (y) of
			 true => all_same_color (y::xs)
		       | false  => false
				       
fun sum_cards(cs)=
    let fun aux (cs,acc)=
	    case cs of
		[] => acc
	      | x::xs  => (aux(xs,acc+card_value(x)))
    in
	aux(cs,0)
    end


fun score (lc,goal:int)=
    let fun pre_score (lc,goal) =
	    case ( sum_cards (lc) ,  goal) of
		(sum,goal) => case sum > goal of
				  true => 3 * (sum - goal)
				| false  => (goal - sum)
    in
	case all_same_color (lc) of
	    true => pre_score (lc,goal) div 2
	  | false => pre_score (lc,goal)
    end


datatype move = Discard of card | Draw


fun officiate (cs,ms,goal)=
    let fun pre_process (cs,ms,held) =
	    case ms of
		[] => held
	      | m:: ms_tail  => case m of
				    Discard card => pre_process(cs, ms_tail, remove_card (held , card ,  IllegalMove))
				  | Draw          => case cs of
							 [] => held
						      |  c:: _ => case sum_cards(c::held) > goal of
								      true => c::held
								   |  false  => pre_process (remove_card(cs , c,  IllegalMove ), ms_tail , c :: held)
    in
	score (pre_process (cs ,ms, []), goal)
    end
	
			      

