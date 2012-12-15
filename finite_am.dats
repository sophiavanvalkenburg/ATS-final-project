(* finite_am.dats
 * by Sophia van Valkenburg
 *
 * functions for the nfa and dfa
 *)

staload "finite_am.sats"

#define nil list_nil
#define :: list_cons
#define cons list_cons

implement
dfa_lookup(d, st, a) =
let
    fun state_eq (s1:state, s2:state):bool =
        case+ (s1,s2) of
        | (State i, State j) => if (i=j) then true else false
        | (Start (), Start () ) => true
        | (Accept (), Accept ()) => true
        | (Reject (), Reject ()) => true
        | (_,_) => false
    //end of [state_eq]
    
    fun input_eq(c1:input, c2:input):bool =
        case+ (c1, c2) of
        | (Ch a, Ch b) => if (a=b) then true else false
        | (End (), End () ) => true
        | (_,_) => false
    //end of [input_eq]
in
    case d of
    | (s1, b, s2) :: d1 =>  if (state_eq(s1, st) && input_eq(a, b)) then s2 
                            else dfa_lookup(d1, st, a)
    | nil () => Reject ()
end

implement
nfa_to_dfa(n) = nil


extern
fun print_input(i:input_nfa):void
implement
print_input(i) = 
case i of
| NCh c => println! (c)
| Eps () => println! ("Eps")
| NEnd () => println! ("End")

extern
fun print_state(s:state):void
implement
print_state(s) = 
case s of
| State i => println! ("State ", i)
| Start () => println! ("Start")
| Accept () => println! ("Accept")
| Reject () => println! ("Reject")

extern
fun print_Ntrans(t:Ntransition):void
implement
print_Ntrans(t) =
let
    val (s1, i, s2) = t
    val () = print_state(s1)
    val () = print_input(i)
    val () = print_state(s2)
    val () = println! ("--")
in end

extern
fun print_nfa(n:nfa):void
implement
print_nfa(n) =
case n of
| t :: n1 => let val () = print_Ntrans(t); val () = print_nfa(n1) in end
| nil => println! (" nil")


//end of [finite_am.dats]
