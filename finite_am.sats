(* finite_am.sats
 * by Sophia van Valkenburg
 * 
 * a collection of data types representing dfa and nfa
 * helpful functions for dfa and nfa
 *)

staload "prelude/DATS/list.dats"

datatype state =
| State of int
| Start of ()
| Accept of ()
| Reject of ()

datatype input =
| Ch of char
| End of ()

datatype input_nfa =
| NCh of char
| Eps of ()
| NEnd of ()

typedef transition = (state (*current state*), input, state (*next state*))
typedef Ntransition = (state, input_nfa, state)
typedef dfa = List(transition)
typedef nfa = List(Ntransition)

fun dfa_lookup (d:dfa, st:state, a:input):state
fun nfa_to_dfa (n:nfa):dfa

