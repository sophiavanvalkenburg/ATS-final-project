(* dfa.sats
 * by Sophia van Valkenburg
 * 
 * a collection of data types representing a dfa
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

typedef transition = (state (*current state*), input, state (*next state*))
typedef dfa = List(transition)

fun dfa_lookup (d:dfa, st:state, a:input):state


