(* finite_am.sats
 * by Sophia van Valkenburg
 * 
 * a collection of data types representing nfa
 * helpful functions for nfa
 *)

staload "prelude/DATS/list.dats"

datatype state =
| State of int
| Start of ()
| Accept of ()
| Reject of ()

datatype input =
| Ch of char
| Eps of ()
| End of ()

typedef transition = (state (*current state*), input, state (*next state*))
typedef nfa = List(transition)

typedef state_set = List(state)

fun nfa_lookup(n:nfa, st:state, a:input):state_set

fun state_eq (s1:state, s2:state):bool
fun input_eq(c1:input, c2:input):bool
fun add_to_set(s:state,ss:state_set):state_set
fun combine_sets(s1:state_set,s2:state_set):state_set
fun set_contains(z1:state_set, z2:state_set):bool
fun set_eq(s1:state_set, s2:state_set):bool
