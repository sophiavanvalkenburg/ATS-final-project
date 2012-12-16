(* pattern.sats
 * by Sophia van Valkenburg
 * 
 *)

staload "prelude/DATS/list.dats"
staload "finite_am.sats"

typedef cont (a:t@ype, b:t@ype) = (a) -<cloref1> b
typedef charlist = List(char)

datatype pattern =
| Empty of ()
| Char of char
| Plus of (pattern, pattern)
| Times of (pattern, pattern)
| Paren of (pattern) (** paren is a dummy node to handle parentheses in the syntax **)
| Star of (pattern)

//[end of pattern]

fun pattern_to_nfa(p:pattern):nfa
fun accept_cont {n:int | n >= 0}(p:pattern, s:string(n)):bool
fun accept_nfa {n:int | n >= 0} (p:pattern, s:string(n)):bool
