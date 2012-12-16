(* test_nfa.dats
 * by Sophia van Valkenburg
 *
 * Methods for testing nfa style pattern matching
 * from pattern.dats
 *)

staload "pattern.sats"
staload "pattern.dats"
dynload "pattern.dats"

implement
main () =  
let
     // (aa*)|(ab)* 
    val p1 = Plus(Times(Char 'a',Star(Char 'a')),Star(Times(Char 'a', Char 'b')))
    // (a|b)*(abb|a+b)
    val p2 = Times(Star(Plus(Char 'a', Char 'b')), Plus(Times(Times(Char 'a', Char 'b'), Char 'b'),Times(Times(Char 'a', Star(Char 'a')),Char 'b')))
    
    val () = println! ("p1: ", accept_nfa(p1, "ababababababababababababababababababab"))
    val () = println! ("p2: ", accept_nfa(p2, "ababbbbbbbbbbbb"))
in end
