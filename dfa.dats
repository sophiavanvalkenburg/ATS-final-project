(* dfa.dats
 * by Sophia van Valkenburg
 *
 * functions for the dfa
 *)

staload "dfa.sats"

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

//end of [dfa.dats]
