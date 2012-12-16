(* finite_am.dats
 * by Sophia van Valkenburg
 *
 * functions for the nfa
 *)

staload "finite_am.sats"

#define nil list_nil
#define :: list_cons
#define cons list_cons

implement
state_eq(s1,s2) = 
    case+ (s1,s2) of
    | (State i, State j) => if (i=j) then true else false
    | (Start (), Start () ) => true
    | (Accept (), Accept ()) => true
    | (Reject (), Reject ()) => true
    | (_,_) => false
//end of [state_eq]

implement
input_eq(c1,c2) =
    case+ (c1, c2) of
    | (Ch a, Ch b) => if (a=b) then true else false
    | (Eps (), Eps() ) => true
    | (End (), End () ) => true
    | (_,_) => false
//end of [input_eq] 
 
implement
add_to_set(s,ss) =
let
    fun add(s:state, ss0:state_set, ss:state_set):state_set =
        case ss0 of
        | s0 :: ss1 => if state_eq(s0,s) then ss else add(s,ss1,ss)
        | nil () => s :: ss
    //end of [add]
in
    add(s,ss,ss)
end
//end of [add_to_set]

implement
combine_sets(s1,s2) =
    case+ s1 of
    | s0 :: ss => combine_sets(ss,add_to_set(s0,s2))
    | nil () => s2
//end of [combine_sets]


//z2 contains all elements of z1
implement
set_contains(z1,z2) =
let
    fun contains(s:state, ss:state_set):bool =
    case ss of
        | s0 :: ss0 => if state_eq(s,s0) then true else contains(s,ss0)
        | nil () => false
    //[end of contains]
in
    case z1 of
    | z10 :: z1s => if contains(z10, z2) then set_contains(z1s, z2) else false
    | nil () => true
end
//end of [set_contains]

implement
set_eq(s1,s2) = if (set_contains(s1,s2) && set_contains(s2, s1)) then true else false
//end of [set_eq]

implement
nfa_lookup(n,st,a) =
let
    fun lookup(n:nfa,nn:nfa,st:state,a:input,sts:state_set):state_set =
        case+ nn of
        | nil () => sts
        | (s1, b, s2) :: n1 => begin 
            case b of
            | Eps () => if state_eq(s1, st) then combine_sets(lookup(n,n1,st,a,sts),lookup(n,n,s2,a,sts)) 
                        else lookup(n,n1,st,a,sts)
            | _ =>  if (state_eq(s1, st) && input_eq(a, b)) then lookup(n,n1,st,a,add_to_set(s2,sts)) 
                    else lookup(n,n1, st, a, sts)
        end
    //end of [lookup]
in
    lookup(n,n,st,a,nil)
end
//end of [nfa_lookup]

//end of [finite_am.dats]
