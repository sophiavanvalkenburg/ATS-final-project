(* pattern.dats
 * by Sophia van Valkenburg
 * from "Dependent Types for Program Termination Verification" by Hongwei Xi
 *
 * constructs a data type 'pattern' (a subset of regular expressions)
 * and matches a given pattern and string using two methods
 *)

staload UN = "prelude/SATS/unsafe.sats"
staload "prelude/DATS/list.dats"
staload "prelude/DATS/list_vt.dats"

staload "finite_am.sats"
staload "finite_am.dats"
dynload "finite_am.dats"

staload "pattern.sats"

#define nil list_nil
#define :: list_cons
#define cons list_cons


// convert a pattern to an nfa
extern
fun p2n(p:pattern,i1:int(*current state*),i2:int(*next state*)):(nfa,int,int)
implement
p2n(p,i1,i2) =
case p of
| Empty ()      => (nil,i1,i2)
| Char c        => ((State i2, Ch c, State (i2+1)) :: nil,i2+1,i2+2)
| Plus (p1, p2) => 
    let
        val (d1,j1,j2) = p2n(p1,i1,i2)
        val (d2,k1,k2) = p2n(p2,j1,j2)
        val d3 = (State i1, Eps, State i2) :: (State i1, Eps, State j2) :: nil
        val d4 = (State j1, Eps, State k2) :: (State k1, Eps, State k2) :: nil
    in
        (d3+d1+d2+d4,k2,k2+1)
    end
| Times (p1, p2)=>
    let
        val (d1,j1,j2) = p2n(p1,i1,i2)
        val (d2, k1,k2) = p2n(p2,j1,j2)
        val d3 = (State i1, Eps, State i2) :: (State j1, Eps, State j2) :: (State k1, Eps, State k2):: nil
    in
        (d1+d2+d3,k2,k2+1)
    end
| Paren p1  => p2n(p1,i1,i2)
| Star p1   => 
    let 
        val (d1,j1,j2) = p2n(p1,i2,i2+1)
        val d2 = (State i1, Eps, State i2) :: (State i2, Eps, State (i2+1)) :: (State j1, Eps, State i2) :: nil
    in
       (d1+d2,i2,j2)
    end
//end of [p2n]

implement
pattern_to_nfa(p) =
let
    val (n0, last, _) = p2n(p,0,1)
    val n1 = (Start, Eps, State 0) :: (State last, End, Accept) :: nil
in 
    n1 + n0
end
//end of [pattern_to_nfa]

(* 
 * ***********************************
 * pattern matching continuation style
 * ***********************************
 *)
extern   
fun acc_cont (p:pattern, cs: charlist, k: cont(charlist,bool)):bool
implement
acc_cont(p,cs,k) =
    case p of
    | Empty() => k(cs) 
    | Char(c) => begin case+ cs of
        | c1 :: cs1 => if (c=c1) then k(cs1) else false
        | nil () => false
        end
    | Plus (p1, p2) => 
        if acc_cont(p1, cs, k) then true else acc_cont(p2, cs, k)
    | Times (p1, p2) => acc_cont(p1, cs, (lam res => acc_cont(p2, res, k)))
    | Paren (p0) => acc_cont(p0, cs, k)
    | Star (p0) =>  
        if k(cs) then true
        else acc_cont(p0, cs, (lam res => 
                            if (list_length(res) = list_length(cs)) then false
                            else acc_cont(p, res, k)))
// end of [acc_cont]

implement
accept_cont (p, s) =
let
    val cs = string_explode(s)
    val cs2 = $UN.castvwtp1 {charlist} (cs)
    val matches = acc_cont (p, cs2, lam res => case res of nil () => true | cons _ => false)
    val () = list_vt_free(cs)
in
    matches
end
//end of [accept_cont]


(* 
 * **************************
 * pattern matching nfa style
 * **************************
 *)
extern
fun acc_nfa (p:nfa, cs:charlist,st:state(*current state*)):bool
implement
acc_nfa(p,cs,st) =
let
    fun all_acc_nfa(p:nfa, cs:charlist,sts:state_set):bool =
        case sts of
        | nil () => false
        | s1 :: rest => if acc_nfa(p,cs,s1) then true 
                        else all_acc_nfa(p,cs,rest)
    //end of [all_acc_nfa]
in
    case cs of
    | c1 :: cs1 =>  
        let val states = nfa_lookup(p, st, Ch c1)
        in  all_acc_nfa(p,cs1,states) 
        end
    | nil () => 
        let val states = nfa_lookup(p, st, End )
        in  if set_contains(Accept :: nil, states) then true
            else false
        end
end
//end of [acc_nfa]

implement
accept_nfa (p, s) =
let
    val cs = string_explode(s)
    val cs2 = $UN.castvwtp1 {charlist} (cs)
    val n = pattern_to_nfa(p)
    val matches = acc_nfa (n, cs2, Start)
    val () = list_vt_free(cs)
in
    matches
end
//end of [accept_nfa]


//end of [pattern.dats]
