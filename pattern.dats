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

typedef cont (a:t@ype, b:t@ype) = (a) -<cloref1> b
typedef charlist = List(char)

#define nil list_nil
#define :: list_cons
#define cons list_cons

datatype pattern =
| Empty of ()
| Char of char
| Plus of (pattern, pattern)
| Times of (pattern, pattern)
| Paren of (pattern) (** paren is a dummy node to handle parentheses in the syntax **)
| Star of (pattern)

//[end of pattern]
(*
exception ParseError of (string)

//generates a pattern given the pattern string
fun {n,m:int | n >= 0; m <= n} gen(last_child:pattern, p:string(n),i:int(m),len:int(n)):(pattern,int) =
    if (i=len) then (last_child,len) 
    else begin case p[i] of 
                | '(' => let 
                            val (in_paren,j) = gen(Empty(), p, i+1, len)
                            val paren = Paren(in_paren)
                            val (rest, k) = gen(paren,p,j,len)
                         in
                            case last_child of
                            | Empty () => (rest,k)
                            | p0 => (Times(p0,rest),k)
                         end
                | ')' => (last_child,i+1)
                | '*' => gen(Star(last_child), p, i+1, len)
                | '|' => Plus(last_child,gen(Empty(),p,i+1, len))
                |  c  => let
                            val (rest,j) = gen(Char c, p, i+1, len)
                         in
                            case last_child of
                            | Empty () => (rest, j)
                            | p0 => (Times(p0, rest),j)
                        end
            end
*)

fun p2n(p:pattern):nfa =
case p of
| Empty ()      => nil
| Char c        => ((State i1, NCh c, State (i+1)) :: nil
| Plus (p1, p2) => 
    let
        val (d1,j)1 = p2n(p1,i+1,i+1)
        val (d2,k) = p2n(p2,j+1)
        val d3 = (State i, Eps, State (i+1)) :: (State i, Eps, State (j+1)) :: nil
        val d4 = (State j, Eps, State (k+1)) :: (State k, Eps, State (k+1)) :: nil
    in
        (d3+d1+d2+d4,k+1)
    end
| Times (p1, p2)=>
    let
        val (d1,j) = p2n(p1,i+1)
        val (d2, k) = p2n(p2,j+1)
        val d3 = (State i, Eps, State (i+1)) :: (State j, Eps, State (j+1)) :: (State k, Eps, State (k+1)):: nil
    in
        (d1+d2+d3,k+1)
    end
| Paren p1  => p2n(p1,i)
| Star p1   => 
    let 
        val (d1,j) = p2n(p1,i+1)
        val d2 = (State i, Eps, State (i+1)) :: (State j, Eps, State i) :: nil
    in
       (d1+d2, i)
    end

fun pattern_to_dfa(p:pattern):dfa =
let
    val (n0, _) = p2n(p, 1)
    val n1 = (Start, Eps, State 1) :: nil
    val n = n1 + n0
in 
    nfa_to_dfa(n)
end

fun acc_cont (p:pattern, cs: charlist, k: cont(charlist,bool)):bool =
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

fun accept_cont {n:int | n >= 0}(p:pattern, s:string(n)):bool =
let
    val cs = string_explode(s)
    val cs2 = $UN.castvwtp1 {charlist} (cs)
    val matches = acc_cont (p, cs2, lam res => case res of nil () => true | cons _ => false)
    val () = list_vt_free(cs)
in
    matches
end
//end of [accept_cont]

fun acc_dfa (p:dfa, cs:charlist,st:state(*current state*)):bool =
    case cs of
    | c1 :: cs1 =>  
        let val next_st = dfa_lookup(p, st, Ch c1)
        in case+ next_st of
            | Accept () => true
            | Reject () => false
            | st2   => acc_dfa(p, cs1, st2)
        end
    | nil () => 
        let val next_st = dfa_lookup(p, st, End ())
        in case+ next_st of
            | Accept () => true
            | Reject () => false
            | _ => false 
        end
//end of [acc_dfa]

fun accept_dfa {n:int | n >= 0} (p:dfa, s:string(n)):bool =
let
    val cs = string_explode(s)
    val cs2 = $UN.castvwtp1 {charlist} (cs)
    val matches = acc_dfa (p, cs2, Start ())
    val () = list_vt_free(cs)
in
    matches
end
//end of [accept_dfa]



implement
main () =  
let
    val p1 = Star(Char('a'))
    val p2 = (Start, Ch 'a', Start) :: (Start, End, Accept) :: nil
    val p3 = Plus(Star(Char 'a'),Char 'b')
    val (n,_) = p2n(p3,1)
    val () = print_nfa(n)
    val () = println! ("accept_cont: ", accept_cont(p1,"aaaaaaaaaaaaaaaaaaaaaaaaaaa")) 
    val () = println! ("accept_dfa: ", accept_dfa(p2, "aaaaaaaaaaaaaaaaaaaaaaaaaaa"))
in end


//end of [pattern.dats]
