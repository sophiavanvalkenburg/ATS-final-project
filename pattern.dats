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
staload "dfa.sats"
dynload "dfa.dats"

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
    | c1 :: cs1 =>  let val next_st = dfa_lookup(p, st, Ch c1)
                    in case+ next_st of
                            | Accept () => true
                            | Reject () => false
                            | st2   => acc_dfa(p, cs1, st2)
                    end
    | nil () => let val next_st = dfa_lookup(p, st, End ())
                in case+ next_st of
                        | Accept () => true
                        | Reject () => false
                        | _ => false (* shouldn't happen *) 
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
    val p1 = Times(Char('a'),Star(Char('a')))
    val p2 = (Start, Ch 'a', State 1) :: (State 1, Ch 'a', State 1) :: (State 1, End, Accept) :: nil
    val () = println! ("accept_cont: ", accept_cont(p1,"aaa")) 
    val () = println! ("accept_dfa: ", accept_dfa(p2, "aaa"))
in end




//end of [pattern.dats]
