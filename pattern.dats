(* pattern.dats
 * by Sophia van Valkenburg
 * from "Dependent Types for Program Termination Verification" by Hongwei Xi
 *
 * constructs a data type 'pattern' (a subset of regular expressions)
 * and matches a given pattern and string using two methods
 *)

staload "prelude/DATS/list.dats"
staload "prelude/DATS/list_vt.dats"

typedef cont_vt (a: viewt@ype, b: t@ype) = (a) -<cloref1> b
viewtypedef charlist = [n:int | n >= 0] list_vt(char,n)

macdef ret (cs, b) = let val () = fold@ ,(cs) in ,(b) end

datatype pattern =
| Empty of ()
| Char of char
| Plus of (pattern, pattern)
| Times of (pattern, pattern)
| Star of (pattern)

//[end of pattern]


fun acc (p:pattern, cs: !List_vt(char), k: (!List_vt (char)) -<cloref1> bool):bool =
    case p of
    | Empty() => k( cs) 
    | Char(c) => begin case+ cs of
                    | list_vt_cons (c1, !cs1) => if (c=c1) then k(cs1) else false
                    | list_vt_nil () => false
                    end
    | Plus (p1, p2) => if acc(p1, cs, k) then true else acc(p2, cs, k)
    | Times (p1, p2) => acc(p1, cs, (lam res => acc(p2, res, k)))
    | Star (p0) =>  if k(cs) then true
                    else acc(p0, cs, (lam res => 
                                        if (list_vt_length(res) = list_vt_length(cs)) then false
                                        else acc(p, res, k)))

fun accept {n:int | n >= 0}(p:pattern, s:string(n)):bool =
let
    val exploded_s = string_explode(s)
    val matches = acc (p, exploded_s, lam res => case res of 
                                            | list_vt_nil () => true
                                            | list_vt_cons(c,cs) => false)
    val () = list_vt_free(exploded_s)
in
    matches
end

implement
main () = let val () = println! (accept(Empty,"abc")) in end


//end of [pattern.dats]
