(* pattern.dats
 * by Sophia van Valkenburg
 * from "Dependent Types for Program Termination Verification" by Hongwei Xi
 *
 * constructs a data type 'pattern' (a subset of regular expressions)
 * and matches a given pattern and string using two methods
 *)

#define nil list0_nil // writing [nil] for list0_nil
#define :: list0_cons // writing [::] for list0_cons
#define cons list0_cons // writing [cons] for list0_cons

staload "prelude/DATS/list.dats"
staload "prelude/DATS/list_vt.dats"

datatype pattern =
| Empty of ()
| Char of char
| Plus of (pattern, pattern)
| Times of (pattern, pattern)
| Star of (pattern)

//[end of pattern]

(*
extern
fun lst_length {a:t@ype} (xs:list a):int

implement {a:t@ype}
length (xs) = let
    fun len (ys:list a, n:int):int = 
        case ys of
        | list_nil () => n
        | list_cons(x, ys) => len(ys, n+1)
in
    len(xs, 0)
end 
//[end of length]
*)

(*
extern
fun accept (p:pattern, s:string)
*)

implement
main () = let val abc = string_explode("abc"); val () = println! (list_vt_length(abc)); val () = list_vt_free(abc) in end


//end of [pattern.dats]
