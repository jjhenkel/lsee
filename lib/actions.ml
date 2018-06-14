
open Expressions;;

module Action = 
  struct 
    module Impl =
      struct 
        type ths = t Hashcons.hash_consed and t =
        | Nopped                
        | Started               
        | Finished              
        | Debugged              of string
        | Predicted             of string 
        | Unsupported           of string
        | Called                of Expr.t
        | Assumed               of int * Expr.t
        | Observed              of Expr.t
        | Returned              of Expr.t
        | Assigned              of Expr.t * Expr.t

      let hash t = Hashtbl.hash_param 100 100 t

      let equal a b = match (a, b) with
      | Nopped, Nopped -> true
      | Started, Started -> true 
      | Finished, Finished -> true
      | Debugged s1, Debugged s2 -> s1 == s2 
      | Predicted s1, Predicted s2 -> s1 == s2
      | Unsupported s1, Unsupported s2 -> s1 == s2
      | Called e1, Called e2 -> e1 == e2
      | Assumed (i1, e1), Assumed (i2, e2) -> i1 == i2 && e1 == e2
      | Observed e1, Observed e2 -> e1 == e2
      | Returned e1, Returned e2 -> e1 == e2
      | Assigned (l1, r1), Assigned (l2, r2) -> l1 == l2 && r1== r2
      | _ -> false

      let prettify a = match a.Hashcons.node with
      | Nopped -> "NOP"
      | Started -> "START"
      | Finished -> "FINISH"
      | Debugged s -> "DEBUG(" ^ s ^ ")"
      | Predicted s -> "PREDICT(" ^ s ^ ")"
      | Unsupported s -> "UNSUP(" ^ s ^ ")"
      | Called e -> "CALL(" ^ (Expr.prettify e) ^ ")"
      | Assumed (i,e) -> "ASSUME(" ^ (Expr.prettify e) ^ ")"
      | Observed e -> "OBSERVE(" ^ (Expr.prettify e) ^ ")"
      | Returned e -> "RETURN(" ^ (Expr.prettify e) ^ ")"
      | Assigned (l,r) -> "ASSIGN(" ^ (Expr.prettify l) ^ " <- " ^ (Expr.prettify r) ^ ")"
    end

    type t = Impl.ths

    module Ht = Hashcons.Make(Impl)

    let ht = Ht.create 5077

    let nop = Ht.hashcons ht Impl.Nopped
    let start = Ht.hashcons ht Impl.Started
    let finish = Ht.hashcons ht Impl.Finished
    let debug s = Ht.hashcons ht @@ Impl.Debugged s
    let predict s = Ht.hashcons ht @@ Impl.Predicted s
    let unsupport s = Ht.hashcons ht @@ Impl.Unsupported s
    let call e = Ht.hashcons ht @@ Impl.Called e
    let assume (i, e) = Ht.hashcons ht @@ Impl.Assumed (i, e)
    let observe e = Ht.hashcons ht @@ Impl.Observed e
    let return e = Ht.hashcons ht @@ Impl.Returned e
    let assign (l, r) = Ht.hashcons ht @@ Impl.Assigned (l, r)

    let prettify x = Impl.prettify x

  end