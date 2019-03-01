open Actions;;
open GccTypes;;
open Expressions;;

module Abstractions =
  struct 
    type t = 
      | Called of string
      | ParamTo of string * string * string
      | ParamShare of string * string
      | RetEq of string * Z.t
      | RetNeq of string * Z.t
      | RetLessThan of string * Z.t
      | RetLessThanEq of string * Z.t
      | RetGreaterThan of string * Z.t
      | RetGreaterThanEq of string * Z.t
      | ParEq of string * Z.t
      | ParNeq of string * Z.t
      | ParLessThan of string * Z.t
      | ParLessThanEq of string * Z.t
      | ParGreaterThan of string * Z.t
      | ParGreaterThanEq of string * Z.t
      | AccessPathStore of string 
      | AccessPathLoad of string 
      | AccessPathSensitive of string 
      | PropRet of string 
      | FunctionStart
      | FunctionEnd
      | RetError of string
      | RetConst of Z.t
      | Error
      | Debug

    let to_string a = 
      match a with 
      | Called (x) -> x
      | ParamTo (x,_,y) -> y ^ "_$PARAMTO_" ^ x
      | ParamShare (_,y) -> ""
      | RetEq (x,c) -> (x ^ "_$EQ_" ^ (Z.to_string c))
      | RetNeq (x,c) -> (x ^ "_$NEQ_" ^ (Z.to_string c))
      | RetLessThan (x,c) -> (x ^ "_$LT_" ^ (Z.to_string c))
      | RetLessThanEq (x,c) -> (x ^ "_$LTE_" ^ (Z.to_string c))
      | RetGreaterThan (x,c) -> (x ^ "_$GT_" ^ (Z.to_string c))
      | RetGreaterThanEq (x,c) -> (x ^ "_$GTE_" ^ (Z.to_string c))
      | ParEq (x,c) -> (x ^ "_$EQ_" ^ (Z.to_string c))
      | ParNeq (x,c) -> (x ^ "_$NEQ_" ^ (Z.to_string c))
      | ParLessThan (x,c) -> (x ^ "_$LT_" ^ (Z.to_string c))
      | ParLessThanEq (x,c) -> (x ^ "_$LTE_" ^ (Z.to_string c))
      | ParGreaterThan (x,c) -> (x ^ "_$GT_" ^ (Z.to_string c))
      | ParGreaterThanEq (x,c) -> (x ^ "_$GTE_" ^ (Z.to_string c))
      | AccessPathStore (p) -> ("!" ^ p)
      | AccessPathSensitive (p) -> ("?" ^ p)
      | PropRet (x) -> ("$RET_" ^ x)
      | RetError (s) -> ("$RET_" ^ s)
      | RetConst (c) -> ("$RET_" ^ (Z.to_string c))
      | FunctionStart -> "$START"
      | FunctionEnd -> "$END"
      | Error -> "$ERR"
      | Debug -> ""
      | _ -> ""

    module SS = Set.Make(String)
    
    let ignorelist =  List.fold_right SS.add [
        "__builtin_expect";
        "__builtin_types_compatible_p";
        "__builtin_constant_p";
        "__builtin_assume_aligned";
        "__builtin___clear_cache";
        "__builtin_prefetch";
        "__dynamic_pr_debug";
    ] SS.empty

    let pprint pre a = 
      print_string pre ; 
      print_endline (to_string a)
        
    let checkstartend a = match a.Hashcons.node with 
    | Action.Impl.Started -> [ Some FunctionStart ]
    | Action.Impl.Finished -> [ Some FunctionEnd ]
    | _ -> [ None ]

    let checkcalled a = match a.Hashcons.node with 
    | Action.Impl.Called e -> (
      match e.Hashcons.node with 
      | Expr.Impl.Call (_, n, _) -> [ Some (Called n) ]
      | _ -> [ None ]
    )
    | _ -> [ None ]

    let checkret a = match a.Hashcons.node with
    | Action.Impl.Returned e -> (
      match e.Hashcons.node with
      | Expr.Impl.Call (_, n, _) -> [ Some (PropRet n) ]
      | Expr.Impl.SIntegerConstant (_, c) -> [ Some (RetConst c) ]
      | Expr.Impl.UIntegerConstant (_, c) -> [ Some (RetConst c) ]
      | _ -> [ None ]
    )
    | _ -> [ None ]

    let checkparamto a = 
      let helper e = match e.Hashcons.node with 
      | Expr.Impl.CallParameter (_, pn, _, cl) -> (
          match cl.Hashcons.node with 
          | Expr.Impl.Call (_, cn, _) -> Some (pn, cn)
          | _ -> None 
      )
      | _ -> None    
      in match a.Hashcons.node with 
      | Action.Impl.Called e -> (
        match e.Hashcons.node with 
        | Expr.Impl.Call (_, n, params) ->
          List.flatten (Array.to_list (Array.map
            (fun el -> 
                (* let tmp = (match el.Hashcons.node with
                    | Expr.Impl.CallParameter (_, _, _, pr) -> (
                        match Hashtbl.find_opt p pr.Hashcons.hkey with 
                        | Some s -> SS.fold (fun s' acc -> 
                            if s' <> n then (Some (ParamShare (n, s'))) :: acc 
                            else acc
                        ) s []
                        | None -> [ None ] 
                    )
                    | _ -> [ None ]
                )
                in tmp @ ( *)
                match helper el with 
                | Some (pn, cn) -> [ Some (ParamTo (n, pn, cn)) ]
                | None -> [ None ]
            ) params
          ))
        | _ -> [ None ]
      )
      | _ -> [ None ] 

    let accesspaths a = 
      let rec helper e = 
        let validtype t = match t.Hashcons.node with 
        | GccType.Impl.UnionType  (n, _, _, _, _) ->  ((String.length n) != 0)
        | GccType.Impl.RecordType (n, _, _, _, _) ->  ((String.length n) != 0)
        | _ -> true 
        in let rec helper' e = match e.Hashcons.node with 
        | Expr.Impl.AddressOf (_, e') -> 
            ("(&" ^ (helper' e') ^ ")")
        | Expr.Impl.MemoryReference (_, e', _) ->
            ("(*" ^ (helper' e') ^ ")")
        | Expr.Impl.ComponentReference (_, mr, fd) -> (
            match (mr.Hashcons.node, fd.Hashcons.node) with 
            | (Expr.Impl.MemoryReference (_, b, _), Expr.Impl.FieldDeclaration (t, f)) ->
                if validtype t then 
                    ((helper' b) ^ "->" ^ (FieldDecl.name f)) else ""
            | (_, Expr.Impl.FieldDeclaration (t, f)) -> 
                if validtype t then 
                    ((helper' mr) ^ "." ^ (FieldDecl.name f)) else ""
            | _ -> ""
        )
        | Expr.Impl.ArrayReference (_, a, ic) -> (
            match ic.Hashcons.node with 
            | Expr.Impl.UIntegerConstant (_, i) -> 
                ((helper' a) ^ "[" ^ (Z.to_string i) ^ "]")
            | Expr.Impl.SIntegerConstant (_, i) -> 
                ((helper' a) ^ "[" ^ (Z.to_string i) ^ "]")
            | _ -> ((helper' a) ^ "[?]")
        )
        | _ -> ""
        in match e.Hashcons.node with 
        | Expr.Impl.SSA (_, _, _, e') -> (helper e')
        | Expr.Impl.RealPart (_, e') -> (helper e')
        | Expr.Impl.ImaginaryPart (_, e') -> (helper e')
        | Expr.Impl.PointerPlus (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.Plus (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.RealDiv (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.Maximum (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.Minimum (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.Minus (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.Times (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.ExactDiv (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.TruncatedDiv (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.TruncatedMod (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BitOr (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BitAnd (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BitXor (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BitLeftShift (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BitRightShift (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BitLeftRotate (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BitRightRotate (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BooleanEq (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BooleanNeq (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BooleanOr (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BooleanAnd (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BooleanNot (_, e') -> (helper e')
        | Expr.Impl.BooleanLessThan (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BooleanLessThanEq (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BooleanGreaterThan (_, l, r) -> (helper l) @ (helper r)
        | Expr.Impl.BooleanGreaterThanEq (_, l, r) -> (helper l) @ (helper r)
        | _ -> let res = (helper' e) in  
            if res = "" then [] else [ res ] 
      (* Loads *)
      (* in let lpaths = match a.Hashcons.node with 
      | Action.Impl.Called e -> helper e
      | Action.Impl.Returned e -> helper e
      | Action.Impl.Assigned (_, e) -> helper e
      | _ -> [] *)
      (* Stores *)
      in let spaths = match a.Hashcons.node with 
      | Action.Impl.Assigned (e, _) -> helper e
      | _ -> []
      (* Sensitives *)
      in let npaths = match a.Hashcons.node with 
      | Action.Impl.Assumed (_, e) -> helper e
      | _ -> []
      in 
        (* Return two lists concated *)
        (* (List.map 
          (fun x -> Some (AccessPathLoad x)
        ) lpaths) @  *)
        (List.map 
          (fun x -> Some (AccessPathStore x)
        ) spaths) @ 
        (List.map 
          (fun x -> Some (AccessPathSensitive x)
        ) npaths) 

    let matchconstant a c =
      let helper a b c d = match (a.Hashcons.node, b.Hashcons.node) with
      | (Expr.Impl.Call (_, n, _), Expr.Impl.SIntegerConstant (_, c')) ->
        if c == c' && (SS.mem n ignorelist) == false then 
          [ Some (d (n, c')) ] else [ None ]
      | (Expr.Impl.Call (_, n, _), Expr.Impl.UIntegerConstant (_, c')) ->
        if c == c' && (SS.mem n ignorelist) == false then 
          [ Some (d (n, c')) ] else [ None ]
      | (_, _) -> [ None ] 
      in match a.Hashcons.node with 
      | Action.Impl.Assumed (_, e) -> (
        match e.Hashcons.node with
        | Expr.Impl.BooleanEq (_, a, b) -> helper a b c (fun (f, g) -> RetEq (f, g))
        | Expr.Impl.BooleanNeq (_, a, b) -> helper a b c (fun (f, g) -> RetNeq (f, g)) 
        | Expr.Impl.BooleanGreaterThan (_, a, b) -> helper a b c (fun (f, g) -> RetGreaterThan (f, g))
        | Expr.Impl.BooleanLessThan (_, a, b) -> helper a b c (fun (f, g) -> RetLessThan (f, g))
        | Expr.Impl.BooleanGreaterThanEq (_, a, b) -> helper a b c (fun (f, g) -> RetGreaterThanEq (f, g))
        | Expr.Impl.BooleanLessThanEq (_, a, b) -> helper a b c (fun (f, g) -> RetLessThanEq (f, g))
        | _ -> [ None ]
      )
    | _ -> [ None ]

    let paramconstr a c p =
      let helper a b c d = 
        match Hashtbl.find_opt p a.Hashcons.hkey with
        | Some s -> SS.fold (fun s' acc ->
            match (a.Hashcons.node, b.Hashcons.node) with
            | (_, Expr.Impl.SIntegerConstant (_, c')) ->
                if c == c' && (SS.mem s' ignorelist) == false then 
                    Some (d (s', c')) :: acc else acc
            | (_, Expr.Impl.UIntegerConstant (_, c')) ->
                if c == c' && (SS.mem s' ignorelist) == false then 
                    Some (d (s', c')) :: acc else acc
            | (_, _) -> acc 
        ) s []
        | _ -> [ None ]
      in match a.Hashcons.node with 
        | Action.Impl.Assumed (_, e) -> (
            match e.Hashcons.node with
            | Expr.Impl.BooleanEq (_, a, b) -> helper a b c (fun (f, g) -> ParEq (f, g))
            | Expr.Impl.BooleanNeq (_, a, b) -> helper a b c (fun (f, g) -> ParNeq (f, g)) 
            | Expr.Impl.BooleanGreaterThan (_, a, b) -> helper a b c (fun (f, g) -> ParGreaterThan (f, g))
            | Expr.Impl.BooleanLessThan (_, a, b) -> helper a b c (fun (f, g) -> ParLessThan (f, g))
            | Expr.Impl.BooleanGreaterThanEq (_, a, b) -> helper a b c (fun (f, g) -> ParGreaterThanEq (f, g))
            | Expr.Impl.BooleanLessThanEq (_, a, b) -> helper a b c (fun (f, g) -> ParLessThanEq (f, g))
            | _ -> [ None ]
        )
        | _ -> [ None ]

    let rec clean l = match l with 
    | x :: xs -> (match x with 
      | Some i -> i :: clean xs 
      | None -> clean xs 
    )
    | [] -> [] 

    let firstpass arr pr = 
      List.iter (fun a -> 
        match a.Hashcons.node with 
        | Action.Impl.Called e -> (
            match e.Hashcons.node with 
            | Expr.Impl.Call (_, n, params) -> (Array.iteri
                (fun idx el -> match el.Hashcons.node with 
                    | Expr.Impl.CallParameter (_, _, _, p) -> (
                        match p.Hashcons.node with 
                        | Expr.Impl.SIntegerConstant (_, _) -> ()
                        | Expr.Impl.UIntegerConstant (_, _) -> ()
                        | _ -> (
                            match Hashtbl.find_opt pr p.Hashcons.hkey with 
                            | Some s -> 
                                Hashtbl.replace pr p.Hashcons.hkey (SS.add (n ^ "$p" ^ (string_of_int idx)) s)
                            | None -> 
                                Hashtbl.add pr p.Hashcons.hkey (SS.add (n ^ "$p" ^ (string_of_int idx)) SS.empty)
                        )
                    )
                    | _ -> raise (Invalid_argument "Should be impossible")
                )
            params)
            | _ -> ()
        )
        | _ -> ()
      ) arr

    let ignore arr =
      let bad n = 
        (* These __compiltime_assert_'s are pesky, ignore them *)
        (BatString.exists n "__compiletime_assert_") || 
        (SS.mem n ignorelist)
      in List.fold_left (fun acc a -> 
        let tmp = match a.Hashcons.node with 
        | Action.Impl.Called e -> (
            match e.Hashcons.node with 
            | Expr.Impl.Call (_, n, _) -> 
                if bad n then Action.nop else a
            | _ -> a 
        )
        | _ -> a
        in tmp :: acc
      ) [] arr

    let abstract arr pr =
      let clarr = ignore arr 
      in let _ = firstpass clarr pr
      in (pr, (List.fold_left (fun acc a -> 
        acc @ (clean ( 
          checkparamto a @ 
          checkcalled a @ 
          paramconstr a (Z.of_string "-2") pr @ 
          paramconstr a (Z.of_string "-1") pr @ 
          paramconstr a (Z.of_string "0") pr @ 
          paramconstr a (Z.of_string "1") pr @
          paramconstr a (Z.of_string "2") pr @
          paramconstr a (Z.of_string "3") pr @
          paramconstr a (Z.of_string "4") pr @
          paramconstr a (Z.of_string "8") pr @
          paramconstr a (Z.of_string "16") pr @
          paramconstr a (Z.of_string "32") pr @
          paramconstr a (Z.of_string "64") pr @
          matchconstant a (Z.of_string "-2") @ 
          matchconstant a (Z.of_string "-1") @ 
          matchconstant a (Z.of_string "0") @ 
          matchconstant a (Z.of_string "1") @
          matchconstant a (Z.of_string "2") @
          matchconstant a (Z.of_string "3") @
          matchconstant a (Z.of_string "4") @
          matchconstant a (Z.of_string "8") @
          matchconstant a (Z.of_string "16") @
          matchconstant a (Z.of_string "32") @
          matchconstant a (Z.of_string "64") @
          checkret a @
          checkstartend a
        )
      )
    ) [] clarr))

 end 