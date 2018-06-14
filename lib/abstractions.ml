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
      | ParamTo (_,_,y) -> y
      | ParamShare (_,y) -> y
      | RetEq (x,c) -> (x ^ "_$EQ_" ^ (Z.to_string c))
      | RetNeq (x,c) -> (x ^ "_$NEQ_" ^ (Z.to_string c))
      | RetLessThan (x,c) -> (x ^ "_$LT_" ^ (Z.to_string c))
      | RetLessThanEq (x,c) -> (x ^ "_$LTE_" ^ (Z.to_string c))
      | RetGreaterThan (x,c) -> (x ^ "_$GT_" ^ (Z.to_string c))
      | RetGreaterThanEq (x,c) -> (x ^ "_$GTE_" ^ (Z.to_string c))
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
      | Expr.Impl.Call (_, "PTR_ERR", _) -> 
          [ Some (Error); Some (PropRet "PTR_ERR") ]
      | Expr.Impl.Call (_, n, _) -> 
          [ Some (PropRet n) ]
      | Expr.Impl.SIntegerConstant (_, c) ->
        if c == (Z.of_string "-1") then
            [ Some (Error); Some (RetError "EPERM") ]
        else if c == (Z.of_string "-2") then
            [ Some (Error); Some (RetError "ENOENT") ]
        else if c == (Z.of_string "-3") then
            [ Some (Error); Some (RetError "ESRCH") ]
        else if c == (Z.of_string "-4") then
            [ Some (Error); Some (RetError "EINTR") ]
        else if c == (Z.of_string "-5") then
            [ Some (Error); Some (RetError "EIO") ]
        else if c == (Z.of_string "-6") then
            [ Some (Error); Some (RetError "ENXIO") ]
        else if c == (Z.of_string "-7") then
            [ Some (Error); Some (RetError "E2BIG") ]
        else if c == (Z.of_string "-8") then
            [ Some (Error); Some (RetError "ENOEXEC") ]
        else if c == (Z.of_string "-9") then
            [ Some (Error); Some (RetError "EBADF") ]
        else if c == (Z.of_string "-10") then
            [ Some (Error); Some (RetError "ECHILD") ]
        else if c == (Z.of_string "-11") then
            [ Some (Error); Some (RetError "EAGAIN") ]
        else if c == (Z.of_string "-12") then
            [ Some (Error); Some (RetError "ENOMEM") ]
        else if c == (Z.of_string "-13") then
            [ Some (Error); Some (RetError "EACCES") ]
        else if c == (Z.of_string "-14") then
            [ Some (Error); Some (RetError "EFAULT") ]
        else if c == (Z.of_string "-15") then
            [ Some (Error); Some (RetError "ENOTBLK") ]
        else if c == (Z.of_string "-16") then
            [ Some (Error); Some (RetError "EBUSY") ]
        else if c == (Z.of_string "-17") then
            [ Some (Error); Some (RetError "EEXIST") ]
        else if c == (Z.of_string "-18") then
            [ Some (Error); Some (RetError "EXDEV") ]
        else if c == (Z.of_string "-19") then
            [ Some (Error); Some (RetError "ENODEV") ]
        else if c == (Z.of_string "-20") then
            [ Some (Error); Some (RetError "ENOTDIR") ]
        else if c == (Z.of_string "-21") then
            [ Some (Error); Some (RetError "EISDIR") ]
        else if c == (Z.of_string "-22") then
            [ Some (Error); Some (RetError "EINVAL") ]
        else if c == (Z.of_string "-23") then
            [ Some (Error); Some (RetError "ENFILE") ]
        else if c == (Z.of_string "-24") then
            [ Some (Error); Some (RetError "EMFILE") ]
        else if c == (Z.of_string "-25") then
            [ Some (Error); Some (RetError "ENOTTY") ]
        else if c == (Z.of_string "-26") then
            [ Some (Error); Some (RetError "ETXTBSY") ]
        else if c == (Z.of_string "-27") then
            [ Some (Error); Some (RetError "EFBIG") ]
        else if c == (Z.of_string "-28") then
            [ Some (Error); Some (RetError "ENOSPC") ]
        else if c == (Z.of_string "-29") then
            [ Some (Error); Some (RetError "ESPIPE") ]
        else if c == (Z.of_string "-30") then
            [ Some (Error); Some (RetError "EROFS") ]
        else if c == (Z.of_string "-31") then
            [ Some (Error); Some (RetError "EMLINK") ]
        else if c == (Z.of_string "-32") then
            [ Some (Error); Some (RetError "EPIPE") ]
        else if c == (Z.of_string "-33") then
            [ Some (Error); Some (RetError "EDOM") ]
        else if c == (Z.of_string "-34") then
            [ Some (Error); Some (RetError "ERANGE") ]
        else if c == (Z.of_string "-35") then
            [ Some (Error); Some (RetError "EDEADLK") ]
        else if c == (Z.of_string "-36") then
            [ Some (Error); Some (RetError "ENAMETOOLONG") ]
        else if c == (Z.of_string "-37") then
            [ Some (Error); Some (RetError "ENOLCK") ]
        else if c == (Z.of_string "-38") then
            [ Some (Error); Some (RetError "ENOSYS") ]
        else if c == (Z.of_string "-39") then
            [ Some (Error); Some (RetError "ENOTEMPTY") ]
        else if c == (Z.of_string "-40") then
            [ Some (Error); Some (RetError "ELOOP") ]
        else if c == (Z.of_string "-41") then
            [ Some (Error); Some (RetError "EWOULDBLOCK") ]
        else if c == (Z.of_string "-42") then
            [ Some (Error); Some (RetError "ENOMSG") ]
        else if c == (Z.of_string "-43") then
            [ Some (Error); Some (RetError "EIDRM") ]
        else if c == (Z.of_string "-44") then
            [ Some (Error); Some (RetError "ECHRNG") ]
        else if c == (Z.of_string "-45") then
            [ Some (Error); Some (RetError "EL2NSYNC") ]
        else if c == (Z.of_string "-46") then
            [ Some (Error); Some (RetError "EL3HLT") ]
        else if c == (Z.of_string "-47") then
            [ Some (Error); Some (RetError "EL3RST") ]
        else if c == (Z.of_string "-48") then
            [ Some (Error); Some (RetError "ELNRNG") ]
        else if c == (Z.of_string "-49") then
            [ Some (Error); Some (RetError "EUNATCH") ]
        else if c == (Z.of_string "-50") then
            [ Some (Error); Some (RetError "ENOCSI") ]
        else if c == (Z.of_string "-51") then
            [ Some (Error); Some (RetError "EL2HLT") ]
        else if c == (Z.of_string "-52") then
            [ Some (Error); Some (RetError "EBADE") ]
        else if c == (Z.of_string "-53") then
            [ Some (Error); Some (RetError "EBADR") ]
        else if c == (Z.of_string "-54") then
            [ Some (Error); Some (RetError "EXFULL") ]
        else if c == (Z.of_string "-55") then
            [ Some (Error); Some (RetError "ENOANO") ]
        else if c == (Z.of_string "-56") then
            [ Some (Error); Some (RetError "EBADRQC") ]
        else if c == (Z.of_string "-57") then
            [ Some (Error); Some (RetError "EBADSLT") ]
        else if c == (Z.of_string "-58") then
            [ Some (Error); Some (RetError "EDEADLOCK") ]
        else if c == (Z.of_string "-59") then
            [ Some (Error); Some (RetError "EBFONT") ]
        else if c == (Z.of_string "-60") then
            [ Some (Error); Some (RetError "ENOSTR") ]
        else if c == (Z.of_string "-61") then
            [ Some (Error); Some (RetError "ENODATA") ]
        else if c == (Z.of_string "-62") then
            [ Some (Error); Some (RetError "ETIME") ]
        else if c == (Z.of_string "-63") then
            [ Some (Error); Some (RetError "ENOSR") ]
        else if c == (Z.of_string "-64") then
            [ Some (Error); Some (RetError "ENONET") ]
        else if c == (Z.of_string "-65") then
            [ Some (Error); Some (RetError "ENOPKG") ]
        else if c == (Z.of_string "-66") then
            [ Some (Error); Some (RetError "EREMOTE") ]
        else if c == (Z.of_string "-67") then
            [ Some (Error); Some (RetError "ENOLINK") ]
        else if c == (Z.of_string "-68") then
            [ Some (Error); Some (RetError "EADV") ]
        else if c == (Z.of_string "-69") then
            [ Some (Error); Some (RetError "ESRMNT") ]
        else if c == (Z.of_string "-70") then
            [ Some (Error); Some (RetError "ECOMM") ]
        else if c == (Z.of_string "-71") then
            [ Some (Error); Some (RetError "EPROTO") ]
        else if c == (Z.of_string "-72") then
            [ Some (Error); Some (RetError "EMULTIHOP") ]
        else if c == (Z.of_string "-73") then
            [ Some (Error); Some (RetError "EDOTDOT") ]
        else if c == (Z.of_string "-74") then
            [ Some (Error); Some (RetError "EBADMSG") ]
        else if c == (Z.of_string "-75") then
            [ Some (Error); Some (RetError "EOVERFLOW") ]
        else if c == (Z.of_string "-76") then
            [ Some (Error); Some (RetError "ENOTUNIQ") ]
        else if c == (Z.of_string "-77") then
            [ Some (Error); Some (RetError "EBADFD") ]
        else if c == (Z.of_string "-78") then
            [ Some (Error); Some (RetError "EREMCHG") ]
        else if c == (Z.of_string "-79") then
            [ Some (Error); Some (RetError "ELIBACC") ]
        else if c == (Z.of_string "-80") then
            [ Some (Error); Some (RetError "ELIBBAD") ]
        else if c == (Z.of_string "-81") then
            [ Some (Error); Some (RetError "ELIBSCN") ]
        else if c == (Z.of_string "-82") then
            [ Some (Error); Some (RetError "ELIBMAX") ]
        else if c == (Z.of_string "-83") then
            [ Some (Error); Some (RetError "ELIBEXEC") ]
        else if c == (Z.of_string "-84") then
            [ Some (Error); Some (RetError "EILSEQ") ]
        else if c == (Z.of_string "-85") then
            [ Some (Error); Some (RetError "ERESTART") ]
        else if c == (Z.of_string "-86") then
            [ Some (Error); Some (RetError "ESTRPIPE") ]
        else if c == (Z.of_string "-87") then
            [ Some (Error); Some (RetError "EUSERS") ]
        else if c == (Z.of_string "-88") then
            [ Some (Error); Some (RetError "ENOTSOCK") ]
        else if c == (Z.of_string "-89") then
            [ Some (Error); Some (RetError "EDESTADDRREQ") ]
        else if c == (Z.of_string "-90") then
            [ Some (Error); Some (RetError "EMSGSIZE") ]
        else if c == (Z.of_string "-91") then
            [ Some (Error); Some (RetError "EPROTOTYPE") ]
        else if c == (Z.of_string "-92") then
            [ Some (Error); Some (RetError "ENOPROTOOPT") ]
        else if c == (Z.of_string "-93") then
            [ Some (Error); Some (RetError "EPROTONOSUPPORT") ]
        else if c == (Z.of_string "-94") then
            [ Some (Error); Some (RetError "ESOCKTNOSUPPORT") ]
        else if c == (Z.of_string "-95") then
            [ Some (Error); Some (RetError "EOPNOTSUPP") ]
        else if c == (Z.of_string "-96") then
            [ Some (Error); Some (RetError "EPFNOSUPPORT") ]
        else if c == (Z.of_string "-97") then
            [ Some (Error); Some (RetError "EAFNOSUPPORT") ]
        else if c == (Z.of_string "-98") then
            [ Some (Error); Some (RetError "EADDRINUSE") ]
        else if c == (Z.of_string "-99") then
            [ Some (Error); Some (RetError "EADDRNOTAVAIL") ]
        else if c == (Z.of_string "-100") then
            [ Some (Error); Some (RetError "ENETDOWN") ]
        else if c == (Z.of_string "-101") then
            [ Some (Error); Some (RetError "ENETUNREACH") ]
        else if c == (Z.of_string "-102") then
            [ Some (Error); Some (RetError "ENETRESET") ]
        else if c == (Z.of_string "-103") then
            [ Some (Error); Some (RetError "ECONNABORTED") ]
        else if c == (Z.of_string "-104") then
            [ Some (Error); Some (RetError "ECONNRESET") ]
        else if c == (Z.of_string "-105") then
            [ Some (Error); Some (RetError "ENOBUFS") ]
        else if c == (Z.of_string "-106") then
            [ Some (Error); Some (RetError "EISCONN") ]
        else if c == (Z.of_string "-107") then
            [ Some (Error); Some (RetError "ENOTCONN") ]
        else if c == (Z.of_string "-108") then
            [ Some (Error); Some (RetError "ESHUTDOWN") ]
        else if c == (Z.of_string "-109") then
            [ Some (Error); Some (RetError "ETOOMANYREFS") ]
        else if c == (Z.of_string "-110") then
            [ Some (Error); Some (RetError "ETIMEDOUT") ]
        else if c == (Z.of_string "-111") then
            [ Some (Error); Some (RetError "ECONNREFUSED") ]
        else if c == (Z.of_string "-112") then
            [ Some (Error); Some (RetError "EHOSTDOWN") ]
        else if c == (Z.of_string "-113") then
            [ Some (Error); Some (RetError "EHOSTUNREACH") ]
        else if c == (Z.of_string "-114") then
            [ Some (Error); Some (RetError "EALREADY") ]
        else if c == (Z.of_string "-115") then
            [ Some (Error); Some (RetError "EINPROGRESS") ]
        else if c == (Z.of_string "-116") then
            [ Some (Error); Some (RetError "ESTALE") ]
        else if c == (Z.of_string "-117") then
            [ Some (Error); Some (RetError "EUCLEAN") ]
        else if c == (Z.of_string "-118") then
            [ Some (Error); Some (RetError "ENOTNAM") ]
        else if c == (Z.of_string "-119") then
            [ Some (Error); Some (RetError "ENAVAIL") ]
        else if c == (Z.of_string "-120") then
            [ Some (Error); Some (RetError "EISNAM") ]
        else if c == (Z.of_string "-121") then
            [ Some (Error); Some (RetError "EREMOTEIO") ]
        else if c == (Z.of_string "-122") then
            [ Some (Error); Some (RetError "EDQUOT") ]
        else if c == (Z.of_string "-123") then
            [ Some (Error); Some (RetError "ENOMEDIUM") ]
        else if c == (Z.of_string "-124") then
            [ Some (Error); Some (RetError "EMEDIUMTYPE") ]
        else if c == (Z.of_string "-125") then
            [ Some (Error); Some (RetError "ECANCELED") ]
        else if c == (Z.of_string "-126") then
            [ Some (Error); Some (RetError "ENOKEY") ]
        else if c == (Z.of_string "-127") then
            [ Some (Error); Some (RetError "EKEYEXPIRED") ]
        else if c == (Z.of_string "-128") then
            [ Some (Error); Some (RetError "EKEYREVOKED") ]
        else if c == (Z.of_string "-129") then
            [ Some (Error); Some (RetError "EKEYREJECTED") ]
        else if c == (Z.of_string "-130") then
            [ Some (Error); Some (RetError "EOWNERDEAD") ]
        else if c == (Z.of_string "-131") then
            [ Some (Error); Some (RetError "ENOTRECOVERABLE") ]
        else if c == (Z.of_string "-132") then
            [ Some (Error); Some (RetError "ERFKILL") ]
        else if c == (Z.of_string "-133") then
            [ Some (Error); Some (RetError "EHWPOISON") ]
        else 
            [ Some (RetConst c) ]
      | Expr.Impl.UIntegerConstant (_, c) -> [ Some (RetConst c) ]
      | _ -> [ None ]
    )
    | _ -> [ None ]

    let checkparamto a p = 
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
                let tmp = (match el.Hashcons.node with
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
                in tmp @ (match helper el with 
                | Some (pn, cn) -> [ Some (ParamTo (n, pn, cn)) ]
                | None -> [ None ]
                )
            ) params
          ))
        | _ -> [ None ]
      )
      | _ -> [ None ] 

    let accesspaths a = 
      let rec helper e = 
        let validtype t = match t.Hashcons.node with 
        | GccType.Impl.UnionType (n, _, _, _, _) ->  ((String.length n) != 0)
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

    let rec clean l = match l with 
    | x :: xs -> (match x with 
      | Some i -> i :: clean xs 
      | None -> clean xs 
    )
    | [] -> [] 

    let paramshare arr pr = 
      List.iter (fun a -> 
        match a.Hashcons.node with 
        | Action.Impl.Called e -> (
            match e.Hashcons.node with 
            | Expr.Impl.Call (_, n, params) -> (Array.iter
                (fun el -> match el.Hashcons.node with 
                    | Expr.Impl.CallParameter (_, _, _, p) -> (
                        match p.Hashcons.node with 
                        | Expr.Impl.SIntegerConstant (_, _) -> ()
                        | Expr.Impl.UIntegerConstant (_, _) -> ()
                        | _ -> (
                            match Hashtbl.find_opt pr p.Hashcons.hkey with 
                            | Some s -> 
                                Hashtbl.replace pr p.Hashcons.hkey (SS.add n s)
                            | None -> 
                                Hashtbl.add pr p.Hashcons.hkey (SS.add n SS.empty)
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
      in let _ = paramshare clarr pr
      in (pr, (List.fold_left (fun acc a -> 
        acc @ (clean ( 
          accesspaths a @
          checkparamto a pr @ 
          checkcalled a @ 
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