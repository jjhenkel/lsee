
open GccTypes;;

module Expr = 
  struct 
    module Impl =
      struct 
        type ths = t Hashcons.hash_consed and t = 
        | Unsupported           of string
        | Nothing               of GccType.t
        | Call                  of GccType.t * string * (ths array)
        | Cast                  of GccType.t * ths
        | Constructor           of GccType.t
        | AddressOf             of GccType.t * ths
        | BitFieldRef           of GccType.t * ths * int * int
        | RealConstant          of GccType.t * float
        | StringConstant        of GccType.t * int * string
        | SIntegerConstant      of GccType.t * Z.t
        | UIntegerConstant      of GccType.t * Z.t
        | ArrayReference        of GccType.t * ths * ths
        | MemoryReference       of GccType.t * ths * ths
        | ComponentReference    of GccType.t * ths * ths
        | FieldDeclaration      of GccType.t * FieldDecl.t
        | LabelDeclaration      of GccType.t * string
        | ConstDeclaration      of GccType.t * string
        | VariableDeclaration   of GccType.t * VarDecl.t
        | FunctionDeclaration   of GccType.t * string
        | ParameterDeclaration  of GccType.t * string * GccType.t
        | RealPart              of GccType.t * ths
        | ImaginaryPart         of GccType.t * ths
        | PointerPlus           of GccType.t * ths * ths
        | SSA                   of GccType.t * string * int * ths
        | CallParameter         of GccType.t * string * int * ths
        | Plus                  of GccType.t * ths * ths
        | RealDiv               of GccType.t * ths * ths
        | Maximum               of GccType.t * ths * ths
        | Minimum               of GccType.t * ths * ths
        | Minus                 of GccType.t * ths * ths
        | Times                 of GccType.t * ths * ths
        | ExactDiv              of GccType.t * ths * ths
        | TruncatedDiv          of GccType.t * ths * ths
        | TruncatedMod          of GccType.t * ths * ths
        | BitOr                 of GccType.t * ths * ths
        | BitAnd                of GccType.t * ths * ths
        | BitXor                of GccType.t * ths * ths
        | BitLeftShift          of GccType.t * ths * ths
        | BitRightShift         of GccType.t * ths * ths
        | BitLeftRotate         of GccType.t * ths * ths
        | BitRightRotate        of GccType.t * ths * ths
        | BooleanEq             of GccType.t * ths * ths
        | BooleanOr             of GccType.t * ths * ths
        | BooleanAnd            of GccType.t * ths * ths
        | BooleanNot            of GccType.t * ths
        | BooleanNeq            of GccType.t * ths * ths
        | BooleanLessThan       of GccType.t * ths * ths
        | BooleanLessThanEq     of GccType.t * ths * ths
        | BooleanGreaterThan    of GccType.t * ths * ths
        | BooleanGreaterThanEq  of GccType.t * ths * ths
        | ResultDecl            of GccType.t * string

      (* 
         Q: Why so much PHYSICAL EQUALITY??  
         A: Because the magic of hashconsing assures us that 
            structurally equal values are also physically equal
      *)
      let equal a b = match (a, b) with
      | ResultDecl (t1, s1), ResultDecl (t2, s2) -> 
          t1 == t2 && s1 == s2
      | Unsupported m1, Unsupported m2 -> 
          m1 == m2
      | Nothing t1, Nothing t2 -> 
          t1 == t2
      | Call (t1,n1,p1), Call (t2,n2,p2) -> 
          t1 == t2 && n1 == n2 && p1 == p2 
      | Cast (t1, e1), Cast (t2, e2) -> 
          t1 == t2 && e1 == e2
      | Constructor t1, Constructor t2 ->
          t1 == t2
      | AddressOf (t1, e1), AddressOf (t2, e2) ->
          t1 == t2 && e1 == e2
      | BitFieldRef (t1, e1, i1, r1), BitFieldRef (t2, e2, i2, r2) ->
          t1 == t2 && e1 == e2 && i1 == i2 && r1 == r2
      | RealConstant (t1, f1), RealConstant (t2, f2) ->
          t1 == t2 && f1 == f2
      | StringConstant (t1, s1, l1), StringConstant (t2, s2, l2) ->
          t1 == t2 && l1 == l2 && s1 == s2
      | SIntegerConstant (t1, i1), SIntegerConstant (t2, i2) ->
          t1 == t2 && i1 == i2 
      | UIntegerConstant (t1, i1), UIntegerConstant (t2, i2) ->
          t1 == t2 && i1 == i2
      | ArrayReference (t1, e1, i1), ArrayReference (t2, e2, i2) ->
          t1 == t2 && e1 == e2 && i1 == i2
      | MemoryReference (t1, e1, i1), MemoryReference (t2, e2, i2) ->
          t1 == t2 && e1 == e2 && i1 == i2
      | ComponentReference (t1, e1, i1), ComponentReference (t2, e2, i2) ->
          t1 == t2 && e1 == e2 && i1 == i2
      | FieldDeclaration (t1, n1), FieldDeclaration (t2, n2) ->
          t1 == t2 && n1 == n2
      | LabelDeclaration (t1, n1), LabelDeclaration (t2, n2) ->
          t1 == t2 && n1 == n2
      | ConstDeclaration (t1, n1), ConstDeclaration (t2, n2) ->
          t1 == t2 && n1 == n2
      | VariableDeclaration (t1, n1), VariableDeclaration (t2, n2) ->
          t1 == t2 && n1 == n2
      | FunctionDeclaration (t1, n1), FunctionDeclaration (t2, n2) ->
          t1 == t2 && n1 == n2  
      | ParameterDeclaration (t1, n1, ta1), ParameterDeclaration (t2, n2, ta2) ->
          t1 == t2 && n1 == n2 && ta1 == ta2
      | RealPart (t1, e1), RealPart (t2, e2) ->
          t1 == t2 && e1 == e2
      | ImaginaryPart (t1, e1), ImaginaryPart (t2, e2) ->
          t1 == t2 && e1 == e2
      | PointerPlus (t1, r1, l1), PointerPlus (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | SSA (t1, n1, v1, e1), SSA (t2, n2, v2, e2) -> 
          t1 == t2 && n1 == n2 && v1 == v2 && e1 == e2
      | CallParameter (t1, n1, i1, e1), CallParameter (t2, n2, i2, e2) ->
          t1 == t2 && n1 == n2 && i1 == i2 && e1 == e2
      | Plus (t1, r1, l1), Plus (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | RealDiv (t1, r1, l1), RealDiv (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | Maximum (t1, r1, l1), Maximum (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | Minimum (t1, r1, l1), Minimum (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | Minus (t1, r1, l1), Minus (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | Times (t1, r1, l1), Times (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | ExactDiv (t1, r1, l1), ExactDiv (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | TruncatedDiv (t1, r1, l1), TruncatedDiv (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | TruncatedMod (t1, r1, l1), TruncatedMod (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BitOr (t1, r1, l1), BitOr (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BitAnd (t1, r1, l1), BitAnd (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BitXor (t1, r1, l1), BitXor (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BitLeftShift (t1, r1, l1), BitLeftShift (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BitRightShift (t1, r1, l1), BitRightShift (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BitLeftRotate (t1, r1, l1), BitLeftRotate (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BitRightRotate (t1, r1, l1), BitRightRotate (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BooleanEq (t1, r1, l1), BooleanEq (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BooleanOr (t1, r1, l1), BooleanOr (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BooleanAnd (t1, r1, l1), BooleanAnd (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BooleanNot (t1, e1), BooleanNot (t2, e2) ->
          t1 == t2 && e1 == e2
      | BooleanNeq (t1, r1, l1), BooleanNeq (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BooleanLessThan (t1, r1, l1), BooleanLessThan (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BooleanLessThanEq (t1, r1, l1), BooleanLessThanEq (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BooleanGreaterThan (t1, r1, l1), BooleanGreaterThan (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | BooleanGreaterThanEq (t1, r1, l1), BooleanGreaterThanEq (t2, r2, l2) ->
          t1 == t2 && r1 == r2 && l1 == l2
      | _ -> false 

      let hash e = Hashtbl.hash_param 100 100 e
    
      let rec prettify e = match e.Hashcons.node with
      | ResultDecl (t, s) -> "ResultDecl(" ^ s ^ ")"
      | Unsupported m -> "Unsupported(" ^ m ^ ")"
      | Nothing t -> "Nothing"
      | Call (t, n, p) -> String.concat "" [
          "Call("; 
          n; 
          ", ";
          String.concat ", " (Array.to_list @@ Array.map prettify p); 
          ")"
        ]
      | Cast (t, e) -> "Cast(" ^ (prettify e) ^ ")"
      | Constructor (t) -> "Constructor"
      | AddressOf (t, e) -> "AddressOf(" ^ (prettify e) ^ ")"
      | BitFieldRef (t, e, i1, i2) -> "BitFieldRef(" ^ (prettify e) ^ ", " ^ (string_of_int i1) ^ ", " ^ (string_of_int i2) ^ ")"
      | RealConstant (t, f) -> (string_of_float f)
      | StringConstant (t, _, s) -> s 
      | SIntegerConstant (t, i) -> (Z.to_string i)
      | UIntegerConstant (t, i) -> (Z.to_string i)
      | ArrayReference (t, a, i) -> (prettify a) ^ "[" ^ (prettify i) ^ "]"
      | MemoryReference (t, b, i) -> (prettify b) ^ " M+ " ^ (prettify i) ^ ")"
      | ComponentReference (t, b, f) -> (prettify b) ^ "." ^ (prettify f)
      | FieldDeclaration (t, d) -> "Field(" ^ (FieldDecl.name d)  ^ ")"
      | LabelDeclaration (t, n) -> "Label(" ^ n ^ ")"
      | ConstDeclaration (t, n) -> "Const(" ^ n ^ ")"
      | VariableDeclaration (t, d) -> "Variable(" ^ (VarDecl.name d) ^ ")"
      | FunctionDeclaration (t, n) -> "Function(" ^ n ^ ")"
      | ParameterDeclaration (t, n, _) -> "Parameter(" ^ n ^ ")"
      | RealPart (_, e) -> "Re(" ^ (prettify e) ^ ")"
      | ImaginaryPart (_, e) -> "Im(" ^ (prettify e) ^ ")"
      | PointerPlus (t, l, r) -> "(" ^ (prettify l) ^ " P+ " ^ (prettify r) ^ ")"
      | SSA (t, n, v, e) -> "SSA(" ^ n ^ ", " ^ (prettify e) ^ ")"
      | CallParameter (t, n, i, e) -> "Param(" ^ n ^ ", " ^ (prettify e) ^ ")"
      | Plus (t, l, r) -> "(" ^ (prettify l) ^ " + " ^ (prettify r) ^ ")"
      | RealDiv (t, l, r) -> "(" ^ (prettify l) ^ " R/ " ^ (prettify r) ^ ")"
      | Maximum (t, l, r) -> "(" ^ (prettify l) ^ " MIN " ^ (prettify r) ^ ")"
      | Minimum (t, l, r) -> "(" ^ (prettify l) ^ " MAX " ^ (prettify r) ^ ")"
      | Minus (t, l, r) -> "(" ^ (prettify l) ^ " - " ^ (prettify r) ^ ")"
      | Times (t, l, r) -> "(" ^ (prettify l) ^ " * " ^ (prettify r) ^ ")"
      | ExactDiv (t, l, r) -> "(" ^ (prettify l) ^ " E/ " ^ (prettify r) ^ ")"
      | TruncatedDiv (t, l, r) -> "(" ^ (prettify l) ^ " T/ " ^ (prettify r) ^ ")"
      | TruncatedMod (t, l, r) -> "(" ^ (prettify l) ^ " T% " ^ (prettify r) ^ ")"
      | BitOr (t, l, r) -> "(" ^ (prettify l) ^ " | " ^ (prettify r) ^ ")"
      | BitAnd (t, l, r) -> "(" ^ (prettify l) ^ " & " ^ (prettify r) ^ ")"
      | BitXor (t, l, r) -> "(" ^ (prettify l) ^ " XOR " ^ (prettify r) ^ ")"
      | BitLeftShift (t, l, r) -> "(" ^ (prettify l) ^ " << " ^ (prettify r) ^ ")"
      | BitRightShift (t, l, r) -> "(" ^ (prettify l) ^ " >> " ^ (prettify r) ^ ")"
      | BitLeftRotate (t, l, r) -> "(" ^ (prettify l) ^ " blr " ^ (prettify r) ^ ")"
      | BitRightRotate (t, l, r) -> "(" ^ (prettify l) ^ " brr " ^ (prettify r) ^ ")"
      | BooleanEq (t, l, r) -> "(" ^ (prettify l) ^ " == " ^ (prettify r) ^ ")"
      | BooleanOr (t, l, r) -> "(" ^ (prettify l) ^ " || " ^ (prettify r) ^ ")"
      | BooleanAnd (t, l, r) -> "(" ^ (prettify l) ^ " && " ^ (prettify r) ^ ")"
      | BooleanNot (t, e) ->  "!" ^ (prettify e)
      | BooleanNeq (t, l, r) -> "(" ^ (prettify l) ^ " != " ^ (prettify r) ^ ")"
      | BooleanLessThan (t, l, r) -> "(" ^ (prettify l) ^ " < " ^ (prettify r) ^ ")"
      | BooleanLessThanEq (t, l, r) -> "(" ^ (prettify l) ^ " <= " ^ (prettify r) ^ ")"
      | BooleanGreaterThan (t, l, r) -> "(" ^ (prettify l) ^ " > " ^ (prettify r) ^ ")"
      | BooleanGreaterThanEq (t, l, r) -> "(" ^ (prettify l) ^ " >= " ^ (prettify r) ^ ")"
      
      let typeof e = match e.Hashcons.node with
      | Unsupported _ -> GccType.none
      | ResultDecl (t, _) -> t
      | Nothing t -> t
      | Call (t, _, _) -> t 
      | Cast (t, _) -> t 
      | Constructor (t) -> t
      | AddressOf (t, _) -> t 
      | BitFieldRef (t, _, _, _) -> t 
      | RealConstant (t, _) -> t 
      | StringConstant (t, _, _) -> t 
      | SIntegerConstant (t, _) -> t 
      | UIntegerConstant (t, _) -> t 
      | ArrayReference (t, _, _) -> t 
      | MemoryReference (t, _, _) -> t 
      | ComponentReference (t, _, _) -> t 
      | FieldDeclaration (t, _) -> t 
      | LabelDeclaration (t, _) -> t 
      | ConstDeclaration (t, _) -> t
      | VariableDeclaration (t, _) -> t 
      | FunctionDeclaration (t, _) -> t 
      | ParameterDeclaration (t, _, _) -> t 
      | RealPart (t, _) -> t 
      | ImaginaryPart (t, _) -> t 
      | PointerPlus (t, _, _) -> t 
      | SSA (t, _, _, _) -> t 
      | CallParameter (t, _, _, _) -> t 
      | Plus (t, _, _) -> t 
      | RealDiv (t, _, _) -> t 
      | Maximum (t, _, _) -> t 
      | Minimum (t, _, _) -> t 
      | Minus (t, _, _) -> t 
      | Times (t, _, _) -> t 
      | ExactDiv (t, _, _) -> t 
      | TruncatedDiv (t, _, _) -> t 
      | TruncatedMod (t, _, _) -> t 
      | BitOr (t, _, _) -> t 
      | BitAnd (t, _, _) -> t 
      | BitXor (t, _, _) -> t 
      | BitLeftShift (t, _, _) -> t 
      | BitRightShift (t, _, _) -> t 
      | BitLeftRotate (t, _, _) -> t 
      | BitRightRotate (t, _, _) -> t 
      | BooleanEq (t, _, _) -> t 
      | BooleanOr (t, _, _) -> t 
      | BooleanAnd (t, _, _) -> t 
      | BooleanNot (t, _) -> t 
      | BooleanNeq (t, _, _) -> t 
      | BooleanLessThan (t, _, _) -> t 
      | BooleanLessThanEq (t, _, _) -> t 
      | BooleanGreaterThan (t, _, _) -> t 
      | BooleanGreaterThanEq (t, _, _) -> t 
    end

    type t = Impl.ths

    module Ht = Hashcons.Make(Impl)

    let ht = Ht.create 5077

    let unsupported m = Ht.hashcons ht @@ Impl.Unsupported m
    let nothing t = Ht.hashcons ht @@ Impl.Nothing t
    let call (t, n, ps) = Ht.hashcons ht @@ Impl.Call (t, n, ps)
    let cast (t, e) = Ht.hashcons ht @@ Impl.Cast (t, e)
    let constructor t = Ht.hashcons ht @@ Impl.Constructor t
    let address_of (t, e) = Ht.hashcons ht @@ Impl.AddressOf (t, e)
    let bitfield_ref (t, e, i1, i2) = Ht.hashcons ht @@ Impl.BitFieldRef (t, e, i1, i2)
    let real_cst (t, f) = Ht.hashcons ht @@ Impl.RealConstant (t, f)
    let string_cst (t, l, s) = Ht.hashcons ht @@ Impl.StringConstant (t, l, s)
    let s_int_cst (t, i) = Ht.hashcons ht @@ Impl.SIntegerConstant (t, i)
    let u_int_cst (t, i) = Ht.hashcons ht @@ Impl.UIntegerConstant (t, i)
    let array_ref (t, a, i) = Ht.hashcons ht @@ Impl.ArrayReference (t, a, i)
    let memory_ref (t, b, i) = Ht.hashcons ht @@ Impl.MemoryReference (t, b, i)
    let component_ref (t, b, f) = Ht.hashcons ht @@ Impl.ComponentReference (t, b, f)
    let field_decl (t, n) = Ht.hashcons ht @@ Impl.FieldDeclaration (t, n)
    let label_decl (t, n) = Ht.hashcons ht @@ Impl.LabelDeclaration (t, n)
    let const_decl (t, n) = Ht.hashcons ht @@ Impl.ConstDeclaration (t, n)
    let variable_decl (t, n) = Ht.hashcons ht @@ Impl.VariableDeclaration (t, n)
    let function_decl (t, n) = Ht.hashcons ht @@ Impl.FunctionDeclaration (t, n)
    let parameter_decl (t, n, ta) = Ht.hashcons ht @@ Impl.ParameterDeclaration (t, n, ta)
    let real_part (e) = Ht.hashcons ht @@ Impl.RealPart (Impl.typeof e, e)
    let imaginary_part (e) = Ht.hashcons ht @@ Impl.ImaginaryPart (Impl.typeof e, e)
    let pointer_plus (t, l, r) = Ht.hashcons ht @@ Impl.PointerPlus (t, l, r)
    let ssa (n, v, e) = Ht.hashcons ht @@ Impl.SSA (Impl.typeof e, n, v, e)
    let parameter (n, i, e) = Ht.hashcons ht @@ Impl.CallParameter (Impl.typeof e, n, i, e)
    let plus (t, l, r) = Ht.hashcons ht @@ Impl.Plus (t, l, r)
    let real_div (t, l, r) = Ht.hashcons ht @@ Impl.RealDiv (t, l, r)
    let maximum (t, l, r) = Ht.hashcons ht @@ Impl.Maximum (t, l, r)
    let minimum (t, l, r) = Ht.hashcons ht @@ Impl.Minimum (t, l, r)
    let minus (t, l, r) = Ht.hashcons ht @@ Impl.Minus (t, l, r)
    let times (t, l, r) = Ht.hashcons ht @@ Impl.Times (t, l, r)
    let exact_div (t, l, r) = Ht.hashcons ht @@ Impl.ExactDiv (t, l, r)
    let truncated_div (t, l, r) = Ht.hashcons ht @@ Impl.TruncatedDiv (t, l, r)
    let truncated_mod (t, l, r) = Ht.hashcons ht @@ Impl.TruncatedMod (t, l, r)
    let bit_or (t, l, r) = Ht.hashcons ht @@ Impl.BitOr (t, l, r)
    let bit_and (t, l, r) = Ht.hashcons ht @@ Impl.BitAnd (t, l, r)
    let bit_xor (t, l, r) = Ht.hashcons ht @@ Impl.BitXor (t, l, r)
    let left_shift (t, l, r) = Ht.hashcons ht @@ Impl.BitLeftShift (t, l, r)
    let right_shift (t, l, r) = Ht.hashcons ht @@ Impl.BitRightShift (t, l, r)
    let rotate_left (t, l, r) = Ht.hashcons ht @@ Impl.BitLeftRotate (t, l, r)
    let rotate_right (t, l, r) = Ht.hashcons ht @@ Impl.BitRightRotate (t, l, r)
    let beq (t, l, r) = Ht.hashcons ht @@ Impl.BooleanEq (t, l, r)
    let bor (t, l, r) = Ht.hashcons ht @@ Impl.BooleanOr (t, l, r)
    let band (t, l, r) = Ht.hashcons ht @@ Impl.BooleanAnd (t, l, r)
    let bnot (t, e) = Ht.hashcons ht @@ Impl.BooleanNot (t, e)
    let bneq (t, l, r) = Ht.hashcons ht @@ Impl.BooleanNeq (t, l, r)
    let blt (t, l, r) = Ht.hashcons ht @@ Impl.BooleanLessThan (t, l, r)
    let blte (t, l, r) = Ht.hashcons ht @@ Impl.BooleanLessThanEq (t, l, r)
    let bgt (t, l, r) = Ht.hashcons ht @@ Impl.BooleanGreaterThan (t, l, r)
    let bgte (t, l, r) = Ht.hashcons ht @@ Impl.BooleanGreaterThanEq (t, l, r)
    let inrange (t, a, low, high) = band(t, bgte(t, a, low), blte(t, a, high))
    let resultdecl (t, s) = Ht.hashcons ht @@ Impl.ResultDecl (t, s)

    let unordered (t, l, r) = unsupported "Unordered Operation"
    let ordered (t, l, r) = unsupported "Unordered Operation"
    let bungt (t, l, r) = unsupported "Unordered Operation"
    let bungte (t, l, r) = unsupported "Unordered Operation"
    let bunlt (t, l, r) = unsupported "Unordered Operation"
    let bunlte (t, l, r) = unsupported "Unordered Operation"
    let buneq (t, l, r) = unsupported "Unordered Operation"

    let unordered_inv (t, l, r) = unsupported "Unordered Operation"
    let ordered_inv (t, l, r) = unsupported "Unordered Operation"
    let bungt_inv (t, l, r) = unsupported "Unordered Operation"
    let bungte_inv (t, l, r) = unsupported "Unordered Operation"
    let bunlt_inv (t, l, r) = unsupported "Unordered Operation"
    let bunlte_inv (t, l, r) = unsupported "Unordered Operation"
    let buneq_inv (t, l, r) = unsupported "Unordered Operation"

    let prettify e = Impl.prettify e

    let rec substitute e get = match e.Hashcons.node with 
    | Impl.Call (t, n, arr) -> call (t, n, Array.map (fun a -> substitute a get) arr)
    | Impl.Cast (t, e) -> cast (t, substitute e get)
    | Impl.AddressOf (t, e) -> address_of (t, substitute e get)
    | Impl.BitFieldRef (t, e, i1, i2) -> bitfield_ref (t, substitute e get, i1, i2)
    | Impl.ArrayReference (t, e1, e2) -> array_ref (t, substitute e1 get, substitute e2 get)
    | Impl.MemoryReference (t, e1, e2) -> memory_ref (t, substitute e1 get, substitute e2 get)
    | Impl.ComponentReference (t, e1, e2) -> component_ref (t, substitute e1 get, substitute e2 get)
    | Impl.FieldDeclaration (_, _) -> get e 
    | Impl.LabelDeclaration (_, _) -> get e
    | Impl.ConstDeclaration (_, _) -> get e 
    | Impl.VariableDeclaration (_, _) -> get e 
    | Impl.FunctionDeclaration (_, _) -> get e 
    | Impl.ParameterDeclaration (_, _, _) -> get e
    | Impl.RealPart (t, e) -> real_part (substitute e get) 
    | Impl.ImaginaryPart (t, e) -> imaginary_part (substitute e get) 
    | Impl.PointerPlus (t, e1, e2) -> pointer_plus (t, substitute e1 get, substitute e2 get) 
    | Impl.SSA (_, _, _, _) -> get e
    | Impl.CallParameter (t, n, i, e) -> parameter (n, i, substitute e get)
    | Impl.Plus (t, e1, e2) -> plus (t, substitute e1 get, substitute e2 get)
    | Impl.RealDiv (t, e1, e2) -> real_div (t, substitute e1 get, substitute e2 get)
    | Impl.Maximum (t, e1, e2) -> maximum (t, substitute e1 get, substitute e2 get)
    | Impl.Minimum (t, e1, e2) -> minimum (t, substitute e1 get, substitute e2 get)
    | Impl.Minus (t, e1, e2) -> minus (t, substitute e1 get, substitute e2 get)
    | Impl.Times (t, e1, e2) -> times (t, substitute e1 get, substitute e2 get)
    | Impl.ExactDiv (t, e1, e2) -> exact_div (t, substitute e1 get, substitute e2 get)
    | Impl.TruncatedDiv (t, e1, e2) -> truncated_div (t, substitute e1 get, substitute e2 get)
    | Impl.TruncatedMod (t, e1, e2) -> truncated_mod (t, substitute e1 get, substitute e2 get)
    | Impl.BitOr (t, e1, e2) -> bit_or (t, substitute e1 get, substitute e2 get)
    | Impl.BitAnd (t, e1, e2) -> bit_and (t, substitute e1 get, substitute e2 get)
    | Impl.BitXor (t, e1, e2) -> bit_xor (t, substitute e1 get, substitute e2 get)
    | Impl.BitLeftShift (t, e1, e2) -> left_shift (t, substitute e1 get, substitute e2 get)
    | Impl.BitRightShift (t, e1, e2) -> right_shift (t, substitute e1 get, substitute e2 get)
    | Impl.BitLeftRotate (t, e1, e2) -> rotate_left (t, substitute e1 get, substitute e2 get)
    | Impl.BitRightRotate (t, e1, e2) -> rotate_right (t, substitute e1 get, substitute e2 get)
    | Impl.BooleanEq (t, e1, e2) -> beq (t, substitute e1 get, substitute e2 get)
    | Impl.BooleanOr (t, e1, e2) -> bor (t, substitute e1 get, substitute e2 get)
    | Impl.BooleanAnd (t, e1, e2) -> band (t, substitute e1 get, substitute e2 get)
    | Impl.BooleanNot (t, e1) -> bnot (t, substitute e1 get)
    | Impl.BooleanNeq (t, e1, e2) -> bneq (t, substitute e1 get, substitute e2 get)
    | Impl.BooleanLessThan (t, e1, e2) -> blt (t, substitute e1 get, substitute e2 get)
    | Impl.BooleanLessThanEq (t, e1, e2) -> blte (t, substitute e1 get, substitute e2 get)
    | Impl.BooleanGreaterThan (t, e1, e2) -> bgt (t, substitute e1 get, substitute e2 get)
    | Impl.BooleanGreaterThanEq (t, e1, e2) -> bgte (t, substitute e1 get, substitute e2 get)
    | _ -> e (* Anything without nested-subexprs is just identity transform *) 
end