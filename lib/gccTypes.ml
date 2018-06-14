module FieldDecl =
  struct
    module Impl = 
      struct type t = Field of string * string * int * string * int * int * bool

      let equal a b = match (a, b) with
      | Field (n1, s1, a1, o1, oa1, bo1, ib1), 
        Field (n2, s2, a2, o2, oa2, bo2, ib2) ->
          n1 == n2 && s1 == s2 && a1 == a2 && o1 == o2 && 
          oa1 == oa2 && bo1 == bo2 && ib1 == ib2

      let hash t = Hashtbl.hash t 

      let name f = match f with 
      | Field (n, _, _, _, _, _, _) -> n
    end

    type t = Impl.t Hashcons.hash_consed

    module Ht = Hashcons.Make(Impl)

    let ht = Ht.create 5077

    let name (x) = Impl.name x.Hashcons.node

    let make (n, s, a, o, oa, bo, ib) = Ht.hashcons ht @@ Impl.Field (n, s, a, o, oa, bo, ib)
end 

module VarDecl =
  struct 
    module Impl = 
      struct type t = Var of string * int * int

      let hash t = Hashtbl.hash t 

      let name v = match v with 
      | Var (n, _, _) -> n

      let equal a b = match (a, b) with 
      | Var (n1, s1, a1), Var (n2, s2, a2) ->
          n1 == n2 && s1 == s2 && a1 == a2
  end

  type t = Impl.t Hashcons.hash_consed 

  module Ht = Hashcons.Make(Impl)

  let ht = Ht.create 5077

  let name (x) = Impl.name x.Hashcons.node

  let make (n, s, a) = Ht.hashcons ht @@ Impl.Var (n, s, a)
end 

module GccType =
  struct 
    module Impl =
      struct 
        type ths = t Hashcons.hash_consed and t =
        | NoType
        | VoidType
        | SelfType
        | BooleanType           
        | NullptrType
        | PointerType           of ths
        | ReferenceType         of ths
        | ComplexType           of ths
        | OffsetType            of ths * ths
        | ArrayType             of ths * ths
        | RecordType            of string * ((VarDecl.t * ths) array) * ((FieldDecl.t * ths) array) * ((string * ths) array) * ((string * ths * int) array)
        | UnionType             of string * ((VarDecl.t * ths) array) * ((FieldDecl.t * ths) array) * ((string * ths) array) * ((string * ths * int) array)
        | VectorType            of ths * int
        | FunctionType          of string * ths * (ths array) * bool
        | MethodType            of string * ths * ths * (ths array) * bool
        | RealType              of int
        | IntegerType           of bool * int * int * Z.t * Z.t
        | EnumeralType          of bool * int * int * int * (string * int) array
        | FixedPointType        of bool * bool * int * int * int * int 
        | PointerBoundsType     of int
        | UnrepresentableType   of string

      let equal a b = match (a, b) with
      | NoType, NoType -> true
      | VoidType, VoidType -> true 
      | SelfType, SelfType -> true 
      | BooleanType, BooleanType -> true
      | NullptrType, NullptrType -> true 
      | PointerType c, PointerType d -> c == d
      | ReferenceType c, ReferenceType d -> c == d
      | ComplexType c, ComplexType d -> c == d
      | OffsetType (b1, o1), OffsetType(b2, o2) -> b1 == b2 && o1 == o2
      | ArrayType (b1, o1), ArrayType(b2, o2) -> b1 == b2 && o1 == o2
      | RecordType (n1, v1, f1, t1, c1), RecordType (n2, v2, f2, t2, c2) -> 
          n1 == n2 && v1 == v2 && f1 == f2 && t1 == t2 && c1 == c2
      | UnionType (n1, v1, f1, t1, c1), UnionType (n2, v2, f2, t2, c2) -> 
          n1 == n2 && v1 == v2 && f1 == f2 && t1 == t2 && c1 == c2
      | VectorType (b1, o1), VectorType(b2, o2) -> b1 == b2 && o1 == o2
      | FunctionType (n1, r1, p1, v1), FunctionType (n2, r2, p2, v2) -> 
          n1 == n2 && r1 == r2 && p1 == p2 && v1 == v2
      | MethodType (n1, r1, t1, p1, v1), MethodType (n2, r2, t2, p2, v2) -> 
          n1 == n2 && r1 == r2 && t1 == t2 && p1 == p2 && v1 == v2
      | RealType c, RealType d -> c == d 
      | IntegerType (s1, p1, z1, mn1, mx1), IntegerType (s2, p2, z2, mn2, mx2) ->
          s1 == s2 && p1 == p2 && z1 == z2 && mn1 == mn2 && mx1 == mx2
      | EnumeralType (s1, p1, mn1, mx1, v1), EnumeralType (s2, p2, mn2, mx2, v2) ->
          s1 == s2 && p1 == p2 && mn1 == mn2 && mx1 == mx2 && v1 == v2
      | FixedPointType (s1, r1, p1, z1, i1, f1), FixedPointType (s2, r2, p2, z2, i2, f2) ->
          s1 == s2 && r1 == r2 && p1 == p2 && z1 == z2 && i1 == i2 && f1 == f2
      | PointerBoundsType c, PointerBoundsType d -> c == d 
      | UnrepresentableType c, UnrepresentableType d -> c == d
      | _ -> false 

      let hash t = Hashtbl.hash_param 100 100 t

      let prettify_int t = match t.Hashcons.node with
      | IntegerType (false, 8, _, _, _)   -> "uint8_t"
      | IntegerType (true,  8, _, _, _)   -> "int8_t"
      | IntegerType (false, 16, _, _, _)  -> "uint16_t"
      | IntegerType (true,  16, _, _, _)  -> "int16_t"
      | IntegerType (false, 32, _, _, _)  -> "uint32_t"
      | IntegerType (true,  32, _, _, _)  -> "int32_t"
      | IntegerType (false, 64, _, _, _)  -> "uint64_t"
      | IntegerType (true,  64, _, _, _)  -> "int64_t"
      | IntegerType (false, 128, _, _, _) -> "uint128_t"
      | IntegerType (true,  128, _, _, _) -> "int128_t"
      | IntegerType (s,p,_,_,_) -> 
          "int(" ^ (string_of_bool s) ^ ", " ^ (string_of_int p) ^ ")"
      | _ -> "???"

      let rec prettify t = match t.Hashcons.node with
      | NoType      -> "notype"
      | VoidType    -> "void"
      | SelfType    -> "self"
      | BooleanType -> "bool"
      | NullptrType -> "nullptr"
      | IntegerType (_, _, _, _, _) -> (prettify_int t)
      | PointerType x               ->  "(" ^ (prettify x) ^ ")*"
      | ReferenceType x             -> "&(" ^ (prettify x) ^ ")"
      | RealType p                  -> "real{" ^ (string_of_int p) ^ "}"
      | ComplexType x               -> "complex{" ^ (prettify x) ^ "}"
      | PointerBoundsType p         -> "pbounds{" ^ (string_of_int p) ^ "}"
      | UnrepresentableType m       -> "unrep{" ^ m ^ "}"
      | OffsetType (b,o) -> 
          "offset{" ^ (prettify b) ^ ", " ^ (prettify o) ^ "}"
      | ArrayType (b,i) -> 
          "array{"  ^ (prettify b) ^ ", " ^ (prettify i) ^ "}"
      | RecordType (n, v, f, t, c) ->
          "struct{" ^ n ^ "}"
      | UnionType (n, v, f, t, c) ->
          "union{" ^ n ^ "}"
      | VectorType (b,l) -> 
          "vector{" ^ (prettify b) ^ ", " ^ (string_of_int l) ^ "}"
      | FunctionType (n,r,p,v) -> 
          (prettify r) ^ "{*" ^ n ^ "}{...}"
      | MethodType (n,r,t,p,v) -> 
          (prettify r) ^ "{*" ^ (prettify t) ^ "::" ^ n ^  "}{...}"
      | EnumeralType (s,p,mn,mx,v) -> "enum(todo)"
      | FixedPointType (s,r,p,z,i,f) -> 
          "fixedp{" ^ (string_of_bool s) ^ ", " ^ (string_of_bool r) ^ ", " ^ (string_of_int p) ^ "}"

    end

    type t = Impl.ths

    module Ht = Hashcons.Make(Impl)

    let ht = Ht.create 5077

    let none = Ht.hashcons ht Impl.NoType
    let void = Ht.hashcons ht Impl.VoidType 
    let self = Ht.hashcons ht Impl.SelfType 
    let boolean = Ht.hashcons ht Impl.BooleanType
    let nullptr = Ht.hashcons ht Impl.NullptrType
    let pointer x = Ht.hashcons ht @@ Impl.PointerType x
    let reference x = Ht.hashcons ht @@ Impl.ReferenceType x
    let complex x = Ht.hashcons ht @@ Impl.ComplexType x
    let offset (b,o) = Ht.hashcons ht @@ Impl.OffsetType (b, o)
    let array (b,i) = Ht.hashcons ht @@ Impl.ArrayType (b, i)
    let record (n,v,f,t,c) = Ht.hashcons ht @@ Impl.RecordType (n, v, f, t, c)
    let union (n,v,f,t,c) = Ht.hashcons ht @@ Impl.UnionType (n, v, f, t, c)
    let vector (b,l) = Ht.hashcons ht @@ Impl.VectorType (b, l)
    let func (n,r,p,v) = Ht.hashcons ht @@ Impl.FunctionType (n, r, p, v)
    let methd (n,r,t,p,v) = Ht.hashcons ht @@ Impl.MethodType (n, r, t, p, v)
    let real p = Ht.hashcons ht @@ Impl.RealType p
    let integer (s,p,z,mn,mx) = Ht.hashcons ht @@ Impl.IntegerType (s, p, z, mn, mx)
    let enum (s,p,mn,mx,v) = Ht.hashcons ht @@ Impl.EnumeralType (s, p, mn, mx, v)
    let fixed_point (s,r,p,z,i,f) = Ht.hashcons ht @@ Impl.FixedPointType (s, r, p, z, i, f)
    let pointer_bounds p = Ht.hashcons ht @@ Impl.PointerBoundsType p
    let unrepresentable m = Ht.hashcons ht @@ Impl.UnrepresentableType m

    let prettify x = Impl.prettify x
end
