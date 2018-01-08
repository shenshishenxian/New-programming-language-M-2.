open Llvm
open Ast
open Sast
open Semant
module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)

let translate(globals, functions) =

    let context = L.global_context() in
    let the_module = L.create_module context "M2"

    and i32_t     = L.i32_type context
    and i1_t      = L.i1_type context
    and i8_t      = L.i8_type context
    and float_t   = L.double_type context
    and void_t    = L.void_type context
    and array_t   = L.array_type
    and pointer_t = L.pointer_type
    in

    let ltype_of_typ = function
          A.Int     -> i32_t
        | A.Float   -> float_t
        | A.Bool    -> i1_t
        | A.Void    -> void_t
        | A.String  -> pointer_t i8_t
        | A.Matrix(typ, rows, cols) ->
            let rows' = match rows with IntLit(s) -> s | _ -> raise(Failure"InvalidMatrixDimension") in
            let cols' = match cols with IntLit(s) -> s | _ -> raise(Failure"InvalidMatrixDimension") in
            (match typ with
                A.Int      -> array_t (array_t i32_t cols') rows'
                | A.Float  -> array_t (array_t float_t cols') rows'
                | _ -> raise(Failure"UnsupportedMatrixType"))
    in

    let ltype_of_datatype = function
        A.Datatype(p) -> ltype_of_typ p
    in

    let global_vars =
        let global_var m (t,n) =
            let init = L.const_int (ltype_of_datatype t) 0 in
                StringMap.add n (L.define_global n init the_module) m in
                    List.fold_left global_var StringMap.empty globals
    in

    let printf_t =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
    in
    let printf_func =
        L.declare_function "printf" printf_t the_module
    in

    let function_decls =
        let function_decl m fdecl =
            let name = fdecl.S.sfname
            and formal_types = Array.of_list
                (List.map (function A.(t,s) -> ltype_of_datatype t) fdecl.S.sformals) in
            let ftype =
                L.function_type (ltype_of_datatype fdecl.S.sreturn_type) formal_types in
                StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty functions
    in

    let build_function_body fdecl =

        let (the_function, _) = StringMap.find fdecl.S.sfname function_decls in

        (* Create an instruction builder *)
        let builder = L.builder_at_end context (L.entry_block the_function) in

        let int_format_str = L.build_global_stringptr "%d\t" "fmt" builder
        and float_format_str = L.build_global_stringptr "%f\t" "fmt" builder
        and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
        in

        let local_vars =
            let add_formal m (t, n) p = L.set_value_name n p;
            let local = L.build_alloca (ltype_of_datatype t) n builder in
            ignore (L.build_store p local builder);
            StringMap.add n local m
        in

        let add_local m (t, n) =
            let local_var = L.build_alloca (ltype_of_datatype t) n builder
            in StringMap.add n local_var m
        in

        let formals = List.fold_left2 add_formal StringMap.empty
            (List.map (function A.(t,n) -> (t,n)) fdecl.S.sformals) (Array.to_list (L.params the_function)) in
            List.fold_left add_local formals (List.map (function A.(t,n) -> (t,n)) fdecl.S.slocals)
        in

        let lookup n = try StringMap.find n local_vars
            with Not_found -> StringMap.find n global_vars
        in

        let build_matrix_access s rowIndex colIndex builder assigned =
            let ptr = L.build_gep (lookup s) [|L.const_int i32_t 0; rowIndex; colIndex|] s builder in
            if assigned then ptr else L.build_load ptr s builder
        in

        let rec expr builder = function
            S.SNumLit(SIntLit(i))      -> L.const_int i32_t i
            | S.SNumLit(SFloatLit(f))  -> L.const_float float_t f
            | S.SBoolLit b             -> L.const_int i1_t (if b then 1 else 0)
            | S.SStringLit s           -> L.build_global_stringptr s "tmp" builder
            | S.SNoexpr                -> L.const_int i32_t 0
            | S.SId (s, d)             -> L.build_load (lookup s) s builder
            | S.SAssign (se1, se2, d)  ->
                let se1' =
                    (match se1 with
                        S.SId(s,_) -> (lookup s)
                        | S.SMatrixAccess(s, i1, j1, d) ->
                            let i = expr builder i1 and j = expr builder j1 in
                                build_matrix_access s i j builder true
                        | _ -> raise(Failure"AssignLHSMustBeAssignable"))
                and se2' = expr builder se2 in
                ignore (L.build_store se2' se1' builder); se2'
            | S.SBinop (op1, op, op2, d)  ->
                let type1 = Sast.get_sexpr_type op1 in
                let type2 = Sast.get_sexpr_type op2 in
                let op1' = expr builder op1
                and op2' = expr builder op2 in

                let int_bops op op1' op2' =
                    match op with
                        A.Add     -> L.build_add op1' op2' "tmp" builder
                        | A.Sub     -> L.build_sub op1' op2' "tmp" builder
                        | A.Mult    -> L.build_mul op1' op2' "tmp" builder
                        | A.Div     -> L.build_sdiv op1' op2' "tmp" builder
                        | A.Equal   -> L.build_icmp L.Icmp.Eq op1' op2' "tmp" builder
                        | A.Neq     -> L.build_icmp L.Icmp.Ne op1' op2' "tmp" builder
                        | A.Less    -> L.build_icmp L.Icmp.Slt op1' op2' "tmp" builder
                        | A.Leq     -> L.build_icmp L.Icmp.Sle op1' op2' "tmp" builder
                        | A.Greater -> L.build_icmp L.Icmp.Sgt op1' op2' "tmp" builder
                        | A.Geq     -> L.build_icmp L.Icmp.Sge op1' op2' "tmp" builder
                        | A.And     -> L.build_and op1' op2' "tmp" builder
                        | A.Or      -> L.build_or op1' op2' "tmp" builder
                in

                let float_bops op op1' op2' =
                    match op with
                        A.Add       -> L.build_fadd op1' op2' "tmp" builder
                        | A.Sub       -> L.build_fsub op1' op2' "tmp" builder
                        | A.Mult      -> L.build_fmul op1' op2' "tmp" builder
                        | A.Div       -> L.build_fdiv op1' op2' "tmp" builder
                        | A.Equal     -> L.build_fcmp L.Fcmp.Oeq op1' op2' "tmp" builder
                        | A.Neq       -> L.build_fcmp L.Fcmp.One op1' op2' "tmp" builder
                        | A.Less      -> L.build_fcmp L.Fcmp.Olt op1' op2' "tmp" builder
                        | A.Leq       -> L.build_fcmp L.Fcmp.Ole op1' op2' "tmp" builder
                        | A.Greater   -> L.build_fcmp L.Fcmp.Ogt op1' op2' "tmp" builder
                        | A.Geq       -> L.build_fcmp L.Fcmp.Oge op1' op2' "tmp" builder
                        | _           -> raise(Failure"IllegalFloatBinop")
                in

                let bool_bops op op1' op2' =
                    match op with
                        | A.And   -> L.build_and op1' op2' "tmp" builder
                        | A.Or    -> L.build_or op1' op2' "tmp" builder
                        | _       -> raise(Failure"IllegalBoolBinop")
                in

                let matrix_bops mtype rDimension cDimension op op1 op2 =
                    let lhs_str = (match op1 with SId(s,_) -> s | _ -> "") in
                    let rhs_str = (match op2 with SId(s,_) -> s | _ -> "") in
                    let operator_type = match mtype with
                     "int" -> i32_t | "float" -> float_t | _ -> i32_t
                    in
                    let operator_type2 = match mtype with
                     "int" -> L.const_int | "float" -> L.const_int | _ -> L.const_int
                    in
                    let buildtype = match mtype with
                     "int" -> (match op with A.Add -> L.build_add | A.Sub -> L.build_sub | A.Mult -> L.build_mul |  _ -> raise(Failure "Invalid Matrix Binop"))
                    | "float" -> (match op with A.Add -> L.build_fadd | A.Sub -> L.build_fsub | A.Mult -> L.build_fmul | _ -> raise(Failure "Invalid Matrix Binop"))
                    | _ -> L.build_add
                    in
                    let buildtype2 = match mtype with
                     "int" -> L.build_add | "float" -> L.build_fadd | _ -> L.build_add
                    in
                            (match op with
                                A.Add  | A.Sub    ->
                                    let tmp_m = L.build_alloca (array_t (array_t operator_type cDimension) rDimension) "tmpmat" builder in
                                    for i=0 to (rDimension-1) do
                                        for j=0 to (cDimension-1) do
                                            let m1 = build_matrix_access lhs_str (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let m2 = build_matrix_access rhs_str (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let add_res = buildtype m1 m2 "tmp" builder in
                                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                            ignore(build_store add_res ld builder);
                                        done
                                    done;
                                    L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                | A.Mult    ->
                                    let first_typ = Sast.get_sexpr_type op1 in
                                    let tmp_m = L.build_alloca (array_t (array_t operator_type cDimension) rDimension) "tmpmat" builder in
                                    (match first_typ with
                                        Datatype(Int)| Datatype(Float) ->
                                            for i=0 to (rDimension-1) do
                                                for j=0 to (cDimension-1) do
                                                    let m2 = build_matrix_access rhs_str (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                                    let add_res = buildtype (build_load (lookup lhs_str) "tmp" builder) m2 "tmp" builder in
                                                    let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                                    ignore(build_store add_res ld builder);
                                                done
                                            done;
                                            L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                        
                                        | Datatype(Matrix(Int,r1,c1)) | Datatype(Matrix(Float,r1,c1)) ->
                                            let tmp_s = L.build_alloca operator_type "tmpsum" builder in
                                            let c1_i = (match c1 with IntLit(n) -> n | _ -> -1) in
                                            ignore(L.build_store (operator_type2 operator_type 0) tmp_s builder);
                                            for i=0 to (rDimension-1) do
                                                for j=0 to (cDimension-1) do
                                                    ignore(L.build_store (operator_type2 operator_type 0) tmp_s builder);
                                                    for k=0 to (c1_i-1) do
                                                        let m1 = build_matrix_access lhs_str (L.const_int i32_t i) (L.const_int i32_t k) builder false in
                                                        let m2 = build_matrix_access rhs_str (L.const_int i32_t k) (L.const_int i32_t j) builder false in
                                                        let mult_res = buildtype m1 m2 "tmp" builder in
                                                        ignore(L.build_store (buildtype2 mult_res (L.build_load tmp_s "addtmp" builder) "tmp" builder) tmp_s builder);
                                                    done;
                                                    let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                                    ignore(build_store (L.build_load tmp_s "restmp" builder) ld builder);
                                                done
                                            done;
                                            L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                        | _ -> L.const_int i32_t 0)
                                | _         -> raise(Failure "Invalid Matrix Binop"))
                in

                let cast operand1 operand2 type1 type2 =
                    match (type1, type2) with
                        (Datatype(Int), Datatype(Int))          ->  int_bops op operand1 operand2
                        | (Datatype(Float), Datatype(Float))    ->  float_bops op operand1 operand2
                        | (Datatype(Bool), Datatype(Bool))      ->  bool_bops op operand1 operand2
                        | (Datatype(Int), Datatype(Matrix(Int,r1,c2))) -> let rDimension = (match r1 with IntLit(n) -> n | _ -> -1)
                                and cDimension = (match c2 with IntLit(n) -> n | _ -> -1) in
                                    matrix_bops "int" rDimension cDimension op op1 op2  
                        | (Datatype(Float), Datatype(Matrix(Float,r1,c2))) ->
                            let rDimension = (match r1 with IntLit(n) -> n | _ -> -1)
                                and cDimension = (match c2 with IntLit(n) -> n | _ -> -1) in
                                    matrix_bops "float" rDimension cDimension op op1 op2  
                        | (Datatype(Matrix(Int,r1,c1)), Datatype(Matrix(Int,r2,c2))) ->
                            let rDimension = (match r1 with IntLit(n) -> n | _ -> -1)
                                and cDimension = (match c2 with IntLit(n) -> n | _ -> -1) in
                                    matrix_bops "int" rDimension cDimension op op1 op2  
                        | (Datatype(Matrix(Float,r1,c1)), Datatype(Matrix(Float,r2,c2))) ->
                            let rDimension = (match r1 with IntLit(n) -> n | _ -> -1)
                                and cDimension = (match c2 with IntLit(n) -> n | _ -> -1) in
                                    matrix_bops "float" rDimension cDimension op op1 op2  
                        | _                                 -> raise(Failure"IllegalCast")
                in
                cast op1' op2' type1 type2

            | S.SUnop(op, e, d)         ->
                let e' = expr builder e in 
                (match d with
                Datatype(Int) -> (match op with
                        A.Neg     -> L.build_neg e' "tmp" builder
                        | A.Inc   -> L.build_store (L.build_add e' (L.const_int i32_t 1) "tmp" builder) (lookup (match e with S.SId(s, d) -> s | _->raise(Failure"IncMustBeCalledOnID"))) builder
                        | A.Dec   -> L.build_store (L.build_sub e' (L.const_int i32_t 1) "tmp" builder) (lookup (match e with S.SId(s, d) -> s | _->raise(Failure"DecMustBeCalledOnID"))) builder
                        | _       -> raise(Failure"IllegalIntUnop"))
                | Datatype(Float) -> (match op with
                        A.Neg   -> L.build_fneg e' "tmp" builder
                        | _     -> raise(Failure"IllegalFloatUnop"))
                | Datatype(Bool) -> (match op with
                        A.Not   -> L.build_not e' "tmp" builder
                        | _       -> raise(Failure"IllegalBoolUnop"))
                | _ -> (raise(Failure"InvalidUnopType")))
            | S.SRows(r)                -> L.const_int i32_t r
            | S.SCols(c)                -> L.const_int i32_t c
            | S.STranspose(s,d)         ->
                (match d with
                    Datatype(Matrix(Int, c, r)) ->
                        let r_tr = (match c with IntLit(n) -> n | _ -> -1) in
                        let c_tr = (match r with IntLit(n) -> n | _ -> -1) in
                        let tmp_tr = L.build_alloca (array_t (array_t i32_t c_tr) r_tr) "tmpmat" builder in
                        for i=0 to (r_tr-1) do
                            for j=0 to (c_tr-1) do
                                let mtr = build_matrix_access s (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                let ld = L.build_gep tmp_tr [| L.const_int i32_t 0; L.const_int i32_t j; L.const_int i32_t i |] "tmpmat" builder in
                                ignore(build_store mtr ld builder);
                            done
                        done;
                        L.build_load (L.build_gep tmp_tr [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                    | _ -> const_int i32_t 0)
            | S.SSubMatrix(s, r1, r2, c1, c2, d) ->
                (* L.const_int i32_t 1 *)
                (let alloctype = match d with
                    Datatype(Matrix(Int, c, r)) -> i32_t | Datatype(Matrix(Float, c, r)) -> float_t| _ -> i32_t in
                    match d with Datatype(Matrix(Int, c, r))| Datatype(Matrix(Float, c, r)) ->
                        let r1' = (match r1 with SNumLit(SIntLit(n)) -> n | _ -> -1) in
                        let r2' = (match r2 with SNumLit(SIntLit(n)) -> n | _ -> -1) in
                        let c1' = (match c1 with SNumLit(SIntLit(n)) -> n | _ -> -1) in
                        let c2' = (match c2 with SNumLit(SIntLit(n)) -> n | _ -> -1) in
                        let tmp_tr = L.build_alloca (array_t (array_t alloctype (c2' - c1' + 1)) (r2' - r1' + 1)) "tmpmat" builder in
                        for i=r1' to (r2') do
                            for j=c1' to (c2') do
                                let mtr = build_matrix_access s (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                let ld = L.build_gep tmp_tr [| L.const_int i32_t 0; L.const_int i32_t (i - r1'); L.const_int i32_t (j - c1') |] "tmpmat" builder in
                                ignore(build_store mtr ld builder);
                            done
                        done;
                        L.build_load (L.build_gep tmp_tr [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                    | _ -> const_int i32_t 0)
            | S.STrace(s, d) -> 
                (let alloctype = match d with
                    Datatype(Matrix(Int, c, r)) -> i32_t 
                    | Datatype(Matrix(Float, c, r)) -> float_t
                    | _ -> i32_t 
                in
                let buildtype = match d with
                    Datatype(Matrix(Int, c, r)) -> L.build_add 
                    | Datatype(Matrix(Float, c, r)) -> L.build_fadd
                    | _ -> L.build_add 
                in
                match d with
                    Datatype(Matrix(Int, c, r)) 
                    | Datatype(Matrix(Float, c, r)) ->
                        let c_tr = (match c with IntLit(n) -> n | _ -> -1) in
                        let tmp_s = L.build_alloca alloctype "tmpsum" builder in
                        ignore(L.build_store (L.const_int i32_t 0) tmp_s builder);
                        for i=0 to (c_tr-1) do
                                let mult_res = build_matrix_access s (L.const_int i32_t i) (L.const_int i32_t i) builder false in
                                ignore(L.build_store (buildtype mult_res (L.build_load tmp_s "addtmp" builder) "tmp" builder) tmp_s builder);
                        done;
                        L.build_load tmp_s "restmp" builder
                    | _ -> raise(Failure "Cannot calculate trace!"))
            | S.SCall ("printStr", [e], d) ->
                L.build_call printf_func [| string_format_str ; (expr builder e) |] "printf" builder
            | S.SCall ("printInt", [e], d) ->
                L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
            | S.SCall ("printBool", [e], d) ->
                L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder    
            | S.SCall ("printFloat", [e], d) ->
                L.build_call printf_func [| float_format_str ; (expr builder e) |] "printf" builder
            | S.SCall (f, act, d) ->
                let (fdef, fdecl) = StringMap.find f function_decls in
                let actuals = List.rev (List.map (expr builder) (List.rev act)) in
                let result =
                    (match fdecl.S.sreturn_type with
                        A.Datatype(A.Void) -> ""
                        | _ -> f ^ "_result") in
                L.build_call fdef (Array.of_list actuals) result builder
            | S.SNull                   -> L.const_null i32_t
            | S.SMatrixAccess (s, se1, se2, d) ->
                let i = expr builder se1 and j = expr builder se2 in
                    (build_matrix_access s i j builder false)
            | S.SMatrixLit (sll, d) -> let numtype = match d with A.Datatype(A.Float) -> float_t
            | A.Datatype(A.Int) -> i32_t
            | _ -> i32_t
             in
                let flipped = List.map List.rev sll in
                let lists        = List.map (List.map (expr builder)) flipped in
                let listArray    = List.map Array.of_list lists in
                let listArray2 = List.rev (List.map (L.const_array numtype) listArray) in
                let arrayArray   = Array.of_list listArray2 in
                L.const_array (array_t numtype (List.length (List.hd sll))) arrayArray

        in

  
        let add_terminal builder f =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
                | None -> ignore (f builder)
        in

        let rec stmt builder = function
            S.SBlock sl -> List.fold_left stmt builder sl
            | S.SExpr e -> ignore (expr builder e); builder
            | S.SReturn e ->
                ignore(match fdecl.S.sreturn_type with
                    A.Datatype(A.Void)  -> L.build_ret_void builder
                    | _                 -> L.build_ret (expr builder e) builder); builder
            | S.SIf (predicate, then_stmt, else_stmt) ->
                let bool_val = expr builder predicate in
                let merge_bb = L.append_block context
                    "merge" the_function in
                let then_bb = L.append_block context
                    "then" the_function in
                add_terminal
                    (stmt (L.builder_at_end context then_bb) then_stmt)
                    (L.build_br merge_bb);
                let else_bb = L.append_block context
                    "else" the_function in
                add_terminal
                    (stmt (L.builder_at_end context else_bb) else_stmt)
                    (L.build_br merge_bb);
                ignore (L.build_cond_br bool_val then_bb else_bb builder);
                L.builder_at_end context merge_bb
            | S.SWhile (predicate, body) ->
                let pred_bb = L.append_block context
                    "while" the_function in
                ignore (L.build_br pred_bb builder);
                let body_bb = L.append_block context
                    "while_body" the_function in
                add_terminal (stmt (L.builder_at_end context body_bb) body)
                (L.build_br pred_bb);
                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = expr pred_builder predicate in
                let merge_bb = L.append_block context
                    "merge" the_function in
                ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
                L.builder_at_end context merge_bb
            | S.SFor (op1, op2, e3, body) -> stmt builder
                (S.SBlock [S.SExpr op1 ;
                    S.SWhile (op2, S.SBlock [body ;
                        S.SExpr e3]) ])
        in

        (* Build the code for each statement in the function *)
        let builder = stmt builder (S.SBlock fdecl.S.sbody) in

        (* Add a return if the last block falls off the end *)
        add_terminal builder
            (match fdecl.S.sreturn_type with
                A.Datatype(A.Void) -> L.build_ret_void;
                | t -> L.build_ret (L.const_int (ltype_of_datatype t) 0))
    in
    List.iter build_function_body functions;

    the_module (*returned as a module to whatever called this*)
