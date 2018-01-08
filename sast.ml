open Ast

type snum =
		SIntLit of int
	|	SFloatLit of float

(* Expressions *)
type sexpr =
	  SNumLit of snum
	| SBoolLit of bool
	| SStringLit of string
	| SMatrixLit of sexpr list list * datatype
	| SId of string * datatype
	| SNoexpr
	| SNull
	| SBinop of sexpr * op * sexpr * datatype
	| SUnop of uop * sexpr * datatype
	| SAssign of sexpr * sexpr * datatype
	| SCall of string * sexpr list * datatype
	| SMatrixAccess of string * sexpr * sexpr * datatype
	| SRows of int
	| SCols of int
	| STranspose of string * datatype
	| SSubMatrix of string * sexpr * sexpr * sexpr * sexpr * datatype
	| STrace of string * datatype
	(* | SMequal of string * string * datatype *)
	(* | SNorm1 of string * datatype *)

let get_sexpr_type sexpr = match sexpr with
	SNumLit(SIntLit(_))				-> Datatype(Int)
	| SNumLit(SFloatLit(_))				-> Datatype(Float)
	| SBoolLit(_)						-> Datatype(Bool)
	| SStringLit(_) 					-> Datatype(String)
	| SNoexpr 							-> Datatype(Void)
	| SNull								-> Datatype(Void)
	| SRows(r) 							-> Datatype(Int)
	| SCols(c) 							-> Datatype(Int)
	| STranspose(_,d) 					-> d
	| SId(_, d) 						-> d
	| SBinop(_, _, _, d) 				-> d
	| SAssign(_, _, d) 					-> d
	| SCall(_, _, d)					-> d
	| SUnop(_, _, d) 					-> d
	| SMatrixAccess(_, _, _, d)			-> d
	| SMatrixLit(smlist, d)				->
		let c = List.length (List.hd smlist) in
		let r = List.length smlist in
		(match d with
			Datatype(Int) 		-> Datatype(Matrix(Int, IntLit(r), IntLit(c)))
			| Datatype(Float)	-> Datatype(Matrix(Float, IntLit(r), IntLit(c)))
			| _ 				-> raise(Failure"UnsupportedMatrixType"))
	| SSubMatrix (_,_,_,_,_,d)  	-> d 
	| STrace(_,d) 					-> d

(* Statements *)
type sstmt =
	  SBlock of sstmt list
	| SExpr of sexpr
	| SIf of sexpr * sstmt * sstmt
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt
	| SReturn of sexpr

(* Function Declarations *)
type sfunc_decl = {
	sreturn_type 	: datatype;
	sfname 			: string;
	sformals 		: bind list;
	slocals  		: bind list;
	sbody 			: sstmt list;
}

(* All method declarations | Main entry method *)
type sprogram = bind list * sfunc_decl list
