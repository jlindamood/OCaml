(* A definition of arithmetic, relational, logical, and let
   expressions and their evaluation.  This language was originally
   defined in ``expr_let.ml``.

   Here we change the type of ``eval`` and introduce static type
   checking of expressions.

   Eric Van Wyk 
 *)

type value 
  = Int of int
  | Bool of bool

type expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id of string

type environment = (string * value) list

type typ = 
  | IntType
  | BoolType

type error =
  (* An unbound name error *)
  | UnboundName of string

  (* An incorrect type error.  The expr has a type (the second
     component) but one of the types in the ``typ list`` was
     expected. *)
  | IncorrectType of expr * typ * (typ list)

  | DivisionByZero of expr


type 'a result = OK of 'a
               | Err of error list

let rec lookup (n:string) (env: (string * 'a) list) : 'a result =
  match env with
  | [] -> Err ( [ UnboundName n ] )
  | (n',v) :: rest when n = n' -> OK v
  | _ :: rest -> lookup n rest


(* WHY DID YOU CHANGE THE ORDER OF ENV AND E HERE ? ? ? *)
let rec eval (e: expr) (env: environment): value result =
  match e with 
  | Val v -> OK v
  | Add (e1, e2) -> (match eval e1 env with
                    | Err x -> Err x
                    | OK v1 -> (match eval e2 env with
                              | Err x -> Err x
                              | OK v2 -> (match v1, v2 with
                                          | Int x, Int y -> OK (Int (x + y))
                                          | Int x, _ -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                                          | _, Int y -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                                          | Bool x, Bool y -> Err ( [ (IncorrectType (e1, BoolType, [IntType])) ] @ [ (IncorrectType (e2, BoolType, [IntType])) ] ) 
                                          
                                        )
                              )
                  )
  | Sub (e1, e2) -> (match eval e1 env with
                    | Err x -> Err x
                    | OK v1 -> (match eval e2 env with
                              | Err x -> Err x
                              | OK v2 -> (match v1, v2 with
                                          | Int x, Int y -> OK (Int (x - y))
                                          | Int x, _ -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                                          | _, Int y -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                                          | Bool x, Bool y -> Err ( [ (IncorrectType (e1, BoolType, [IntType])) ] @ [ (IncorrectType (e2, BoolType, [IntType])) ] )
                                          
                              ))
                  )
  | Mul (e1, e2) -> (match eval e1 env with
                    | Err x -> Err x
                    | OK v1 -> (match eval e2 env with
                              | Err x -> Err x
                              | OK v2 -> (match v1, v2 with
                                          | Int x, Int y -> OK (Int (x * y))
                                          | Int x, _ -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                                          | _, Int y -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                                          | Bool x, Bool y -> Err ( [ (IncorrectType (e1, BoolType, [IntType])) ] @ [ (IncorrectType (e2, BoolType, [IntType])) ] )
                                          
                              ))
                  )
  | Div (e1, e2) -> (match eval e1 env with
                    | Err x -> Err x
                    | OK v1 -> (match eval e2 env with
                              | Err x -> Err x
                              | OK v2 -> (match v1, v2 with
                                          | Int x, Int 0 -> Err ( [DivisionByZero e2] )
                                          | Int x, Int y -> OK (Int (x / y))
                                          | Int x, _ -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                                          
                                          | _, Int y -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                                          | Bool x, Bool y -> Err ( [ (IncorrectType (e1, BoolType, [IntType])) ] @ [ (IncorrectType (e2, BoolType, [IntType])) ] )
                                          
                              ))
                  )
  | Lt (e1, e2) -> (match eval e1 env with
                    | Err x -> Err x
                    | OK v1 -> (match eval e2 env with
                              | Err x -> Err x
                              | OK v2 -> (match v1, v2 with
                                          | Int x, Int y -> OK (Bool (x < y))
                                          | Int x, _ -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                                          | _, Int y -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                                          | Bool x, Bool y -> Err ( [ (IncorrectType (e1, BoolType, [IntType])) ] @ [ (IncorrectType (e2, BoolType, [IntType])) ] )
                                          
                              ))
                  )
  | Eq (e1, e2) -> (match eval e1 env with
                    | Err x -> Err x
                    | OK v1 -> (match eval e2 env with
                              | Err x -> Err x
                              | OK v2 -> (match v1, v2 with
                                          | Int x, Int y -> OK (Bool (x = y))
                                          | Int x, _ -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                                          | _, Int y -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                                          | Bool x, Bool y -> OK (Bool (x = y))
                                          
                              ))
                  )
  | And (e1, e2) -> (match eval e1 env with
                    | Err x -> Err x
                    | OK v1 -> (match eval e2 env with
                              | Err x -> Err x
                              | OK v2 -> (match v1, v2 with
                                          | Int x, Int y -> Err ( [ (IncorrectType (e1, IntType, [BoolType])) ] @ [ (IncorrectType (e2, IntType, [BoolType])) ] )
                                          | Int x, _ -> Err ( [ IncorrectType (e1, IntType, [BoolType])])
                                          | _, Int y -> Err ( [ IncorrectType (e2, IntType, [BoolType])])
                                          | Bool x, Bool y -> OK (Bool (x && y))
                                          
                              ))
                  )
  | Not (e1) -> (match eval e1 env with
                    | Err x -> Err x
                    | OK v1 -> (match v1 with
                              | Bool x -> OK (Bool (not x))
                              | Int x -> Err ( [IncorrectType (e1, IntType, [BoolType])] )
                              
                            )
                  )
  | Let (n, dexpr, body) ->
      (match eval dexpr env with
      | OK v -> eval body ((n, v)::env)
      | Err x -> Err x
    )
  | Id n -> lookup n env

(* A helper function to start evaluation with the empty environment. *)
let evaluate e = eval e []

let rec serialize expr =
  match expr with
  | Val v -> (match v with
            | Int x -> "Val (Int " ^ string_of_int x ^ ")"
            | Bool x -> "Val (Bool " ^ string_of_bool x ^ ")"
          )
  | Add (e1, e2) -> "Add " ^ "(" ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Sub (e1, e2) -> "Sub " ^ "(" ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Mul (e1, e2) -> "Mul " ^ "(" ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Div (e1, e2) -> "Div " ^ "(" ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Lt  (e1, e2) -> "Lt " ^ "(" ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Eq  (e1, e2) -> "Eq " ^ "(" ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | And (e1, e2) -> "And " ^ "(" ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Not (e1) -> "Not " ^ "(" ^ serialize e1 ^ ")"
  | Let (n, dexpr, body) ->
      "Let (\"" ^ n ^ "\", " ^ serialize dexpr ^ ", " ^ serialize body ^ ")"
  | Id n -> "Id " ^ "\"" ^ n ^ "\"" 

let rec unparse expr =
  match expr with
  | Val v -> (match v with
            | Int x -> string_of_int x
            | Bool x -> string_of_bool x
          )
  | Add (e1, e2) -> "(" ^ unparse e1 ^ " + " ^ unparse e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ unparse e1 ^ " - " ^ unparse e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ unparse e1 ^ " * " ^ unparse e2 ^ ")"
  | Div (e1, e2) -> "(" ^ unparse e1 ^ " / " ^ unparse e2 ^ ")"
  | Lt  (e1, e2) -> "(" ^ unparse e1 ^ " < " ^ unparse e2 ^ ")"
  | Eq  (e1, e2) -> "(" ^ unparse e1 ^ " = " ^ unparse e2 ^ ")"
  | And (e1, e2) -> "(" ^ unparse e1 ^ " && " ^ unparse e2 ^ ")"
  | Not (e1) -> "(" ^ "not " ^ unparse e1 ^ ")"
  | Let (n, dexpr, body) ->
      "(let " ^ n ^ " = " ^ unparse dexpr ^ " in " ^ unparse body ^ ")"
  | Id n -> n


(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = eval e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = eval e2

let e3 = Eq (e1, e2)
let e4 = Lt (e1, e2)

let e5 = Not e4

(* ``let y = 5 in let x = y + 5 in x + y'' *)
let e6 = Let ("y", 
              Val (Int 5), 
              Let ("x", 
                   Add (Id "y", Val (Int 5)), 
                   Add (Id "x", Id "y")
                  )
             )

(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

(* Assert expressions to test the evaluate function. *)
let () =
  assert (evaluate e1 = OK (Int 7));
  assert (evaluate e2 = OK (Int 7));
  assert (evaluate e3 = OK (Bool true));
  assert (evaluate e4 = OK (Bool false));
  assert (evaluate e5 = OK (Bool true));
  assert (evaluate e6 = OK (Int 15));
  assert (evaluate e7 = OK (Bool true))


let er1 = Add (Val (Int 1), Mul (Val (Bool true), Val (Int 3)))
let er2 = Eq (Val (Bool true), Val (Int 3))
let er3 = Eq (e1, e4)

let er4 = Let ("y", 
               Val (Int 5), 
               And (Val (Bool true), Id "y")
              )

let er5 = And (Val (Bool true), Id "y")

let er6 = Let ("y", 
               Val (Int 0), 
               Div (Val (Int 5), Id "y")
              )

let er7 = Let ("x", 
              Add (Val (Int 5), Val (Bool true)),
              Add (Id "x", Val (Int 5))
              )

let has_eval_errors (e:expr) : bool =
  match evaluate e with
  | OK _ -> false
  | Err _ -> true

let () =
  assert (has_eval_errors er1);
  assert (has_eval_errors er2);
  assert (has_eval_errors er3);
  assert (has_eval_errors er4);
  assert (has_eval_errors er5);
  assert (has_eval_errors er6);
  assert (has_eval_errors er7)


(* To check the type correctness of expressions by infering their
   type, we use the following data types. *)

type context = (string * typ) list

let rec contextlookup (n:string) (ctxt: (string * typ) list) : typ result =
  match ctxt with
  | [] -> Err ( [ UnboundName n ] )
  | (n',v) :: rest when n = n' -> (match v with
                                  | _ -> OK v
                                )
  | _ :: rest -> contextlookup n rest

let rec check (e:expr) (ctxt:context) : typ result =
  match e with 
  | Val (Int _) -> OK IntType
  | Val (Bool _) -> OK BoolType
  | Add (e1, e2) -> (match check e1 ctxt with
                    | Err x -> Err x
                    | OK BoolType -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                    | OK IntType -> (match check e2 ctxt with
                            | Err y -> Err y
                            | OK BoolType -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                            | OK IntType -> OK IntType
                          )
                  )
  | Sub (e1, e2) -> (match check e1 ctxt with
                    | Err x -> Err x
                    | OK BoolType -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                    | OK IntType -> (match check e2 ctxt with
                            | Err y -> Err y
                            | OK BoolType -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                            | OK IntType -> OK IntType
                          )
                  )
  | Mul (e1, e2) -> (match check e1 ctxt with
                    | Err x -> Err x
                    | OK BoolType -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                    | OK IntType -> (match check e2 ctxt with
                            | Err y -> Err y
                            | OK BoolType -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                            | OK IntType -> OK IntType
                          )
                  )
  | Div (e1, e2) -> (match check e1 ctxt with
                    | Err x -> Err x
                    | OK BoolType -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                    | OK IntType -> (match check e2 ctxt with
                            | Err y -> Err y
                            | OK BoolType -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                            | OK IntType -> OK IntType
                          )
                  )
  | Lt (e1, e2) -> (match check e1 ctxt with
                    | Err x -> Err x
                    | OK BoolType -> Err ( [ IncorrectType (e1, BoolType, [IntType])])
                    | OK IntType -> (match check e2 ctxt with
                            | Err y -> Err y
                            | OK BoolType -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                            | OK IntType -> OK IntType
                          )
                  )
  | Eq (e1, e2) -> (match check e1 ctxt with
                    | Err x -> Err x
                    | OK IntType -> (match check e2 ctxt with
                            | Err y -> Err y
                            | OK BoolType -> Err ( [ IncorrectType (e2, BoolType, [IntType])])
                            | OK IntType -> OK IntType
                          )
                    | OK BoolType -> (match check e2 ctxt with
                                      | Err y -> Err y
                                      | OK IntType -> Err ( [ IncorrectType (e2, IntType, [BoolType])])
                                      | OK BoolType -> OK BoolType
                                    )
                  )
  | And (e1, e2) -> (match check e1 ctxt with
                    | Err x -> Err x
                    | OK IntType -> Err ( [ IncorrectType (e1, IntType, [BoolType])])
                    | OK BoolType -> (match check e2 ctxt with
                            | Err y -> Err y
                            | OK IntType -> Err ( [ IncorrectType (e2, IntType, [BoolType])])
                            | OK BoolType -> OK BoolType
                          )
                  )
  | Not (e1) -> (match check e1 ctxt with
                    | Err x -> Err x
                    | OK IntType -> Err ( [ IncorrectType (e1, IntType, [BoolType])])
                    | OK BoolType -> OK BoolType
                  )
  | Let (n, dexpr, body) -> (match check dexpr ctxt with
                            | Err x -> Err x
                            | OK IntType -> check body ((n, IntType)::ctxt)
                            | OK BoolType -> check body ((n, BoolType)::ctxt)
                          )
  | Id n -> contextlookup n ctxt



let e8 = Div (Val (Int 5), Val (Int 0))

let has_type_errors (e:expr) : bool =
  match check e [] with
  | OK _ -> false
  | Err _ -> true

let () =
  assert (not (has_type_errors e8))

let () =
  assert (has_type_errors er1);
  assert (has_type_errors er2);
  (*assert (has_type_errors er3);*)
  assert (has_type_errors er4);
  assert (has_type_errors er5);
  (* er6 has not type error *)
  assert (has_type_errors er7)



let () =
  print_endline ("Success! All tests passed.")