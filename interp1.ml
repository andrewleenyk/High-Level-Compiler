#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
type const = Boolean of bool 
   | Digit of int 
   | Unit

type com = Push of const 
   | Pop 
   | Trace          
   | Add 
   | Sub 
   | Mul 
   | Div 
   | And 
   | Or 
   | Not         
   | Lt 
   | Gt
type coms = Coms of com * coms | Empty
let whitespaceParse: unit parser = (many whitespace) >| ();;

(* helper function to parse constants *)
let parse_constant keyword_parser constructor = 
  let* _ = keyword_parser in
  pure constructor
;;
let intParser: const parser =
  (let* _ = char '-' in
   let* num = natural in pure(Digit(-num)))
  <|>
  (let* num = natural in pure(Digit(num)));;

let boolParser: const parser =
  parse_constant (keyword "True") (Boolean true)
  <|>
  parse_constant (keyword "False") (Boolean false);;

let unitParser: const parser = 
  parse_constant (keyword "Unit") Unit;;

let constParser: const parser =
  intParser <|> boolParser <|> unitParser;;

(* a helper to parse the commans *)
let parse_command command_keyword command_constructor = 
   (let* _ = whitespaceParse in
   let* _ = keyword command_keyword in
   let* _ = whitespaceParse in
   pure(command_constructor))
;;

let com: com parser = 
(let* _ = whitespaceParse in
let* _ = keyword "Push" in
let* _ = whitespaceParse in
let* x = constParser in
let* _ = whitespaceParse in
pure(Push x))
   <|> parse_command "Pop" Pop
   <|> parse_command "Trace" Trace
   <|> parse_command "Add" Add
   <|> parse_command "Sub" Sub
   <|> parse_command "Mul" Mul
   <|> parse_command "Div" Div
   <|> parse_command "And" And
   <|> parse_command "Or" Or
   <|> parse_command "Not" Not
   <|> parse_command "Gt" Gt
   <|> parse_command "Lt" Lt
;;
let rec coms(): coms parser =
   (let* _ = whitespaceParse in
    let* parsed_com = com in
    let* _ = whitespaceParse in
    let* _ = char ';' in
    let* parsed_coms = coms() in
    pure (Coms(parsed_com, parsed_coms)))
   <|>
   (let* _ = whitespaceParse in
    pure(Empty))
;;

let constToString(c: const): string = 
  match c with
  | Boolean boolean -> if boolean then "True" else "False"
  | Digit num -> string_of_int num
  | Unit -> "Unit";;

  let rec eval (x: coms) (stack: const list) (trace: string list): string list =
   let eval_binary_operation stack op res trace =
     match stack with
     | Digit i :: Digit j :: stack -> eval res (op i j :: stack) trace
     | _ -> "Panic" :: trace
   in
   let eval_binary_boolean_operation stack op res trace =
     match stack with
     | Boolean a :: Boolean b :: stack -> eval res (Boolean (op a b) :: stack) trace
     | _ -> "Panic" :: trace
   in
   let eval_unary_boolean_operation stack op res trace =
     match stack with
     | Boolean a :: stack -> eval res (Boolean (op a) :: stack) trace
     | _ -> "Panic" :: trace
   in
   match x with
   | Coms (com, res) ->
     (match com with
      | Push y -> eval res (y :: stack) trace
      | Pop -> (match stack with | _ :: stack -> eval res stack trace | [] -> "Panic" :: trace)
      | Trace -> (match stack with | hd :: stack -> eval res (Unit :: stack) (constToString hd :: trace) | [] -> "Panic" :: trace)
      | Add -> eval_binary_operation stack (fun i j -> Digit (i + j)) res trace
      | Sub -> eval_binary_operation stack (fun i j -> Digit (i - j)) res trace
      | Mul -> eval_binary_operation stack (fun i j -> Digit (i * j)) res trace
      | Div -> (match stack with | Digit i :: Digit j :: stack -> if j == 0 then "Panic" :: trace else eval res (Digit (i / j) :: stack) trace | _ -> "Panic" :: trace)
      | And -> eval_binary_boolean_operation stack (&&) res trace
      | Or -> eval_binary_boolean_operation stack (||) res trace
      | Not -> eval_unary_boolean_operation stack not res trace
      | Lt -> eval_binary_operation stack (fun i j -> Boolean (i < j)) res trace
      | Gt -> eval_binary_operation stack (fun i j -> Boolean (i > j)) res trace)
   | Empty -> trace
;;
 
let explode str = List.of_seq (String.to_seq str);;
let parse (p : 'a parser) (str : string) : ('a * char list) option = p (explode str);;

let interp (str : string) : string list option = 
   let parsed = parse(coms())(str) in
   match parsed with
   | Some (x, []) -> Some(eval(x)([])([]))
   | _ -> None
 ;;
