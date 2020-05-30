type data_type_t =
  | IntType
  | FloatType
  | DoubleType
  | CharType
  | ArrayType of data_type_t * int
  | PointerType of data_type_t
  | StructType of string * (string * data_type_t) list
  | UnknownType
  | VoidType

(* Expression is further decomposed into factors and terms. Factor is the smallest group
 * consisting of constants, unary operator or grouped expressions.
 * Term is defined as multiple factors connected by / or *. 
 * Expression is multiple terms connected by + or - signs. *)
type expression_t =
  | ConstantIntExp of int
  | ConstantCharExp of char
  | ConstantStringExp of string
  | ConstantFloatExp of float
  | PreIncExp of expression_t
  | PreDecExp of expression_t
  | PostIncExp of expression_t
  | PostDecExp of expression_t
  | VarExp of string
  | ArrayIndexExp of expression_t * expression_t (* first expression_t must be of type array, second is index *)
  | StructMemberExp of expression_t * string (* first exp should be struct, second is member name *)
  | ArrowStructMemberExp of expression_t * string (* first exp should be pointer to struct, second is member name *)
  | DereferenceExp of expression_t
  | AddressOfExp of expression_t
  | GroupedExpression of expression_t
  | NegateOp of expression_t
  | LogicalNegateOp of expression_t
  | ComplementOp of expression_t
  | MultiExp of expression_t * expression_t
  | DivideExp of expression_t * expression_t
  | AdditionExp of expression_t * expression_t
  | MinusExp of expression_t * expression_t
  | LessExp of expression_t * expression_t
  | LessOrEqualExp of expression_t * expression_t
  | GreaterExp of expression_t * expression_t
  | GreaterOrEqualExp of expression_t * expression_t
  | EqualExp of expression_t * expression_t
  | NotEqualExp of expression_t * expression_t
  | OrExp of expression_t * expression_t
  | AndExp of expression_t * expression_t
  | AssignExp of expression_t * expression_t (* The first expression_t must be assignable. *)
  | ConditionExp of expression_t * expression_t * expression_t
  | FunctionCallExp of string * expression_t list

type statement_t =
  | ReturnStatement of expression_t
  | ExpressionStatement of expression_t option
  | ConditionalStatement of expression_t * statement_t * statement_t option
  | CompoundStatement of block_item_t list
  | ForStatement of
      expression_t option * expression_t * expression_t option * statement_t
  | ForDeclStatement of
      declare_t * expression_t * expression_t option * statement_t
  | WhileStatement of expression_t * statement_t
  | DoStatement of statement_t * expression_t
  | BreakStatement
  | ContinueStatement

and declare_t = DeclareStatement of data_type_t * string * expression_t option

and block_item_t = StatementItem of statement_t | DeclareItem of declare_t

type function_t =
  | IntFunction of string * (string * data_type_t) list * block_item_t list option

type global_item_t =
  | GlobalFunction of function_t
  | GlobalDef of data_type_t

type program_t = Program of global_item_t list

type static_data_t = DataInt of int | DataString of string

