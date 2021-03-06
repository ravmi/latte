

module AbsLatte where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = FnTopDef FnDef | ClDef Ident [CDeclare]
  deriving (Eq, Ord, Show, Read)

data FnDef = FnDef Type Ident [Arg] Block
  deriving (Eq, Ord, Show, Read)

data Arg = Arg Type Ident
  deriving (Eq, Ord, Show, Read)

data LeftEq = LVar Ident | LSDeref Ident Ident | LADeref Ident Expr
  deriving (Eq, Ord, Show, Read)

data Block = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass LeftEq Expr
    | Incr Ident
    | Decr Ident
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | For Type Ident Ident Stmt
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (Eq, Ord, Show, Read)

data Type
    = Int
    | Str
    | Bool
    | Void
    | CType Ident
    | AType Type
    | Fun Type [Type]
  deriving (Eq, Ord, Show, Read)

data Expr
    = ENull Type
    | EALength Ident
    | ENewOb Type
    | ENewAr Type Expr
    | EDerefA Ident Expr
    | EDerefS Ident Ident
    | EVar Ident
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)

data CDeclare = CDVar Type Ident
  deriving (Eq, Ord, Show, Read)

