-- |
-- Data types for the intermediate simplified-Erlang AST
--
module Language.PureScript.CodeGen.Erl.AST where

import Prelude ()
import Prelude.Compat

import Data.Char(isAscii, isAlpha)

-- -- |
-- -- Data type for simplified Javascript expressions
-- --
data Erl
  -- |
  -- A numeric literal
  --
  = ENumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | EStringLiteral String
  -- |
  -- A char literal
  --
  | ECharLiteral Char
  -- |
  -- An atom literal (possibly qualified a:b)
  --
  | EAtomLiteral Atom
  -- |
  -- A unary operator application
  --
  | EUnary UnaryOperator Erl
  -- |
  -- A binary operator application
  --
  | EBinary BinaryOperator Erl Erl


  -- |
  -- Top-level function definition (over-simplified)
  --
  | EFunctionDef Atom [String] Erl

  -- TODO not really a separate form. and misused
  | EVarBind String Erl

  -- |
  -- A variable
  --
  | EVar String

  -- |
  -- A function reference f/1
  --
  | EFunRef Atom Int



  -- |
  -- A fun definition
  --
  | EFun (Maybe String) String Erl

  | EFunFull [(EFunBinder, Erl)]

  -- |
  -- Function application
  --
  | EApp Erl [Erl]

  -- |
  -- Block
  --
  | EBlock [Erl]

  -- |
  -- Tuple literal {a, 1, "C"}
  --
  | ETupleLiteral [Erl]

  | EComment String

  | EMapLiteral [(Atom, Erl)]

  | EMapPattern [(Atom, Erl)]

  | EMapUpdate Erl [(Atom,Erl)]

  | ECaseOf Erl [(EBinder, Erl)]

  | EArrayLiteral [Erl]

  deriving (Show, Read, Eq)

data EFunBinder
 = EFunBinder [Erl] (Maybe Guard)

   deriving (Show, Read, Eq)

data EBinder
  = EBinder Erl -- TODO split out literals?

  | EGuardedBinder Erl Guard
  -- | EVarBinder String

  deriving (Show, Read, Eq)

data Guard
  = Guard Erl
  deriving (Show, Read, Eq)
  -- -- |
  -- -- A boolean literal
  -- --
  -- | JSBooleanLiteral (Maybe SourceSpan) Bool

--   -- |
--   -- An array literal
--   --
--   | JSArrayLiteral (Maybe SourceSpan) [JS]
--   -- |
--   -- An array indexer expression
--   --
--   | JSIndexer (Maybe SourceSpan) JS JS
--   -- |
--   -- An object literal
--   --
--   | JSObjectLiteral (Maybe SourceSpan) [(String, JS)]
--   -- |
--   -- An object property accessor expression
--   --
--   | JSAccessor (Maybe SourceSpan) String JS
--   -- |
--   -- A function introduction (optional name, arguments, body)
--   --
--   | JSFunction (Maybe SourceSpan) (Maybe String) [String] JS
--   -- |
--   -- Function application
--   --
--   | JSApp (Maybe SourceSpan) JS [JS]
--   -- |
--   -- Variable
--   --
--   | JSVar (Maybe SourceSpan) String
--   -- |
--   -- Conditional expression
--   --
--   | JSConditional (Maybe SourceSpan) JS JS JS
--   -- |
--   -- A block of expressions in braces
--   --
--   | JSBlock (Maybe SourceSpan) [JS]
--   -- |
--   -- A variable introduction and optional initialization
--   --
--   | JSVariableIntroduction (Maybe SourceSpan) String (Maybe JS)
--   -- |
--   -- A variable assignment
--   --
--   | JSAssignment (Maybe SourceSpan) JS JS
--   -- |
--   -- While loop
--   --
--   | JSWhile (Maybe SourceSpan) JS JS
--   -- |
--   -- For loop
--   --
--   | JSFor (Maybe SourceSpan) String JS JS JS
--   -- |
--   -- ForIn loop
--   --
--   | JSForIn (Maybe SourceSpan) String JS JS
--   -- |
--   -- If-then-else statement
--   --
--   | JSIfElse (Maybe SourceSpan) JS JS (Maybe JS)
--   -- |
--   -- Return statement
--   --
--   | JSReturn (Maybe SourceSpan) JS
--   -- |
--   -- Throw statement
--   --
--   | JSThrow (Maybe SourceSpan) JS
--   -- |
--   -- Type-Of operator
--   --
--   | JSTypeOf (Maybe SourceSpan) JS
--   -- |
--   -- InstanceOf test
--   --
--   | JSInstanceOf (Maybe SourceSpan) JS JS
--   -- |
--   -- Labelled statement
--   --
--   | JSLabel (Maybe SourceSpan) String JS
--   -- |
--   -- Break statement
--   --
--   | JSBreak (Maybe SourceSpan) String
--   -- |
--   -- Continue statement
--   --
--   | JSContinue (Maybe SourceSpan) String
--   -- |
--   -- Raw Javascript (generated when parsing fails for an inline foreign import declaration)
--   --
--   | JSRaw (Maybe SourceSpan) String
--   -- |
--   -- Commented Javascript
--   --
--   | JSComment (Maybe SourceSpan) [Comment] JS deriving (Show, Read, Eq)


-- \ Possibly qualified atom
-- | TODO : This is not really an atom, each part is an atom.
data Atom = Atom (Maybe String) String deriving (Show, Eq, Read)
-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  deriving (Show, Read, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division (float)
  --
  | FDivide
  -- |
  -- Numeric division (integer)
  --
  | IDivide
  -- |
  -- Remainder
  --
  | Remainder



  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Generic identical test
  --
  | IdenticalTo
  -- |
  -- Generic non-identical test
  --
  | NotIdenticalTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo

  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Boolean short-circuit and
  --
  | AndAlso
  -- |
  -- Boolean short-circuit or
  --
  | OrElse
  -- |
  -- Boolean xor
  --
  | XOr
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  -- -- |
  -- -- Bitwise right shift with zero-fill
  -- --
  -- | ZeroFillShiftRight
  deriving (Show, Read, Eq)
