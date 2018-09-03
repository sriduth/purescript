-- | Pretty printer for the JavaScript AST
module Language.PureScript.CodeGen.JS.Printer
  ( prettyPrintJS
  , prettyPrintJSWithSourceMaps
  ) where

import Prelude.Compat

import Control.Arrow ((<+>))
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
import qualified Control.Arrow as A

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.CoreImp.AST
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Pretty.Common
import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

import Debug.Trace (trace)
import Data.Maybe
-- TODO (Christoph): Get rid of T.unpack / pack

objectPropertyToString :: (Emit gen) => PSString -> gen
objectPropertyToString s =
  emit $ case decodeString s of
           Just s' | not (identNeedsEscaping s') ->
                       s'
           _ ->
             prettyPrintStringJS s

literals :: (Emit gen) => Pattern PrinterState AST gen
literals = mkPattern' match'
  where
  match' :: (Emit gen) => AST -> StateT PrinterState Maybe gen
  match' js = (addMapping' (getSourceSpan js) <>) <$> match js

  match :: (Emit gen) => AST -> StateT PrinterState Maybe gen
  match (NumericLiteral _ n) = return $ emit $ T.pack $ either show show n
  match (StringLiteral _ s) = return $ emit $ prettyPrintStringJS s
  match (BooleanLiteral _ True) = return $ emit "true"
  match (BooleanLiteral _ False) = return $ emit "false"
  match (ArrayLiteral _ xs) = mconcat <$> sequence
    [ return $ emit "[ "
    , intercalate (emit ", ") <$> forM xs prettyPrintJS'
    , return $ emit " ]"
    ]
  match (StructLiteral _ name []) = return $ emit ("%" <> name <> "{}")
  match (StructLiteral _ name ps) = mconcat <$> sequence
    [ return $ emit ("%" <> name <> "{\n")
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap (((emit "\"") <> objectPropertyToString key <> (emit "\"") <> emit ": ") <>) . prettyPrintJS' $ value
        indentString <- currentIndent
        return $ intercalate (emit ",\n") $ map (indentString <>) jss
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
  match (ObjectLiteral _ []) = return $ emit "{}"
  match (ObjectLiteral _ ps) = mconcat <$> sequence
    [ return $ emit "%{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap (((emit "\"") <> objectPropertyToString key <> (emit "\"") <> emit ": ") <>) . prettyPrintJS' $ value
        indentString <- currentIndent
        return $ intercalate (emit ",\n") $ map (indentString <>) jss
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]

-- [IfElse
--  Nothing
--  (Var Nothing "arg")
--  (Block Nothing [Return Nothing (NumericLiteral Nothing (Left 123))]) Nothing,
--  Return Nothing (NumericLiteral Nothing (Left 456))]

  match (FnBlock _ sts) =
    mconcat <$> sequence
    [ return $ emit "\n"
    , withIndent $ prettyStatements sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "end"]

-- recurseAndFixIfElse ife =
--   case ife of
--     [(IfElse a b c d), rst] -> [IfElseBlock a b c (Just rst)]
--     _ -> 
  match (Block _ sts) =
    mconcat <$> sequence
    [ return $ emit "do\n"
    , withIndent $ prettyStatements sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "end"
    ]
  match m@(ModuleIntroduction _ name (Just rest)) =
    -- | if the top level binders of the module introduction has a VariableIntroduction
    -- where the RHS of the same is the instantiation of a data constructor or
    -- the result of any function application make it a top level function `def`
    -- in the generated elixir code.
    let fixedModuleDefn =
          case rest of
            Block _ bindings ->
              Block Nothing ((\binding ->
                                 case binding of
                                   (VariableIntroduction _ name (Just app@(App ss x y))) ->
                                     Function Nothing (Just name) [] (Block Nothing [app])
                                   (VariableIntroduction _ name (Just index@(Indexer _ _ _))) ->
                                     Function Nothing (Just name) [] (Block Nothing [index])
                                   _ -> binding
                            ) <$> bindings)
            _ -> rest
    in
    mconcat <$> sequence
    [ return $ emit ("defmodule " <> name <> " ")
    , prettyPrintJS' fixedModuleDefn
    , currentIndent]
  match (ModuleImport _ moduleToImport importAlias') =
    let underscoreAlias = (\alias -> intercalate "_" (T.splitOn "." alias))
        importAlias = case importAlias' of
                        Just alias ->
                          ", as: " <> (underscoreAlias alias)
                        _ ->
                          ", as: " <> (underscoreAlias moduleToImport)
    in
    mconcat <$> sequence 
    [ return $ emit ("alias " <> moduleToImport <> importAlias)]
    
  match (StructDeclaration _ fields _) =
    let
      fields' = (\field -> ":" <> field) <$> fields
    in
      mconcat <$> sequence
      [return $ emit ("defstruct [" <> (intercalate ", " fields') <> "]")]

  -- |
  -- If the varible binding is $foreign, then use Foreign
  match (Var _ ident') =
    let ident = case ident' of
                  "$foreign" -> "Foreign"
                  _ -> ident' in
    return $ emit ident

  match x@(VariableIntroduction _ ident value') =
    let value = value' in
    case value of
      -- | If the unary operator is a new + a function application, we know for sure that
      -- it is the instatiation of a data structure
      Just un@(Unary _ New rst) ->
        prettyPrintJS' $ case rst of
                           app@(App _ (Var _ fnName) arguments) ->
                             Function Nothing (Just ident) [] (Block Nothing [app])
                           _ -> un
      
      -- | HACK:
      -- In a variable introduction, if the right hand side
      -- is the creation of a data constructor, return the
      -- generated code directly
      Just (mod@(ModuleIntroduction _ _ _)) ->
        let mod' = mod in
        prettyPrintJS' mod'

      -- | HACK:
      -- All function invocations are of the form let function = ...
      -- To make it to the Elixir co mpatible def form and to support
      -- currying, take the first argument and make it a def, the rest
      -- of the arguments can be generated as anonymous functions
      Just fun@(Function _ name arguments' body) ->
        let arguments = arguments' in
        case arguments of
          (head:rest) ->
            case head of
              "__typeClassInstanceDefn" -> 
                let
                  body' = case body of
                            Block ss1 [(App ss2 (Var ss3 str) rst1)] -> Block ss1 [(App ss2 (Var ss3 ("tc" <> str)) rst1)]
                            _ -> body
                in
                  mconcat <$> sequence [ return $ emit ("def " <> ident <> " ")
                                       , prettyPrintJS' body']
                  
              -- "create" ->
              --   let fun' = fun in
              --   prettyPrintJS' fun'
              
              _ ->
                if name == (Just "__typeClassDefinition")
                then
                  let curriedFnBody = (Block Nothing [(Function Nothing Nothing arguments body)])
                  in
                    mconcat <$> sequence [ return $ emit ("def " <> "tc" <> ident <> " ")
                                         , prettyPrintJS' curriedFnBody]
                else
                  let curriedFnBody = (Block Nothing [(Function Nothing Nothing arguments body)]) in
                    mconcat <$> sequence [ return $ emit ("def " <> ident <> " ")
                                         , prettyPrintJS' curriedFnBody]
          _ -> prettyPrintJS' body
          
      _ ->
        mconcat <$> sequence
           [ return $ emit $ ident
           , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintJS') value
           ]
  match (Assignment _ target value) = mconcat <$> sequence
    [ prettyPrintJS' target
    , return $ emit " = "
    , prettyPrintJS' value
    ]
  match (While _ cond sts) = mconcat <$> sequence
    [ return $ emit "while ("
    , prettyPrintJS' cond
    , return $ emit ") "
    , prettyPrintJS' sts
    ]
  match (For _ ident start end sts) = mconcat <$> sequence
    [ return $ emit $ "for (let " <> ident <> " = "
    , prettyPrintJS' start
    , return $ emit $ "; " <> ident <> " < "
    , prettyPrintJS' end
    , return $ emit $ "; " <> ident <> "++) "
    , prettyPrintJS' sts
    ]
  match (ForIn _ ident obj sts) = mconcat <$> sequence
    [ return $ emit $ "for (let " <> ident <> " in "
    , prettyPrintJS' obj
    , return $ emit ") "
    , prettyPrintJS' sts
    ]
  match z@(IfElseBlock _  pred thens elses) =
    mconcat <$> sequence
    [ return $ emit "if ("
    , prettyPrintJS' pred
    , return $ emit ") "
    --, return $ emit "do\n  "
    , withIndent $ prettyPrintJS' $ thens
    , return $ emit "\n"
    , currentIndent
    , return $ emit "else\n"
    , currentIndent
    , maybe (return $ emit "# not JS") (\x -> withIndent $ prettyPrintJS' x) elses
    , return $ emit "\n"
    , currentIndent
    , return $ emit "end"
    ]
                       --, maybe (return mempty) (fmap (emit "else" <>) . prettyPrintJS') elses
  match (IfElse _ cond thens elses) =
    let elses' = case elses of
                   Just (Block _ items) -> items
                   Just other -> [other]
                   _ -> [Throw Nothing (StringLiteral Nothing "No pattern matched")]
        thens' = case thens of
                   (Block _ items) -> items
                   other -> [other]
                   _ -> [thens]
    in
                            
                   
    -- let (Block _ insides) = thens
    --     thens' = IfElseBlock Nothing insides elses in
      mconcat <$> sequence
      [ withIndent $ return $ emit "if ("
      , prettyPrintJS' cond
      , return $ emit ") do \n"
      , withIndent $ prettyStatements thens'
      , return $ emit "\n"
      , currentIndent
      , return ( emit  "else\n")
      , withIndent $ prettyStatements elses'
      , return $ emit "\n"
      , currentIndent
      , return $ emit $ "end"
      ]

  -- | Don't "return"
  match (Return _ value) = mconcat <$> sequence
    [
      prettyPrintJS' value
    ]
  match (ReturnNoResult _) = return $ emit "/* return */"
  match (Throw _ value) =
    let value' = value in
    mconcat <$> sequence
    [ return $ emit "raise "
    , prettyPrintJS' (StringLiteral Nothing "Errore")
    ]
  match (Comment _ com js) = mconcat <$> sequence
    [ return $ emit "\n"
    , mconcat <$> forM com comment
    , prettyPrintJS' js
    ]
  match a@(_) = mzero

  comment :: (Emit gen) => Comment -> StateT PrinterState Maybe gen
  comment (LineComment com) = fmap mconcat $ sequence $
    [ currentIndent
    , return $ emit "//" <> emit com <> emit "\n"
    ]
  comment (BlockComment com) = fmap mconcat $ sequence $
    [ currentIndent
    , return $ emit "/**\n"
    ] ++
    map asLine (T.lines com) ++
    [ currentIndent
    , return $ emit " */\n"
    , currentIndent
    ]
    where
    asLine :: (Emit gen) => Text -> StateT PrinterState Maybe gen
    asLine s = do
      i <- currentIndent
      return $ i <> emit " * " <> (emit . removeComments) s <> emit "\n"

    removeComments :: Text -> Text
    removeComments t =
      case T.stripPrefix "*/" t of
        Just rest -> removeComments rest
        Nothing -> case T.uncons t of
          Just (x, xs) -> x `T.cons` removeComments xs
          Nothing -> ""

accessor :: Pattern PrinterState AST (Text, AST)
accessor = mkPattern match
  where
  match (Indexer _ (StringLiteral _ prop) val) =
    case decodeString prop of
      Just s | not (identNeedsEscaping s) -> Just (s, val)
      _ -> Nothing
  match _ = Nothing

indexer :: (Emit gen) => Pattern PrinterState AST (gen, AST)
indexer = mkPattern' match
  where
  match (Indexer _ index val') =
    let val = val' in
    (,) <$> prettyPrintJS' index <*> pure val
  match _ = mzero

lam :: Pattern PrinterState AST ((Maybe Text, [Text], Maybe SourceSpan), AST)
lam = mkPattern match
  where
  match z@(Function ss name' args ret) =
    let name = name'
        body' = case ret of
                 -- | If we get an iife form of RHS, we can assume that we have a function binder
                 -- with where clause inside
                 Block ss binders -> 
                   let binds =
                         ((\binder ->
                              case binder of
                                (VariableIntroduction _ varName (Just fn@(Function ss name args body))) ->
                                  let lhs = Var Nothing varName
                                      rhs = fn in
                                    (Assignment Nothing lhs rhs)
                                _ -> binder
                          ) <$> binders)
                   in
                     Block ss binds
                 _ -> ret
    in
    -- | HACK
    -- For anonymous functions (where name is Nothing),
    -- replace the body of the function by a FnBlock, which does not
    -- insert a `do` in the beginning of the block
    case name of
      Nothing ->
        let (Block _ body) = body'
            ret' = (FnBlock Nothing body)
        in
          Just((name, args, ss), ret')
      _ ->
        Just ((name, [], ss), body')
  match _ = Nothing

app :: (Emit gen) => Pattern PrinterState AST (gen, AST)
app = mkPattern' match
  where
  match (App _ val args) = do
    jss <- traverse prettyPrintJS' args
    return (intercalate (emit ", ") jss, val)
  match _ = mzero

instanceOf :: Pattern PrinterState AST (AST, AST)
instanceOf = mkPattern match
  where
  match (InstanceOf _ val ty) = Just (val, ty)
  match _ = Nothing

unary' :: (Emit gen) => UnaryOperator -> (AST -> Text) -> Operator PrinterState AST gen
unary' op mkStr = Wrap match (<>)
  where
  match :: (Emit gen) => Pattern PrinterState AST (gen, AST)
  match = mkPattern match'
    where
    match' (Unary _ op' val) | op' == op = Just (emit $ mkStr val, val)
    match' _ = Nothing

unary :: (Emit gen) => UnaryOperator -> Text -> Operator PrinterState AST gen
unary op str = unary' op (const str)

negateOperator :: (Emit gen) => Operator PrinterState AST gen
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (Unary _ Negate _) = True
  isNegate _ = False

binary :: (Emit gen) => BinaryOperator -> Text -> Operator PrinterState AST gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " <> str <> " ") <> v2)
  where
  match :: Pattern PrinterState AST (AST, AST)
  match = mkPattern match'
    where
    match' (Binary _ op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: (Emit gen) => [AST] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  jss <- forM sts prettyPrintJS'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit "") . (indentString <>)) jss

-- | Generate a pretty-printed string representing a collection of JavaScript expressions at the same indentation level
prettyPrintJSWithSourceMaps :: [AST] -> (Text, [SMap])
prettyPrintJSWithSourceMaps js =
  let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements) js
  in (s, mp)

prettyPrintJS :: [AST] -> Text
prettyPrintJS = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements

-- | Generate an indented, pretty-printed string representing a JavaScript expression
prettyPrintJS' :: (Emit gen) => AST -> StateT PrinterState Maybe gen
prettyPrintJS' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: (Emit gen) => Pattern PrinterState AST gen
  matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
  operators :: (Emit gen) => OperatorTable PrinterState AST gen
  operators =
    OperatorTable [ [ Wrap indexer $ \index val -> val <> emit "[" <> index <> emit "]" ]
                  , [ Wrap accessor $ \prop val -> (emit "Map.get(") <> val <> emit "," <> emit "\"" <> emit prop <> emit "\")"]
                  , [ Wrap app $ \args val -> val <> emit(".") <> emit "(" <> args <> emit ")" ]
                  , [ unary New "" ]
                  , [ Wrap lam $ \(name, args, ss) ret ->
                        addMapping' ss <>
                        case name of
                          Just name -> 
                            emit ("def " <> name <> "(" <> intercalate ", " args <> ") ") <> ret
                          Nothing -> emit ("fn" <> "(" <> intercalate "," args <> ") -> ") <> ret]
                  , [ unary     Not                  "!"
                    , unary     BitwiseNot           "~"
                    , unary     Positive             "+"
                    , negateOperator ]
                  , [ binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%" ]
                  , [ binary    Add                  "+"
                    , binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<"
                    , binary    ShiftRight           ">>"
                    , binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    LessThan             "<"
                    , binary    LessThanOrEqualTo    "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqualTo ">="
                    , AssocR instanceOf $ \v1 v2 -> v1 <> emit ".__struct__ == " <> v2 ]
                  , [ binary    EqualTo              "==="
                    , binary    NotEqualTo           "!==" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                    ]
