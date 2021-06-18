module Mlatu.Back.Print (generate) where

import Control.Monad.Loops (untilM)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Mlatu.Back.AST (EFun (..), Expr (..), Pattern (..))
import Mlatu.Back.Erlify (entryErl, erlifyI, erlifyQ, evalCodegen, getToDo)
import Mlatu.Back.Optimize (rewrite)
import Mlatu.Base.Name (Qualified (..))
import Mlatu.Front.Definition (mainName)
import Mlatu.Middle.Dictionary (Dictionary (..), wordEntries)
import Mlatu.Middle.Dictionary qualified as Dictionary
import Mlatu.Middle.Instantiated (Instantiated (..))

generate :: Dictionary -> Maybe Qualified -> IO Text
generate dict mMain = do
  let firstKey = maybe "main" erlifyQ mMain
  let firstEntry = case Dictionary.lookupWord (Instantiated (fromMaybe mainName mMain) []) dict of
        Just e -> e
        Nothing -> error "Could not find main entry"
  funs <-
    catMaybes
      <$> evalCodegen
        (untilM entryErl (Map.null <$> getToDo))
        (one (firstKey, firstEntry), Map.empty, 0, 0, 0)
        (Map.mapKeys erlifyI (view wordEntries dict))
  pure
    ( "-module(mlatu).\n -export([main/1]).\n main(_) -> m"
        <> firstKey
        <> "({[], []}).\n\n"
        <> Text.concat (serFun <$> funs)
    )

serPattern :: Pattern -> Text
serPattern PNil = "[]"
serPattern (PCons h t) = "[" <> serPattern h <> " | " <> serPattern t <> "]"
serPattern (PVar var) = var
serPattern (PAtom a) = a
serPattern (PInt i) = show i
serPattern (PWhen p cond) = serPattern p <> " when " <> serExpr cond
serPattern (PTuple xs) = "{" <> Text.concat (intersperse " , " (serPattern <$> xs)) <> "}"

serFun :: EFun -> Text
serFun (MkFun name body) = case rewrite body of
  ECase (EVar "Rest0") cases ->
    Text.concat
      ( intersperse
          ";\n"
          ( ( \(p, b) -> case p of
                (PWhen p guard) -> name <> "({" <> serPattern p <> ",Closure0}) when " <> serExpr guard <> " -> " <> serExpr b
                _ -> name <> "({" <> serPattern p <> ",Closure0}) -> " <> serExpr b
            )
              <$> cases
          )
      )
      <> ".\n\n"
  ECase (EVar "Closure0") cases ->
    Text.concat
      ( intersperse
          ";\n"
          ( ( \(p, b) -> case p of
                (PWhen p guard) -> name <> "({Rest0," <> serPattern p <> "}) when " <> serExpr guard <> " -> " <> serExpr b
                _ -> name <> "({Rest0," <> serPattern p <> "}) -> " <> serExpr b
            )
              <$> cases
          )
      )
      <> ".\n\n"
  r -> name <> "({Rest0, Closure0}) -> " <> serExpr r <> ".\n\n"

serExpr :: Expr -> Text
serExpr (ECase scrutinee cases) =
  " case " <> serExpr scrutinee <> " of "
    <> Text.concat (intersperse " ; " ((\(p, body) -> serPattern p <> " -> " <> serExpr body) <$> cases))
    <> " end "
serExpr (ECallFun name args) = name <> "(" <> Text.concat (intersperse " , " (serExpr <$> args)) <> " )"
serExpr (EVar var) = var
serExpr (ESet var expr) = serPattern var <> " = " <> serExpr expr
serExpr (EAnd xs) = Text.concat (intersperse " , " (serExpr <$> xs))
serExpr ENil = "[]"
serExpr (ECons h t) = "[" <> serExpr h <> " | " <> serExpr t <> "]"
serExpr (EAtom a) = a
serExpr (EIf xs) =
  " if " <> Text.concat (intersperse " ; " ((\(cond, body) -> serExpr cond <> " -> " <> serExpr body) <$> xs)) <> " end "
serExpr (EInt i) = show i
serExpr (EFun args body) = "fun (" <>  Text.concat (intersperse " , " args) <> ") -> " <> serExpr body <> " end "
serExpr (EString s) = "\"" <> fromString s <> "\""
serExpr (EOp left op right) = "(" <> serExpr left <> " " <> op <> " " <> serExpr right <> ")"
serExpr (ETuple xs) = "{ " <> Text.concat (intersperse " , " (serExpr <$> xs)) <> " }"
