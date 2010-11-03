module Lang.Types where

data LangExpr = Identifier String
                | FunCall [LangExpr]
                | CompoundExpr [LangExpr]
                deriving Show
