module Lang.Types where

data LangExpr = Identifier String
                | FunCall [LangExpr]
                | CompoundExpr [LangExpr]
                | IntExpr  Integer
                | FloatExpr Double
                | StrExpr String
                deriving Show
