module Lang.Parser where

import Lang.Types

import Text.ParserCombinators.Parsec
import Text.Parsec.Token (LanguageDef, commentStart, commentEnd, commentLine,
                          nestedComments, identStart, identLetter, opStart, opLetter,
                          reservedOpNames, reservedNames, caseSensitive, makeTokenParser
                         )
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

langStyle :: LanguageDef st
langStyle = emptyDef
    { commentStart   = ""
    , commentEnd     = ""
    , commentLine    = ""
    , nestedComments = True
    , identStart     = letter <|> char '_'
    , identLetter    = alphaNum <|> oneOf "_?!"
    , opStart        = opLetter emptyDef
    , opLetter       = oneOf ":#$%&*+./<=>@\\^|-~"
    , reservedOpNames= []
    , reservedNames  = []
    , caseSensitive  = True
    }

lexer = makeTokenParser langStyle

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

funcall :: Parser LangExpr
funcall = do{ symbol "["
            ; f <- many expr
            ; symbol "]"
            ; return $ FunCall f
            }
          <?> "function call"

variable :: Parser LangExpr
variable = do{ i <- identifier
             ;return $ Identifier i
             }
           <?> "variable"

expr :: Parser LangExpr
expr = (parens expr)
       <|> variable
       <|> funcall
       <?> "expression"

compoundExpr :: Parser LangExpr
compoundExpr = do { e <- sepBy1 expr (symbol ";")
                   ; return $ CompoundExpr e
                   }
                  <?> "compound expression"

multiexpr :: Parser [LangExpr]
multiexpr = do{ whiteSpace
           ; m <- many (lexeme expr)
           ; eof
           ; return m
           }

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

runLex :: Show a => Parser a -> String -> IO ()
runLex p input
        = run (do{ whiteSpace
                 ; x <- p
                 ; eof
                 ; return x
                 }) input
