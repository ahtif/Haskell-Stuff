module MyParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Exp = Var String
         | IntConst Integer
         | Neg Exp
         | IntBinary IntOp Exp Exp
         | BoolConst Bool
         | Not Exp
         | BBinary BoolOp Exp Exp
         | RBinary RelOp Exp Exp 
         | EqBinary EqOp Exp Exp
         | StrConst String
         | StrBinary StrOp Exp Exp 
         | Funct String [Exp]
          deriving (Show)

data StrOp = Append deriving (Show)

data BoolOp = And | Or deriving (Show)

data RelOp = Greater 
           | Less
           | GreaterEqual
           | LessEqual
            deriving (Show)

data EqOp = Equal | NotEqual deriving (Show)

data IntOp = Add
           | Subtract
           | Multiply
           | Divide
           | Modulo
           | Power
            deriving (Show)

data Statement = Seq [Statement] 
               | Only Exp
               | Assign String Exp
               | If Exp Statement 
               | IfElse Exp Statement Statement
               | While Exp Statement
                deriving (Show)

data Program = Lst [Program] 
             | FunctionDef String [String] Statement Exp
             | Jst Statement
              deriving (Show)

format2 :: Program -> String
format2 (Lst []) = []
format2 (Lst (p:pp)) = "Seq [<div class='indent'>" ++ format2 p ++ format2L pp ++"<br/></div>]"
format2 (FunctionDef name args body ret) = "FunctionDef " ++ name ++ "(" ++ flattenArgs args ++ ")" ++ " [<div class='indent'>" ++ format body ++ "<br/> Return " ++ show ret ++ "<br/></div>]"
format2 (Jst statement) = format statement

flattenArgs :: [String] -> String
flattenArgs (x:[]) = x
flattenArgs (x:xs) = x ++ ", " ++ flattenArgs xs

format2L :: [Program] -> String
format2L [] = []
format2L (p:pp) = "<br/>" ++ format2 p ++ " " ++ format2L pp

format :: Statement -> String
format (Seq []) = []
format (Seq (s:ss)) = "Seq [<div class='indent'>" ++ format s ++ formatL ss ++"<br/></div>]"
format (Only exp) = "Only " ++ "(" ++ show exp ++ ")"
format (Assign str exp) = "Assign " ++ show str ++ " (" ++ show exp ++ ")"
format (If exp stmt) = "If " ++ show exp ++ "<br/>" ++ format stmt ++ ""
format (IfElse exp stmt1 stmt2) = "If " ++ show exp ++ "<br/>" ++ format stmt1 ++ "<br/>Else <br/>" ++ (format stmt2) 
format (While exp stmt) = "While " ++ show exp ++ "<br/>" ++ format stmt

formatL :: [Statement] -> String
formatL [] = []
formatL (s:ss) = "<br/>" ++ format s ++ " " ++ formatL ss

languageDef =
    emptyDef { Token.commentStart    = "/*"
             , Token.commentEnd      = "*/"
             , Token.commentLine     = "//"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "if"
                                       , "then"
                                       , "else"
                                       , "endif"
                                       , "while"
                                       , "do"
                                       , "endwhile"
                                       , "true"
                                       , "false"
                                       , "and"
                                       , "or"
                                       , "int"
                                       , "bool"
                                       , "function"
                                       , "return"
                                       ]
             , Token.reservedOpNames = [ "+"
                                       , "-"
                                       , "*"
                                       , "/"
                                       , "%"
                                       , "^"
                                       , ":="
                                       , "<"
                                       , ">"
                                       , "="
                                       , "<="
                                       , ">="
                                       , "<>"
                                       , "!"
                                       , "++"
                                       ] 
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier    lexer -- parses an identifier
reserved   = Token.reserved      lexer -- parses a reserved name
reservedOp = Token.reservedOp    lexer -- parses an operator
parens     = Token.parens        lexer -- parses surrounding parenthesis:
                                       --   parens p
                                       -- takes care of the parenthesis and
                                       -- uses p to parse what's inside them
integer    = Token.integer       lexer -- parses an integer
semi       = Token.semi          lexer -- parses a semicolon
whiteSpace = Token.whiteSpace    lexer -- parses whitespace
comma      = Token.comma         lexer -- parses a comma
str        = Token.stringLiteral lexer -- parses a literal string        


myParser :: Parser Program
myParser = whiteSpace >> listOfPrograms

listOfPrograms = 
    do list <- (endBy1 program semi)
       return $ if length list == 1 then head list else Lst list

program :: Parser Program
program =  liftM Jst statement
       <|> functDef

functDef :: Parser Program
functDef = 
    do reserved "function"
       name <- identifier
       args <- parens (sepBy1 identifier comma)
       body <- statements
       reserved "return"
       ret  <- expression
       return $ FunctionDef name args body ret


statements :: Parser Statement
statements =  parens statements
          <|> listOfStatements

listOfStatements = 
    do list <- (endBy1 statement semi)
       return $ if length list == 1 then head list else Seq list

statement :: Parser Statement
statement = ifStatement
         <|> whileStatement
         <|> try 
              assignStatement
         <|> expressionStatement

ifStatement :: Parser Statement
ifStatement =
   try 
   (do reserved "if"
       cond  <- expression
       reserved "then"
       statement1 <- statements
       reserved "endif"
       return $ If cond statement1) <|>
   (do reserved "if"
       cond  <- expression
       reserved "then"
       statement1 <- statements
       reserved "else"
       statement2 <- statements
       reserved "endif"
       return $ IfElse cond statement1 statement2) 
   

whileStatement :: Parser Statement
whileStatement =
    do reserved "while"
       cond <- expression
       reserved "do"
       stmt <- statements
       reserved "endwhile"
       return $ While cond stmt
       
assignStatement :: Parser Statement
assignStatement = 
  do var  <- identifier
     reservedOp ":="
     expr <- expression
     return $ Assign var expr

expressionStatement :: Parser Statement
expressionStatement = do exp <- expression
                         return $ Only exp

expression :: Parser Exp
expression = buildExpressionParser operators term

operators = [ 
              [Prefix (reservedOp "-"   >> return (Neg                 ))          ]
            , [Infix  (reservedOp "*"   >> return (IntBinary Multiply  )) AssocLeft,
               Infix  (reservedOp "/"   >> return (IntBinary Divide    )) AssocLeft]
            , [Infix  (reservedOp "+"   >> return (IntBinary Add       )) AssocLeft,
               Infix  (reservedOp "-"   >> return (IntBinary Subtract  )) AssocLeft]
            , [Infix  (reservedOp "%"   >> return (IntBinary Modulo    )) AssocLeft,
               Infix  (reservedOp "^"   >> return (IntBinary Power     )) AssocLeft] 

            , [Prefix (reservedOp "!"   >> return (Not                 ))          ]
            , [Infix  (reservedOp "and" >> return (BBinary And         )) AssocLeft,
               Infix  (reservedOp "or"  >> return (BBinary Or          )) AssocLeft]   

            , [Infix  (reservedOp "<"   >> return (RBinary Less        )) AssocLeft,
               Infix  (reservedOp ">"   >> return (RBinary Greater     )) AssocLeft,
               Infix  (reservedOp "<="  >> return (RBinary LessEqual   )) AssocLeft,
               Infix  (reservedOp ">="  >> return (RBinary GreaterEqual)) AssocLeft]

            , [Infix  (reservedOp "="   >> return (EqBinary Equal      )) AssocLeft,
               Infix  (reservedOp "<>"  >> return (EqBinary NotEqual   )) AssocLeft]

            , [Infix  (reservedOp "++"  >> return (StrBinary Append   )) AssocLeft]

            ]

term = parens expression
    <|> do 
        string <- str
        return (StrConst string)
    <|> (reserved "true"  >> return (BoolConst True ))
    <|> (reserved "false" >> return (BoolConst False))
    <|> try
        (do 
          string <- identifier
          args   <- parens (sepBy1 expression comma)
          return $ Funct string args)
    <|> (liftM Var identifier)
    <|> (liftM IntConst integer)

parseString :: String -> Program
parseString str =
  case parse myParser "" str of
    Left  e  -> error $ show e
    Right r  -> r
 
parseFile :: String -> IO Program
parseFile file =
  do program  <- readFile file
     case parse myParser "" program of
        Left  e  -> print e >> fail "parse error"
        Right r  -> return r

parseStringShow :: String -> String
parseStringShow str =
  case parse myParser "" str of
    Left  e  -> show e
    Right r  -> format2 r
