module Execute where

import MyParser

import Control.Monad
import Data.Function
import Data.List
import Data.Ord
import qualified Data.Map as Map

type Var = String 

data Val = Number Int | Boolean Bool | Str String | Fun ([Exp] -> Interp Val)

instance Show Val where
  show (Number n)  = show n
  show (Boolean b) = show b
  show (Str s)     = s
  show (Fun f)     = "(func)"

instance Eq Val where
  (Number n1 ) == (Number n2 ) = n1 == n2
  (Boolean b1) == (Boolean b2) = b1 == b2
  (Str s1)     == (Str s2)     = s1 == s2
  _            == _            = False

instance Ord Val where
  (Boolean b1) <= (Boolean b2) = b1 <= b2
  (Str s1)     <= (Str s2)     = s1 <= s2
  _            <= _            = False

instance Num Val where
  (Number n1) + (Number n2) = Number (n1 + n2)
  (Number n1) * (Number n2) = Number (n1 * n2)
  abs (Number n)            = Number (abs n)
  fromInteger n             = Number (fromIntegral n)
  signum (Number n)         = Number (signum n)
  negate (Number n)         = Number (negate n)

divVal :: Val -> Val -> Val
divVal (Number n1) (Number n2) = Number (n1 `div` n2)

modVal :: Val -> Val -> Val
modVal (Number n1) (Number n2) = Number (n1 `mod` n2)

powerVal :: Val -> Val -> Val
powerVal (Number n1) (Number n2) = Number (n1 ^ n2)

notVal :: Val -> Val
notVal (Boolean b) = Boolean (not b)

andVal :: Val -> Val -> Val
andVal (Boolean b1) (Boolean b2) = Boolean (b1 && b2)

orVal :: Val -> Val -> Val
orVal (Boolean b1) (Boolean b2) = Boolean (b1 || b2)

appendVal :: Val -> Val -> Val
appendVal (Str s1) (Str s2) = Str  (s1 ++ s2)
appendVal (Number n) (Str s) = Str (show n ++ s)
appendVal (Str s) (Number n) = Str (s ++ show n)

apply :: Val -> [Exp] -> Interp Val
apply (Fun f) x = f x
apply f       _ = fail $ show f ++ " should be a function"

type Store = [(Var, Val)]

newtype Interp a = Interp { runInterp  :: Store -> Either String (a, Store) }

instance Functor Interp where
  fmap = liftM

instance Applicative Interp where
  pure  = return
  (<*>) = ap

instance Monad Interp where
  return x = Interp $ \r -> Right (x, r)
  i >>= k  = Interp $ \r -> case runInterp i r of
               Left msg      -> Left msg
               Right (x, r') -> runInterp (k x) r'
  fail msg = Interp $ \_ -> Left msg

readVar :: Var -> Interp Val
readVar x = Interp $ \r -> case lookup x r of
         Nothing -> Left  ("unbound variable '" ++ x ++ "'")
         Just v  -> Right (v, r)

writeVar :: Var -> Val -> Interp ()
writeVar x v = Interp $ \r -> Right ((), (x, v) : r)

evaluate :: Exp -> Interp Val
evaluate (IntConst n)                 = do return (fromIntegral n)
evaluate (Var x)                      = do readVar x
evaluate (IntBinary Add e1 e2)        = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (v1 + v2)
evaluate (IntBinary Subtract e1 e2)   = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (v1 - v2)
evaluate (IntBinary Multiply e1 e2)   = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (v1 * v2)
evaluate (IntBinary Divide e1 e2)     = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           if v2 == 0
                                            then fail "division by zero"
                                            else return (v1 `divVal` v2)
evaluate (IntBinary Modulo e1 e2)     = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           if v2 == 0
                                            then fail "division by zero"
                                            else return (v1 `modVal` v2)
evaluate (IntBinary Power e1 e2)      = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (v1 `powerVal` v2)
evaluate (Neg e)                      = do v <- evaluate e
                                           return (negate v)
evaluate (BoolConst n)                = do return (Boolean n)
evaluate (Not n)                      = do v <- evaluate n
                                           return (notVal v)
evaluate (BBinary And e1 e2)          = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (andVal v1 v2)
evaluate (BBinary Or e1 e2)           = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (orVal v1 v2)
evaluate (EqBinary Equal e1 e2)       = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (Boolean (v1 == v2))
evaluate (RBinary Less e1 e2)         = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (Boolean (v1 < v2))
evaluate (RBinary Greater e1 e2)      = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (Boolean (v1 > v2))
evaluate (RBinary LessEqual e1 e2)    = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (Boolean (v1 <= v2))
evaluate (RBinary GreaterEqual e1 e2) = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (Boolean (v1 >= v2))
evaluate (StrConst s)                 = do return (Str s)
evaluate (StrBinary Append e1 e2)     = do v1 <- evaluate e1
                                           v2 <- evaluate e2
                                           return (v1 `appendVal` v2)
evaluate (Funct name args)            = do funct <- readVar name
                                           apply funct args
  
executeStmt :: Statement -> Interp ()
executeStmt (Assign name exp)        = do v <- evaluate exp
                                          writeVar name v
executeStmt (While exp stmt)         = do v <- evaluate exp
                                          when (v == (Boolean True)) (executeStmt (Seq [stmt, While exp stmt]))
executeStmt (If exp stmt)            = do v <- evaluate exp
                                          when (v == (Boolean True)) (executeStmt stmt)
executeStmt (IfElse exp stmt1 stmt2) = do v <- evaluate exp
                                          if (v == (Boolean True)) then (executeStmt stmt1)
                                            else (executeStmt stmt2) 
executeStmt (Only exp)               = do v <- evaluate exp
                                          return ()
executeStmt (Seq [])                 = return ()
executeStmt (Seq (s : ss))           = do executeStmt s
                                          executeStmt (Seq ss)

execute :: Program -> Interp ()
execute (Lst [])                         = return ()
execute (Lst (p : pp))                   = do execute p
                                              execute (Lst pp)
execute (FunctionDef name args body ret) = writeVar name (Fun $ mkFunc args body ret)
execute (Jst statement)                  = executeStmt statement 


mkFunc :: [Var] -> Statement -> Exp -> ([Exp] -> Interp Val)
mkFunc args body ret = (\x -> do assignArgs args x
                                 executeStmt body
                                 toReturn <- evaluate ret
                                 return toReturn)

-- Take list of args and exps, assign each arg to exp
assignArgs :: [Var] -> [Exp] -> Interp ()
assignArgs [] [] = return ()
assignArgs (x:xs) (y:ys)
  | length (x:xs) /= length (y:ys) = fail "Number of arguments do not match function definition"
  | otherwise                      = do executeStmt (Assign x y)
                                        assignArgs xs ys


run :: Program -> Store -> Either String Store
run p r = case runInterp (execute p) r of
            Left msg      -> Left msg
            Right (_, r') -> Right r' 

runString :: String -> Store -> Either String Store
runString prog lst = run (parseString prog) lst

showExec :: Either String Store -> String
showExec (Left a)    = show a
showExec (Right lst) = formatExec $ groupList $ nubBy ((==) `on` fst) lst

showStep :: Either String Store -> String
showStep (Left a)    = show a
showStep (Right lst) = formatExec $ groupList lst

formatExec :: (Show a, Show b) => [(a, [b])] -> String
formatExec [] = []
formatExec ((a, lst):xs) = show a ++ ": " ++ printL lst ++ "<br />" ++ formatExec xs

printL :: Show a => [a] -> String
printL (y:[])     = show y
printL (y:ys)     = show y ++ ", " ++ printL ys

extractL :: Either String Store -> Store
extractL (Right lst) = lst

groupList :: Ord a => [(a, b)] -> [(a, [b])]
groupList xs = Map.toList $ sortAndGroup xs

sortAndGroup :: Ord k => [(k, a)] -> Map.Map k [a]
sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

fib :: String
fib = "x:=0;\ny:=1;\nn:=5;\nwhile n>0 do\n\tz:= x+y;\n\tx:=y;\n\ty:=z;\n\tn:=n-1;\nendwhile;"

err :: String
err = "x:=5;\ny:=0;\nz:=x/y;"

factorial :: String
factorial = "function fact(n)\
  \if n = 1 then \
    \ret:=1;\
  \else \
    \ret:= n * fact(n-1);\
  \endif;\
  \return ret;\

  \a:=5;\
  \res:=fact(a);"

fib2 :: String
fib2 = "function fib(m) \
  \ret2:= 0; \
  \if m = 0 then \
    \ret2:= 0; \
  \endif; \
  \if m = 1 then \
    \ret2 := 1; \
  \endif; \
  \if m > 1 then \
    \ret2:= fib(m-1); \
    \ret2:= ret2 + fib (m-2); \
  \endif; \
  \return ret2; \
  
  \res2 := fib(10);"

{- Unfinished code for a better small step interpreter

data Conf = Ste (Store, Program) | End Store

newtype SmallInterp a = SmallInterp { stepInterp :: Conf -> Either String (a, Conf) }

instance Functor SmallInterp where
  fmap = liftM

instance Applicative SmallInterp where
  pure  = return
  (<*>) = ap

instance Monad SmallInterp where
  return x = SmallInterp $ \r -> Right (x, r)
  i >>= k  = SmallInterp $ \r -> case stepInterp i r of
                 Left msg       -> Left msg
                 Right (x, r')  -> stepInterp (k x) r'
  fail msg = SmallInterp $ \_-> Left msg

step :: Program -> SmallInterp ()
step _ = return ()

runStep :: Program -> Conf -> Either String Conf
runStep p conf = case stepInterp (step p) conf of
               Left msg       -> Left msg
               Right (_, cnf) -> Right cnf

 -}
