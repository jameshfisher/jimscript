{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Map as Map
import qualified System.Posix.IO as PosixIO
import qualified Data.Char as Char
import qualified Control.Exception as Exception
import qualified Foreign.C.Types
import qualified System.Environment as Environment

data T = TOpen | TClose | TSymbol String | TInt Int deriving (Show)

tokenize :: String -> [T]
tokenize [] = []
tokenize ('#':cs) = tokenize $ dropWhile (/= '\n') cs
tokenize ('(':cs) = TOpen : tokenize cs
tokenize (')':cs) = TClose : tokenize cs
tokenize ('\'':'\\':'\\':'\'':cs) = TInt (Char.ord '\\') : tokenize cs
tokenize ('\'':'\\':'\'':'\'':cs) = TInt (Char.ord '\'') : tokenize cs
tokenize ('\'':'\\':'n':'\'':cs) = TInt (Char.ord '\n') : tokenize cs
tokenize ('\'':c:'\'':cs) = TInt (Char.ord c) : tokenize cs
tokenize (c : cs)
  | Char.isNumber c = TInt (read $ c : takeWhile Char.isNumber cs) : tokenize (dropWhile Char.isNumber cs)
  | isSymbolChar c = TSymbol (c : takeWhile isSymbolChar cs) : tokenize (dropWhile isSymbolChar cs)
  | Char.isSpace c = tokenize cs
  | otherwise      = error $ "unexpected character: " ++ [c]

isSymbolChar c = Char.isAlphaNum c || elem c "=+<-/%"

data R
  = RList [R]
  | RSymbol String
  | RInt Int
  deriving (Show)

parseOne :: [T] -> ([R], [T])
parseOne []               = ([], [])
parseOne (TOpen     : ts) = let (rs, ts') = parseMany [] ts in ([RList rs], ts')
parseOne (TSymbol s : ts) = ([RSymbol s], ts)
parseOne (TInt i    : ts) = ([RInt i], ts)
parseOne (TClose    : ts) = ([], ts)

parseMany :: [R] -> [T] -> ([R], [T])
parseMany prev ts = case parseOne ts of
  ([], ts') -> (prev , ts')
  (rs, ts') -> parseMany (prev++rs) ts'

parseR :: [T] -> R
parseR ts = case parseOne ts of
  ([r], []) -> r
  _ -> error "unexpected content after rose tree"

data E =
  -- literals
    EInt Int
  -- pure operators
  | EBinOp Op E E
  | ENot E
  -- global variables
  | EGet String
  | ESet String E
  -- control flow
  | EIf E E E
  | ESeq E E
  | EWhile E E
  | EDoWhile E E
  | ESkip
  -- I/O
  | EWriteByte E
  | EReadByte
  deriving (Show)

data Op = Add | Sub | Div | Mod | Eq | Lt | Lte | And
  deriving (Show)

parseE :: R -> E
parseE (RInt i) = EInt i
parseE (RList [RSymbol "-", RInt i]) = EInt $ negate i
parseE (RList [RSymbol "+", a, b]) = EBinOp Add (parseE a) (parseE b)
parseE (RList [RSymbol "-", a, b]) = EBinOp Sub (parseE a) (parseE b)
parseE (RList [RSymbol "/", a, b]) = EBinOp Div (parseE a) (parseE b)
parseE (RList [RSymbol "%", a, b]) = EBinOp Mod (parseE a) (parseE b)
parseE (RList [RSymbol "=", a, b]) = EBinOp Eq (parseE a) (parseE b)
parseE (RList [RSymbol "<", a, b]) = EBinOp Lt (parseE a) (parseE b)
parseE (RList [RSymbol "<=", a, b]) = EBinOp Lte (parseE a) (parseE b)
parseE (RList [RSymbol "and", a, b]) = EBinOp And (parseE a) (parseE b)
parseE (RList [RSymbol "not", a]) = ENot (parseE a)
parseE (RList [RSymbol "get", RSymbol a]) = EGet a
parseE (RList [RSymbol "set", RSymbol a, b]) = ESet a (parseE b)
parseE (RList [RSymbol "if", a, b, c]) = EIf (parseE a) (parseE b) (parseE c)
parseE (RList (RSymbol "seq" : xs)) = foldr1 ESeq $ map parseE xs
parseE (RList (RSymbol "while" : a : bs)) = EWhile (parseE a) (foldr1 ESeq $ map parseE bs)
parseE (RList [RSymbol "do-while", a, b]) = EDoWhile (parseE a) (parseE b)
parseE (RList [RSymbol "skip"]) = ESkip
parseE (RList [RSymbol "write", a]) = EWriteByte (parseE a)
parseE (RList [RSymbol "read"]) = EReadByte
parseE (RSymbol a) = EGet a
parseE r = error $ "did not match: " ++ show r

evalOp :: Op -> Int -> Int -> Int
evalOp Add a b = a + b
evalOp Sub a b = a - b
evalOp Div a b = a `div` b
evalOp Mod a b = a `mod` b
evalOp Eq a b = if a == b then 1 else 0
evalOp Lt a b = if a < b then 1 else 0
evalOp Lte a b = if a <= b then 1 else 0
evalOp And a b = if a == 0 || b == 0 then 0 else 1

eval :: Map.Map String Int -> E -> IO (Int, Map.Map String Int)
eval vars (EInt i) = return (i, vars)
eval vars (EBinOp op e1 e2) = do
  (val1, vars') <- eval vars e1
  (val2, vars'') <- eval vars' e2
  return (evalOp op val1 val2, vars'')
eval vars (ENot e) = do
  (v, vars') <- eval vars e
  case v of
    0 -> return (1, vars)
    _ -> return (0, vars)
eval vars (EGet var) = case Map.lookup var vars of
  Nothing -> error $ "no such variable: " ++ var
  Just x -> return (x, vars)
eval vars (ESet var e) = do
  (val, vars') <- eval vars e
  return (val, Map.insert var val vars)
eval vars (EIf c t e) = do
  (cond, vars') <- eval vars c
  case cond of
    0 -> eval vars' e
    _ -> eval vars' t
eval vars (EWhile c e) = do
  (cond, vars') <- eval vars c
  case cond of
    0 -> return (0, vars')
    _ -> do
      (_, vars'') <- eval vars' e
      eval vars'' (EWhile c e)
eval vars (EDoWhile e c) = do
  (_, vars') <- eval vars e
  (cond, vars'') <- eval vars' c
  case cond of
    0 -> return (0, vars'')
    _ -> eval vars'' (EDoWhile e c)
eval vars (ESeq e1 e2) = do
  (_, vars') <- eval vars e1
  eval vars' e2
eval vars ESkip = return (0, vars)
eval vars (EWriteByte byteE) = do
  (byte, vars') <- eval vars byteE
  if byte < 0 then error $ "Tried to print byte < 0: " ++ show byte
  else if 255 < byte then error $ "Tried to print byte > 255: " ++ show byte
  else PosixIO.fdWrite PosixIO.stdOutput [Char.chr byte]
  return (byte, vars')
eval vars EReadByte = do
  exp :: Either Exception.SomeException (String,Foreign.C.Types.CSize) <- Exception.try (PosixIO.fdRead PosixIO.stdInput 1)
  case exp of
    Left _ -> return (-1, vars)
    Right (str,count) -> do
      if count == 0 then
        return (-1, vars)
      else do
        let [c] = str
        return (Char.ord c, vars)

main :: IO ()
main = do
  (f:_) <- Environment.getArgs
  script <- readFile f
  let tokens = tokenize script
  let (rs, []) = parseMany [] tokens
  let e = foldr1 ESeq $ map parseE rs
  eval Map.empty e
  return ()
