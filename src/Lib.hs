module Lib
  ( someFunc
  ) where

import Data.Decimal (Decimal)
import qualified Data.Text as T
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Data.Vector (Vector, (!?), empty, snoc)

data Symbol = OPERATOR Operator | BRACKET Bracket
  deriving (Eq, Show)

data UnaryOperator = PLUS | MINUS
  deriving (Eq, Show)

data BinaryOperator = ADDITION | SUBTRACTION | MULTIPLICATION | DIVISION | EXPONENT
  deriving (Eq, Show)

data Operator = UNARY UnaryOperator | BINARY BinaryOperator
  deriving (Eq, Show)

data Bracket = OPENING | CLOSING
  deriving (Eq, Show)

instance Read Symbol where
  readsPrec :: Int -> ReadS Symbol
  readsPrec _ "+" = [(OPERATOR (BINARY ADDITION), "")]
  readsPrec _ "-" = [(OPERATOR (BINARY SUBTRACTION), "")]
  readsPrec _ "*" = [(OPERATOR (BINARY MULTIPLICATION), "")]
  readsPrec _ "/" = [(OPERATOR (BINARY DIVISION), "")]
  readsPrec _ "^" = [(OPERATOR (BINARY EXPONENT), "")]
  readsPrec _ "(" = [(BRACKET OPENING, "")]
  readsPrec _ ")" = [(BRACKET CLOSING, "")]
  readsPrec _  _  = []

data Term = VARIABLE Int | CONSTANT Decimal
  deriving (Show, Eq)

instance Read Term where
  readsPrec :: Int -> ReadS Term
  readsPrec _ ('s' : indexStr) = maybe [] (\ index -> [(VARIABLE index, "")]) $ readMaybe indexStr
  readsPrec _ str = maybe [] (\ index -> [(CONSTANT index, "")]) $ readMaybe str

data Token = SYMBOL Symbol | TERM Term
  deriving (Eq)

instance Read Token where
  readsPrec :: Int -> ReadS Token
  readsPrec _ tokenStr =
    let mSymbol = (\ symbol -> (SYMBOL symbol, "")) <$> readMaybe tokenStr
        mTerm = (\ term -> (TERM term, "")) <$> readMaybe tokenStr
    in catMaybes [mSymbol <|> mTerm]

data Expression
  = LEAF Term
  | UNARY_NODE UnaryOperator Expression 
  | BINARY_NODE BinaryOperator Expression Expression
  deriving (Show)

-- Lower to Higher: Top to Bottom
precedenceOrder :: [[Symbol]]
precedenceOrder =
  [ [OPERATOR (BINARY ADDITION), OPERATOR (BINARY SUBTRACTION)]
  , [OPERATOR (BINARY MULTIPLICATION), OPERATOR (BINARY DIVISION)]
  , [OPERATOR (BINARY EXPONENT)]
  , [OPERATOR (UNARY PLUS), OPERATOR (UNARY MINUS)]
  , [BRACKET OPENING , BRACKET CLOSING]
  ]

precedence :: Symbol -> Int
precedence = precedenceAux 1 precedenceOrder
  where
  precedenceAux :: Int -> [[Symbol]] -> Symbol -> Int
  precedenceAux _ [] _ = 0
  precedenceAux order (headSymbols : otherSymbols) operator
    | operator `elem` headSymbols = order
    | otherwise = precedenceAux (order + 1) otherSymbols operator

isSymbol :: String -> Bool
isSymbol = isJust . (readMaybe :: String -> Maybe Symbol)

isBinaryOperator :: Token -> Bool
isBinaryOperator (SYMBOL (OPERATOR (BINARY _))) = True
isBinaryOperator _ = False

unary :: UnaryOperator -> Decimal -> Decimal
unary PLUS x = x
unary MINUS x = -x

binary :: BinaryOperator -> Decimal -> Decimal -> Decimal
binary ADDITION = (+)
binary SUBTRACTION = (-)
binary MULTIPLICATION = (*)
binary DIVISION = (/)
binary EXPONENT = \ a b -> a ^^ (floor b :: Integer)

splitTokens :: String -> [String]
splitTokens = dropWhile null . foldr (flip splitTokensAux) []
  where
  splitTokensAux :: [String] -> Char -> [String]
  splitTokensAux [] c = splitTokensAux [[]] c
  splitTokensAux acc@([] : _) ' ' = acc
  splitTokensAux acc ' ' = [] : acc
  splitTokensAux ([] : ar) c | isSymbol [c] = [] : [c] : ar
  splitTokensAux acc c | isSymbol [c] = [] : [c] : acc
  splitTokensAux (a : ar) c = (c : a) : ar

parseExpression :: String -> Either String Expression
parseExpression string = do
  tokens <- fmap identifyUnaryOps $ maybe (Left $ "Invalid Expression: " <> string) return $ mapM readMaybe $ splitTokens string
  parseExpressionAux tokens [] []
  where
  parseExpressionAux :: [Token] -> [Symbol] -> [Expression] -> Either String Expression
  parseExpressionAux [] (OPERATOR (UNARY o) : opr) (e : er) = parseExpressionAux [] opr (UNARY_NODE o e : er)
  parseExpressionAux [] (OPERATOR (BINARY o) : opr) (e1 : e2 : er) = parseExpressionAux [] opr (BINARY_NODE o e2 e1 : er)
  parseExpressionAux [] [] [expr] = Right expr
  parseExpressionAux (SYMBOL (BRACKET CLOSING) : tr) (BRACKET OPENING : opr) exps
    = parseExpressionAux tr opr exps
  parseExpressionAux tokens@(SYMBOL op : _) (o'@(OPERATOR (UNARY o)) : opr) (e : er)
    | op == BRACKET CLOSING || precedence op <= precedence o'
    = parseExpressionAux tokens opr (UNARY_NODE o e : er)
  parseExpressionAux tokens@(SYMBOL op : _) (o'@(OPERATOR (BINARY o)) : opr) (e1 : e2 : er)
    | op == BRACKET CLOSING || precedence op <= precedence o'
    = parseExpressionAux tokens opr (BINARY_NODE o e2 e1 : er)
  parseExpressionAux (SYMBOL op : tr) ops exps | notNextOperator op tr = parseExpressionAux tr (op : ops) exps
  parseExpressionAux (TERM tm : tr) ops exps = parseExpressionAux tr ops (LEAF tm : exps)
  parseExpressionAux _ _ _ = Left ("Invalid Expression: " <> string)

  notNextOperator :: Symbol -> [Token] -> Bool
  notNextOperator (OPERATOR (UNARY _)) (SYMBOL (OPERATOR _) : _) = False
  notNextOperator _ (SYMBOL (OPERATOR (BINARY _)) : _) = False
  notNextOperator _ _ = True

  identifyUnaryOps :: [Token] -> [Token]
  identifyUnaryOps = identifyUnaryOpsHead . foldr identifyUnaryOpsAux []

  identifyUnaryOpsAux :: Token -> [Token] -> [Token]
  identifyUnaryOpsAux token (SYMBOL (OPERATOR (BINARY op)) : ar)
    | (token == SYMBOL (BRACKET OPENING) || isBinaryOperator token) && op `elem` [ADDITION, SUBTRACTION]
    = let op' = if op == ADDITION then PLUS else MINUS
      in (token : SYMBOL (OPERATOR (UNARY op')) : ar)
  identifyUnaryOpsAux token acc = token : acc

  identifyUnaryOpsHead :: [Token] -> [Token]
  identifyUnaryOpsHead (SYMBOL (OPERATOR (BINARY op)) : ar)
    | op `elem` [ADDITION, SUBTRACTION]
    = let op' = if op == ADDITION then PLUS else MINUS
      in (SYMBOL (OPERATOR (UNARY op')) : ar)
  identifyUnaryOpsHead acc = acc

printExpr :: Int -> Expression -> IO ()
printExpr level ex = do
  putStr $ replicate level '-'
  putStr " "
  case ex of
    LEAF t -> print t
    UNARY_NODE op e -> print op >> printExpr (level * 2) e
    BINARY_NODE op e1 e2 -> print op >> printExpr (level * 2) e1 >> printExpr (level * 2) e2

evaluateExprAux :: Vector Decimal -> Expression -> Either String Decimal
evaluateExprAux _ (LEAF (CONSTANT val)) = Right val
evaluateExprAux array (LEAF (VARIABLE idx)) = maybe (Left $ "Variable index out of range: " <> show idx) Right (array !? (idx - 1))
evaluateExprAux array (UNARY_NODE op e) = do
  let operation = unary op
  res <- evaluateExprAux array e
  return $ operation res
evaluateExprAux array (BINARY_NODE op e1 e2) = do
  let operation = binary op
  res1 <- evaluateExprAux array e1
  res2 <- evaluateExprAux array e2
  if op == EXPONENT && res2 - fromInteger (floor res2) > 0
    then Left "Exponent cannot be a decimal number"
    else return $ operation res1 res2

evaluateExpr :: Vector Decimal -> String -> Either String Decimal
evaluateExpr array exprStr = do
  expr <- parseExpression exprStr
  evaluateExprAux array expr

someFunc :: IO ()
someFunc = do
  let exprStr = "--(2 * (-54) + -5)"
      vals = empty `snoc` 1 `snoc` 2 `snoc` 3 `snoc` 4 `snoc` 5 `snoc` 6 `snoc` 7 `snoc` 8
      res = evaluateExpr vals exprStr
  print res
  return ()
