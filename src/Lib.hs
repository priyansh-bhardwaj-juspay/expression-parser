module Lib
  ( someFunc
  ) where

import Data.Decimal (Decimal)
import qualified Data.Text as T
import Data.Maybe (isJust, catMaybes)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Data.Vector (Vector, (!?), empty, snoc)

data Symbol = OPERATOR Operator | BRACKET Bracket
  deriving (Eq, Show)

data Operator = ADDITION | SUBTRACTION | MULTIPLICATION | DIVISION | EXPONENT
  deriving (Eq, Show)

data Bracket = OPENING | CLOSING
  deriving (Eq, Show)

instance Read Symbol where
  readsPrec :: Int -> ReadS Symbol
  readsPrec _ "+" = [(OPERATOR ADDITION, "")]
  readsPrec _ "-" = [(OPERATOR SUBTRACTION, "")]
  readsPrec _ "*" = [(OPERATOR MULTIPLICATION, "")]
  readsPrec _ "/" = [(OPERATOR DIVISION, "")]
  readsPrec _ "^" = [(OPERATOR EXPONENT, "")]
  readsPrec _ "(" = [(BRACKET OPENING, "")]
  readsPrec _ ")" = [(BRACKET CLOSING, "")]
  readsPrec _  _  = []

data Term = VARIABLE Int | CONSTANT Decimal
  deriving (Show)

instance Read Term where
  readsPrec :: Int -> ReadS Term
  readsPrec _ ('s' : indexStr) = maybe [] (\ index -> [(VARIABLE index, "")]) $ readMaybe indexStr
  readsPrec _ str = maybe [] (\ index -> [(CONSTANT index, "")]) $ readMaybe str

data Expression = TERM Term | COMPOSITE Operator Expression Expression
  deriving (Show)

-- Lower to Higher: Left to Right
precedenceOrder :: [[Symbol]]
precedenceOrder =
  [ [OPERATOR ADDITION, OPERATOR SUBTRACTION]
  , [OPERATOR MULTIPLICATION, OPERATOR DIVISION]
  , [OPERATOR EXPONENT]
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

isOperator :: String -> Bool
isOperator s = maybe False id $ readMaybe s >>= \ s -> case s of
  OPERATOR _ -> Just True
  _ -> Just False

isBracket :: String -> Bool
isBracket s = maybe False id $ readMaybe s >>= \ s -> case s of
  BRACKET _ -> Just True
  _ -> Just False

isTerm :: String -> Bool
isTerm = isJust . (readMaybe :: String -> Maybe Term)

getOperator :: Operator -> Decimal -> Decimal -> Decimal
getOperator ADDITION = (+)
getOperator SUBTRACTION = (-)
getOperator MULTIPLICATION = (*)
getOperator DIVISION = (/)
getOperator EXPONENT = (\ a b -> a ^^ floor b)

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
parseExpression string =
  let tokens = splitTokens string
  in parseExpressionAux tokens [] []
  where
  parseExpressionAux :: [String] -> [Symbol] -> [Expression] -> Either String Expression
  parseExpressionAux [] (OPERATOR o : opr) (e1 : e2 : er) = parseExpressionAux [] opr (COMPOSITE o e2 e1 : er)
  parseExpressionAux [] [] [expr] = Right expr
  parseExpressionAux (op : tr) (o : opr) exps
    | isSymbol op && read op == BRACKET CLOSING && o == BRACKET OPENING = parseExpressionAux tr opr exps
  parseExpressionAux tokens@(op : _) (o'@(OPERATOR o) : opr) (e1 : e2 : er)
    | isSymbol op && (read op == BRACKET CLOSING || precedence (read op) <= precedence o')
    = parseExpressionAux tokens opr (COMPOSITE o e2 e1 : er)
  parseExpressionAux (op : tr) ops exps | isSymbol op && notNextOperator tr = parseExpressionAux tr (read op : ops) exps
  parseExpressionAux (tm : tr) ops exps | isTerm tm = parseExpressionAux tr ops (TERM (read tm) : exps)
  parseExpressionAux _ _ _ = Left ("Invalid Expression: " <> string)

  notNextOperator :: [String] -> Bool
  notNextOperator [] = True
  notNextOperator (x : _) = not $ isOperator x

printExpr :: Int -> Expression -> IO ()
printExpr level ex = do
  putStr $ replicate level '-'
  putStr " "
  case ex of
    TERM t -> print t
    COMPOSITE op e1 e2 -> print op >> printExpr (level * 2) e1 >> printExpr (level * 2) e2

evaluateExprAux :: Vector Decimal -> Expression -> Either String Decimal
evaluateExprAux _ (TERM (CONSTANT val)) = Right val
evaluateExprAux array (TERM (VARIABLE idx)) = maybe (Left $ "Variable index out of range: " <> show idx) Right (array !? (idx - 1))
evaluateExprAux array (COMPOSITE op e1 e2) = do
  let operation = getOperator op
  res1 <- evaluateExprAux array e1
  res2 <- evaluateExprAux array e2
  if (op == EXPONENT && res2 - fromInteger (floor res2) > 0)
    then Left "Exponent cannot be a decimal number"
    else Right $ operation res1 res2

evaluateExpr :: Vector Decimal -> String -> Either String Decimal
evaluateExpr array exprStr = do
  expr <- parseExpression exprStr
  evaluateExprAux array expr

someFunc :: IO ()
someFunc = do
  let exprStr = "(s5+s2)*(s3-s1)/((s4^s2+s3)*(s8/s2))"
      vals = empty `snoc` 1 `snoc` 2 `snoc` 3 `snoc` 4 `snoc` 5 `snoc` 6 `snoc` 7 `snoc` 8
      res = evaluateExpr vals exprStr
  print res
  return ()
