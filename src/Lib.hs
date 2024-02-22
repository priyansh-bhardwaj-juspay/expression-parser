module Lib
  ( someFunc
  ) where

import Data.Decimal (Decimal)
import qualified Data.Text as T
import Data.Maybe (isJust, catMaybes)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Data.Vector (Vector, (!?), empty, snoc)

data Operator = ADDITION | SUBTRACTION | MULTIPLICATION | DIVISION | EXPONENT | OPENING_BRACKET | CLOSING_BRACKET
  deriving (Eq, Show)

instance Read Operator where
  readsPrec :: Int -> ReadS Operator
  readsPrec _ "+" = [(ADDITION, "")]
  readsPrec _ "-" = [(SUBTRACTION, "")]
  readsPrec _ "*" = [(MULTIPLICATION, "")]
  readsPrec _ "/" = [(DIVISION, "")]
  readsPrec _ "^" = [(EXPONENT, "")]
  readsPrec _ "(" = [(OPENING_BRACKET, "")]
  readsPrec _ ")" = [(CLOSING_BRACKET, "")]
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
precedenceOrder :: [[Operator]]
precedenceOrder = [[ADDITION, SUBTRACTION], [MULTIPLICATION, DIVISION], [EXPONENT], [OPENING_BRACKET, CLOSING_BRACKET]]

precedence :: Operator -> Int
precedence = precedenceAux 1 precedenceOrder
  where
  precedenceAux :: Int -> [[Operator]] -> Operator -> Int
  precedenceAux _ [] _ = 0
  precedenceAux order (headOperators : otherOperators) operator
    | operator `elem` headOperators = order
    | otherwise = precedenceAux (order + 1) otherOperators operator

isOperator :: String -> Bool
isOperator = isJust . (readMaybe :: String -> Maybe Operator)

isTerm :: String -> Bool
isTerm = isJust . (readMaybe :: String -> Maybe Term)

getOperator :: Operator -> Either String (Decimal -> Decimal -> Decimal)
getOperator ADDITION = Right (+)
getOperator SUBTRACTION = Right (-)
getOperator MULTIPLICATION = Right (*)
getOperator DIVISION = Right (/)
getOperator EXPONENT = Right (\ a b -> a ^^ floor b)
getOperator other = Left $ "Not supported for operation " <> show other

splitTokens :: String -> [String]
splitTokens = dropWhile null . foldr (flip splitTokensAux) []
  where
  splitTokensAux :: [String] -> Char -> [String]
  splitTokensAux [] c = splitTokensAux [[]] c
  splitTokensAux acc@([] : _) ' ' = acc
  splitTokensAux acc ' ' = [] : acc
  splitTokensAux ([] : ar) c | isOperator [c] = [] : [c] : ar
  splitTokensAux acc c | isOperator [c] = [] : [c] : acc
  splitTokensAux (a : ar) c = (c : a) : ar

parseExpression :: String -> Either String Expression
parseExpression string =
  let tokens = splitTokens string
  in parseExpressionAux tokens [] []
  where
  parseExpressionAux :: [String] -> [Operator] -> [Expression] -> Either String Expression
  parseExpressionAux [] (o : opr) (e1 : e2 : er) | not (isBracket o) = parseExpressionAux [] opr (COMPOSITE o e2 e1 : er)
  parseExpressionAux [] [] [expr] = Right expr
  parseExpressionAux (op : tr) (o : opr) exps
    | isOperator op && read op == CLOSING_BRACKET && o == OPENING_BRACKET = parseExpressionAux tr opr exps
  parseExpressionAux tokens@(op : _) (o : opr) (e1 : e2 : er)
    | isOperator op && o /= OPENING_BRACKET && (read op == CLOSING_BRACKET || precedence (read op) <= precedence o)
    = parseExpressionAux tokens opr (COMPOSITE o e2 e1 : er)
  parseExpressionAux (op : tr) ops exps | isOperator op && notNextOperator tr = parseExpressionAux tr (read op : ops) exps
  parseExpressionAux (tm : tr) ops exps | isTerm tm = parseExpressionAux tr ops (TERM (read tm) : exps)
  parseExpressionAux s _ t = Left ("Invalid Expression: " <> string)

  isBracket :: Operator -> Bool
  isBracket = flip elem [OPENING_BRACKET, CLOSING_BRACKET]

  notNextOperator :: [String] -> Bool
  notNextOperator [] = True
  notNextOperator (x : _) = x == "(" || not (isOperator x)

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
  operation <- getOperator op
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
      vals = empty `snoc` 1 `snoc` 2.1 `snoc` 3 `snoc` 4 `snoc` 5 `snoc` 6 `snoc` 7 `snoc` 8
      res = evaluateExpr vals exprStr
  print res
  return ()
