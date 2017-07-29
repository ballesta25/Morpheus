{-

-}

{-# LANGUAGE GADTs, KindSignatures #-}

module Parser (parse, lexSymbol) where
import Text.Read  (readMaybe)
import Data.Maybe (isJust)
import Data.Char

import Core

data Token :: * where
  Bracket  :: Direction -> Paren -> Token
  Tok      :: Symbol -> Token
  deriving Show

tokenize :: String -> [String]
tokenize = words -- may want to update

-- returns either the parsed program, or a list of errors
parse :: String -> Either [String] Prgm
parse = accumulateParse . map lexSymbol . tokenize

accumulateParse :: [Either String Token] -> Either [String] Prgm
accumulateParse = clean . acc []
  where clean (Left x)    = Left x
        clean (Right [])  = Right [] -- empty program
        clean (Right [x]) = Right x
        clean _ = Left ["error in clean step."]

acc :: [Paren] -> [Either String Token] -> Either [String] [Prgm]
acc [] [] = Right [[]]
acc bt [] = Left ["reached end with unmatched parens: " ++ show bt]
acc bt (Right (Tok s) : xs) = case acc bt xs of
                                   Right (ys : zss) -> Right ((s:ys) : zss)
                                   errs -> errs
acc bt (Right (Bracket L b) : xs) = case acc (b : bt) xs of
  Right (exp : rest : zss) -> Right $ (Quotation exp : rest) : zss
  errs -> errs
acc [] (Right (Bracket R b) : xs) = acc [] $ (Left $ "unmatched closing paren: " ++ show b) : xs
acc (p : bt) (Right (Bracket R b) : xs) = if p == b
                                          then
                                            case acc bt xs of
                                              Right ys -> Right ([] : ys)
                                              errs -> errs
                                          else
                                            acc bt $ (Left $ "mismatched parens: expected: " ++ show p ++ " found: " ++ show b) : xs
acc bt (Left err : xs) = Left $ err : foldr filterErr [] xs
    where filterErr (Left e) = (e:)
          filterErr _ = id

lexSymbol :: String -> Either String Token
lexSymbol str = case readSymbol str of
                    Just s -> Right s
                    Nothing -> Left $ "Couldn't read: " ++ str

parseTypes :: [String -> Maybe Symbol]
parseTypes = [ fmap IntLit . readMaybe
             , fmap StrLit . readMaybe
             , fmap QuotID . readQuotName
             ] ++ fmap readPrim primatives ++
             [ fmap Identifier . readIdentifier
             ]
    where primatives = [ ("+",(:+))
                       , ("-",(:-))
                       , ("print", Print)
                       , ("bind", Bind)
                       , ("exec", Exec)
                       ]


readPrim :: (String, Primative) -> String -> Maybe Symbol
readPrim (s, p) s1 | s == s1 = Just $ PrimOp p
                   | otherwise = Nothing

readQuotName :: String -> Maybe Name
readQuotName s = case s of
                   ('\'':nm) -> readIdentifier nm
                   _         -> Nothing

readIdentifier :: String -> Maybe Name
readIdentifier s = if all isAlphaNum s  -- TODO: fix to allow morpheme decomposition
                   then Just s
                   else Nothing


-- add more: maybe infinite possibilities like Lua nested comments
data Paren :: * where
  Sq      :: Paren -- [ ]
  deriving (Show, Eq)

data Direction = L | R deriving Show -- left / right (open / close)

readBracket :: String -> Maybe Token
readBracket "[" = Just $ Bracket L Sq
readBracket "]" = Just $ Bracket R Sq
readBracket _ = Nothing

readSymbol :: String -> Maybe Token
readSymbol str = let parses = map ($ str) parseTypes
               in  case filter isJust parses of
                     (Just p : _) -> Just $ Tok p
                     []    -> readBracket str
