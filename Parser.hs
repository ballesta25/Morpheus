{-

-}
module Parser (parse, parseSymbol) where
import Text.Read  (readMaybe)
import Data.Maybe (isJust)
import Data.Char

import Core

-- returns either the parsed program, or a list of errors
parse :: String -> Either [String] Prgm
parse = accumulateParse . map parseSymbol . words

accumulateParse :: [Either String Symbol] -> Either [String] Prgm
accumulateParse [] = Right []
accumulateParse (Right s : xs) = case accumulateParse xs of
                                   Right ys -> Right (s:ys)
                                   errs -> errs
accumulateParse (Left err : xs) = Left $ err : foldr filterErr [] xs
    where filterErr (Left e) = (e:)
          filterErr _ = id

parseSymbol :: String -> Either String Symbol
parseSymbol str = case readSymbol str of
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

readSymbol :: String -> Maybe Symbol
readSymbol str = let parses = map ($ str) parseTypes
               in  case filter isJust parses of
                     (p:_) -> p
                     []    -> Nothing
