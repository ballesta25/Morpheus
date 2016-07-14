{-
Andrei Elliott
2015/03/17

Core for Morpheus language.
-}

{-# LANGUAGE GADTs, KindSignatures #-}

module Core where
import Control.Monad (mzero)

import State

data Primative = (:+) | (:-) | Print
               deriving Show

data Symbol :: * where
     IntLit :: Integer -> Symbol
     StrLit :: String -> Symbol
     PrimOp :: Primative -> Symbol
     Quotation :: Prgm -> Symbol
     deriving Show

type Prgm = [Symbol]

data Expr :: * where
     MInt :: Integer -> Expr
     MString :: String -> Expr
     MVoid :: Expr

instance Show Expr where
    show (MInt n) = show n
    show (MString s) = show s
    show MVoid = "()"

type Stack = [Expr]

showStack :: Stack -> String
showStack = unwords . map show . reverse

data MState :: * where
     MState :: IO () -> Stack -> MState

instance Monoid MState where
    mempty = MState (return ()) []
    (MState a0 s0) `mappend` (MState a1 s1) = MState (a0 >> a1) (s1 ++ s0)

stack :: MState -> Stack
stack (MState _ s) = s

action :: MState -> IO ()
action (MState a _) = a

setAction :: MState -> IO() -> MState
setAction s a = MState a (stack s)

appendAction :: IO () -> State MState Expr
appendAction act = State $ \(MState a s) -> (MVoid, MState (a >> act) s)

push :: Expr -> State MState Expr
push e = State $ \s -> (MVoid, MState (action s) (e:stack s))

pop :: State MState Expr
pop = State $ \s -> let l = stack s
                    in (head l, MState (action s) (tail l))


step :: Symbol -> State MState Expr
step (IntLit n) = push (MInt n)
step (StrLit s) = push (MString s)
step (PrimOp o) = stepPrim o

stepPrim :: Primative -> State MState Expr
stepPrim (:+) = do MInt x <- pop
                   MInt y <- pop
                   let res = MInt $ y + x
                   push res
                   return res
stepPrim (:-) = do MInt x <- pop
                   MInt y <- pop
                   let res = MInt $ y - x
                   push res
                   return res
stepPrim Print = do p <- pop
                    appendAction (print p)
                    return p


run :: Prgm -> (Expr, MState)
run = flip runEnv mempty

runEnv :: Prgm -> MState -> (Expr, MState)
runEnv p = runState (foldl (>>) (return MVoid) $ map step p)