{-
Andrei Elliott
2015/03/17

Core for Morpheus language.
-}

{-# LANGUAGE GADTs, KindSignatures #-}

module Core where
import Control.Monad (mzero)

import State

data Primative = (:+) | (:-) | Print | Bind
               deriving Show

data Symbol :: * where
     IntLit :: Integer -> Symbol
     StrLit :: String -> Symbol
     PrimOp :: Primative -> Symbol
     Quotation :: Prgm -> Symbol
     Name :: String -> Symbol
     deriving Show

type Prgm = [Symbol]

data Expr :: * where
     MInt :: Integer -> Expr
     MString :: String -> Expr
     MVoid :: Expr
     MName :: Name -> Expr

instance Show Expr where
    show (MInt n) = show n
    show (MString s) = show s
    show MVoid = "()"

type Name = String

type Stack = [Expr]

showStack :: Stack -> String
showStack = unwords . map show . reverse

-- may want to replace later (Data.Map maybe?)
type Bindings = [(Name, Expr)]

insert :: Name -> Expr -> Bindings -> Bindings
insert name value = ((name, value):)

data MState :: * where
     MState :: IO () -> Stack -> Bindings -> MState

instance Monoid MState where
    mempty = MState (return ()) [] []
    (MState a0 s0 b0) `mappend` (MState a1 s1 b1) = MState (a0 >> a1) (s1 ++ s0) (b0 `mappend` b1)

stack :: MState -> Stack
stack (MState _ s _) = s

action :: MState -> IO ()
action (MState a _ _) = a

bindings :: MState -> Bindings
bindings (MState _ _ b) = b

setAction :: MState -> IO() -> MState
setAction st a = MState a (stack st) (bindings st)

appendAction :: IO () -> State MState Expr
appendAction act = State $ \(MState a s b) -> (MVoid, MState (a >> act) s b)

addBinding :: Name -> Expr -> State MState Expr
addBinding name value = State $ \s -> (value, MState (action s) (stack s) (insert name value (bindings s)))

push :: Expr -> State MState Expr
push e = State $ \s -> (MVoid, MState (action s) (e:stack s) (bindings s))

pop :: State MState Expr
pop = State $ \s -> let l = stack s
                    in (head l, MState (action s) (tail l) (bindings s))


step :: Symbol -> State MState Expr
step (IntLit n) = push (MInt n)
step (StrLit s) = push (MString s)
step (PrimOp o) = stepPrim o
step (Name s)   = push (MName s)

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
stepPrim Bind = do MName nm <- pop
                   expr <- pop
                   addBinding nm expr
                   return expr

run :: Prgm -> (Expr, MState)
run = flip runEnv mempty

runEnv :: Prgm -> MState -> (Expr, MState)
runEnv p = runState (foldl (>>) (return MVoid) $ map step p)