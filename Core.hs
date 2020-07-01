{-
Andrei Elliott
2015/03/17

Core for Morpheus language.
-}

{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances #-}

module Core where

import Control.Applicative ((<|>))
import Control.Monad.Fail
import State

data Primative = (:+) | (:-) | Print | Bind | Exec
               deriving Show

data Symbol :: * where
     IntLit :: Integer -> Symbol
     StrLit :: String -> Symbol
     PrimOp :: Primative -> Symbol
     Quotation :: Prgm -> Symbol
     QuotID :: Name -> Symbol
     Identifier :: Name -> Symbol
     deriving Show

type Prgm = [Symbol]

data Expr :: * where
     MInt :: Integer -> Expr
     MString :: String -> Expr
     MVoid :: Expr
     MName :: Name -> Expr
     MQuotExpr :: Prgm -> Bindings -> Expr

-- Show instance so we can print the stack
instance Show Expr where
    show (MInt n) = show n
    show (MString s) = show s
    show MVoid = "()"
    show (MName nm) = '\'':nm
    show (MQuotExpr p _) = show p -- result here is ugly

type Name = String

type Stack = [Expr]

showStack :: Stack -> String
showStack = unwords . map show . reverse

-- Frame: bindings at a given scope 
-- may want to replace later (Data.Map maybe?)
type Frame = [(Name, Expr)]
data Bindings = Local Frame Bindings | Global Frame deriving Show

-- todo: add the rest of the builtins here
prelude = Global []

readBinding :: Name -> Bindings -> Maybe Expr
readBinding n (Global f)  = lookup n f
readBinding n (Local f b) = lookup n f <|> readBinding n b

insert :: Name -> Expr -> Bindings -> Bindings
insert name value (Global f)  = Global $ (name, value):f
insert name value (Local f b) = Local ((name, value):f) b

data MState :: * where
     MState :: IO () -> Stack -> Bindings -> MState

instance Semigroup MState where
    (MState a0 s0 b0) <> (MState a1 s1 b1) = MState (a0 >> a1) (s1 ++ s0) (b0 <> b1)  

instance Monoid MState where
    mempty = MState (return ()) [] prelude


-- needed to write MState mappend (maybe get rid of MState Monoid instance
--    instead?) this part doesn't make a whole lot of sense.
instance Semigroup Bindings where
  l <> (Local f b) = Local f $ l <> b
  l <> (Global f) = Local f l
instance Monoid Bindings where
  mempty = prelude

  

stack :: MState -> Stack
stack (MState _ s _) = s

action :: MState -> IO ()
action (MState a _ _) = a

bindings :: MState -> Bindings
bindings (MState _ _ b) = b

setBindings :: MState -> Bindings -> MState
setBindings st b = MState (action st) (stack st) b

setAction :: MState -> IO() -> MState
setAction st a = MState a (stack st) (bindings st)

appendAction :: IO () -> State MState Expr
appendAction act = State $ \(MState a s b) -> (MVoid, MState (a >> act) s b)

addBinding :: Name -> Expr -> State MState Expr
addBinding name value = State $ \s -> (value, MState (action s) (stack s) (insert name value (bindings s)))

lookupBinding :: Name -> State MState (Maybe Expr)
lookupBinding nm = State $ \s -> (readBinding nm (bindings s), s)

push :: Expr -> State MState Expr
push e = State $ \s -> (e, MState (action s) (e:stack s) (bindings s))

pop :: State MState Expr
pop = State $ \s -> let l = stack s
                    in (head l, MState (action s) (tail l) (bindings s))

-- error handling
instance MonadFail (State MState) where
  fail err = State $ \(MState a s b) -> (error "attempted to use error value", MState (a >> putStrLn ("Morpheus runtime error: " ++ err)) s b)


step :: Symbol -> State MState Expr
step (IntLit n) = push (MInt n)
step (StrLit s) = push (MString s)
step (PrimOp o) = stepPrim o
step (QuotID s) = push (MName s)
step (Identifier s)   = runIdentifier s
step (Quotation prgm) = State $ \s -> let
    outerScope = bindings s
    innerScope = Local [] outerScope
  in runState (push $ MQuotExpr prgm innerScope) s

stepPrim :: Primative -> State MState Expr
stepPrim (:+) = do MInt x <- pop
                   MInt y <- pop
                   let res = MInt $ y + x
                   push res
stepPrim (:-) = do MInt x <- pop
                   MInt y <- pop
                   let res = MInt $ y - x
                   push res
stepPrim Print = do p <- pop
                    appendAction (print p)
                    return p
stepPrim Bind = do MName nm <- pop
                   expr <- pop
                   addBinding nm expr
                   return expr
stepPrim Exec = do MQuotExpr quot binds <- pop
                   State $ \(MState a s b) -> let (res, st) = runEnv quot (MState a s binds)
                                              in (res, setBindings st b)
                   

-- this will need to become more sophisticated to deal with morphemes
runIdentifier :: Name -> State MState Expr
runIdentifier nm = do Just value <- lookupBinding nm
                      push value

run :: Prgm -> (Expr, MState)
run = flip runEnv mempty

-- run starting from a given environment (state)
runEnv :: Prgm -> MState -> (Expr, MState)
runEnv p = runState (foldl (>>) (return MVoid) $ map step p)
