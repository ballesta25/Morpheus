{-
copy of State as in LYaH - no longer seems to be in the standard libraries
updated for GHC 7.10 changes (Monad a subclass of Applicative)
-}

module State where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    fmap f (State h) = State $ \s -> let (x, newState) = h s
                                     in  (f x, newState)

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    (State f) <*> (State h) = State $ \s -> let (g, newState) = f s
                                                (a, b) = h newState
                                            in  (g a, b)

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState
