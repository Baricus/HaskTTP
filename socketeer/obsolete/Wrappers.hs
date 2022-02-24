{-# LANGUAGE ViewPatterns #-}

module Wrappers where

import Handlers (Handler, Connection)

-- unWrap :: a -> IO b
newtype Wrap a b = Wrap {unWrap :: Handler a b}

instance (Semigroup b) => Semigroup (Wrap a b) where
    (<>) (unWrap -> hnew) (unWrap -> hOld) = Wrap $ hnew <> hOld

instance Functor (Wrap a) where
    fmap f (unWrap -> handle) = Wrap $ 
      (\val -> do
        res <- handle val
        pure $ f res)

instance Applicative (Wrap a) where
    (<*>) (unWrap -> f) (unWrap -> prior) = Wrap $ (\x -> do
                -- execute both IO actions to get the results
                x' <- prior x
                f' <- f x
                -- combine them together
                pure $ f' x')

    pure con = Wrap $ const $ pure $ con

instance Monad (Wrap a) where
    -- (>>=) :: Wrap a b -> (b -> Wrap a c) -> Wrap a c
    (>>=) (unWrap -> handl) mf =
        Wrap $ (\xa ->
                do
                    resb <- handl xa
                    let handl' = unWrap $ mf resb
                    handl' xa)
        
-- | Ends a wrap by providing a base handler
-- This also handles the endline because reasons
(|>) :: Wrap a () -> Handler a () -> Wrap a ()
(|>) (unWrap -> wrap) (handler) = Wrap $ wrap <> (const $ putStrLn "") <> handler


-- | a wrapper which prints the connection address
-- and passes along
printWrap :: Wrap Connection ()
printWrap = Wrap (\(_, addr) -> putStr $ "Connection from: " ++ show addr ++ " -- ")


-- | Ignore all input and pass ()
dropIn :: Wrap a ()
dropIn = Wrap $ const $ pure ()
