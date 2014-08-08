-- | When you've caught all the exceptions that can be handled safely,
--   this is what you're left with.
--
-- > runEitherIO . fromIO ≡ id
module UnexceptionalIO (
  UnexceptionalIO,
  UIO,
  fromIO,
  runUnexceptionalIO,
  -- * Unsafe entry points
  fromIO',
  unsafeFromIO
) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap, (<=<))
import Control.Monad.Fix (MonadFix(..))
import Control.Arrow (left)
import Control.Exception
import Data.Dynamic (Dynamic)
import System.Exit (ExitCode)

-- | IO without any non-error, synchronous exceptions
newtype UnexceptionalIO a = UnexceptionalIO (IO a)

-- | A convenience alias to 'UnexceptionalIO'
type UIO = UnexceptionalIO

instance Functor UnexceptionalIO where
  fmap = liftM

instance Applicative UnexceptionalIO where
  pure = return
  (<*>) = ap

instance Monad UnexceptionalIO where
  return = UnexceptionalIO . return
  UnexceptionalIO x >>= f = UnexceptionalIO (x >>= runUnexceptionalIO . f)

  fail s = error $ "UnexceptionalIO cannot fail (" ++ s ++ ")"

instance MonadFix UnexceptionalIO where
  mfix f = UnexceptionalIO (mfix $ runUnexceptionalIO . f)

-- | Catch any non-error, synchronous exceptions in an 'IO' action
fromIO :: IO a -> UnexceptionalIO (Either SomeException a)
fromIO = UnexceptionalIO . flip catches handlers . fmap Right
  where
  handlers =
    [ 
      Handler $ \e -> throwIO (e :: ArithException),
      Handler $ \e -> throwIO (e :: ArrayException),
      Handler $ \e -> throwIO (e :: AssertionFailed),
      Handler $ \e -> throwIO (e :: AsyncException),
      Handler $ \e -> throwIO (e :: BlockedIndefinitelyOnMVar),
      Handler $ \e -> throwIO (e :: BlockedIndefinitelyOnSTM),
      Handler $ \e -> throwIO (e :: Deadlock),
      Handler $ \e -> throwIO (e :: Dynamic),
      Handler $ \e -> throwIO (e :: ErrorCall),
      Handler $ \e -> throwIO (e :: ExitCode),
      Handler $ \e -> throwIO (e :: NestedAtomically),
      Handler $ \e -> throwIO (e :: NoMethodError),
      Handler $ \e -> throwIO (e :: NonTermination),
      Handler $ \e -> throwIO (e :: PatternMatchFail),
      Handler $ \e -> throwIO (e :: RecConError),
      Handler $ \e -> throwIO (e :: RecSelError),
      Handler $ \e -> throwIO (e :: RecUpdError),
      Handler $ return . Left
    ]

-- | Re-embed 'UnexceptionalIO' into 'IO'
runUnexceptionalIO :: UnexceptionalIO a -> IO a
runUnexceptionalIO (UnexceptionalIO io) = io

-- | You promise that 'e' covers all non-error, synchronous exceptions
--   thrown by this 'IO' action
--
-- This function is partial if you lie
fromIO' :: Exception e => IO a -> UnexceptionalIO (Either e a)
fromIO' = (fmap . left) (maybePartial . fromException) . fromIO
  where
  maybePartial (Just x) = x
  maybePartial Nothing = error "UnexceptionalIO.fromIO' exception of unspecified type"

-- | You promise there are no exceptions thrown by this 'IO' action
unsafeFromIO :: IO a -> UnexceptionalIO a
unsafeFromIO = UnexceptionalIO
