{-# LANGUAGE CPP #-}
-- | When you've caught all the exceptions that can be handled safely,
--   this is what you're left with.
--
-- > runEitherIO . fromIO ≡ id
module UnexceptionalIO (
	UIO,
	Unexceptional(..),
	fromIO,
	runUIO,
	runEitherIO,
	-- * Unsafe entry points
#ifdef __GLASGOW_HASKELL__
	fromIO',
#endif
	unsafeFromIO,
	-- * Utilities
	syncIO
) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap, (<=<))
import Control.Monad.Fix (MonadFix(..))
#ifdef __GLASGOW_HASKELL__
import Data.Dynamic (Dynamic)
import System.Exit (ExitCode)
import qualified Control.Exception as Ex

type SomeException = Ex.SomeException

throwIO :: (Ex.Exception e) => e -> IO a
throwIO = Ex.throwIO
#else
-- Haskell98 import 'IO' instead
import System.IO.Error (IOError, ioError, try)

type SomeException = IOError

throwIO :: SomeException -> IO a
throwIO = ioError
#endif

-- | IO without any non-error, synchronous exceptions
newtype UIO a = UIO (IO a)

instance Functor UIO where
	fmap = liftM

instance Applicative UIO where
	pure = return
	(<*>) = ap

instance Monad UIO where
	return = UIO . return
	(UIO x) >>= f = UIO (x >>= runUIO . f)

	fail s = error $ "UnexceptionalIO cannot fail (" ++ s ++ ")"

instance MonadFix UIO where
	mfix f = UIO (mfix $ runUIO . f)

-- | Polymorphic base without any non-error, synchronous exceptions
class Unexceptional m where
	liftUIO :: UIO a -> m a

instance Unexceptional UIO where
	liftUIO = id

instance Unexceptional IO where
	liftUIO = runUIO

-- | Catch any non-error, synchronous exceptions in an 'IO' action
fromIO :: IO a -> UIO (Either SomeException a)
fromIO = unsafeFromIO . syncIO

-- | Re-embed 'UIO' into 'IO'
runUIO :: UIO a -> IO a
runUIO (UIO io) = io

-- | Re-embed 'UIO' and possible exception back into 'IO'
#ifdef __GLASGOW_HASKELL__
runEitherIO :: (Ex.Exception e) => UIO (Either e a) -> IO a
#else
runEitherIO :: UIO (Either SomeException a) -> IO a
#endif
runEitherIO = either throwIO return <=< runUIO

#ifdef __GLASGOW_HASKELL__
-- | You promise that 'e' covers all non-error, synchronous exceptions
--   thrown by this 'IO' action
--
-- This function is partial if you lie
fromIO' :: (Ex.Exception e) => IO a -> UIO (Either e a)
fromIO' =
	(return . either (Left . maybePartial . Ex.fromException) Right) <=< fromIO
	where
	maybePartial (Just x) = x
	maybePartial Nothing = error "UnexceptionalIO.fromIO' exception of unspecified type"
#endif

-- | You promise there are no exceptions thrown by this 'IO' action
unsafeFromIO :: IO a -> UIO a
unsafeFromIO = UIO

-- | Catch all exceptions, except for asynchronous exceptions found in @base@
syncIO :: IO a -> IO (Either SomeException a)
#ifdef __GLASGOW_HASKELL__
syncIO a = Ex.catches (fmap Right a) [
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.ArithException)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.ArrayException)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.AssertionFailed)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.AsyncException)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.BlockedIndefinitelyOnMVar)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.BlockedIndefinitelyOnSTM)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.Deadlock)),
		Ex.Handler (\e -> Ex.throwIO (e :: Dynamic)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.ErrorCall)),
		Ex.Handler (\e -> Ex.throwIO (e :: ExitCode)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.NestedAtomically)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.NoMethodError)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.NonTermination)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.PatternMatchFail)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.RecConError)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.RecSelError)),
		Ex.Handler (\e -> Ex.throwIO (e :: Ex.RecUpdError)),
		Ex.Handler (return . Left)
	]
#else
syncIO = try
#endif
