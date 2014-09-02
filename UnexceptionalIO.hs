-- | When you've caught all the exceptions that can be handled safely,
--   this is what you're left with.
--
-- > runEitherIO . fromIO ≡ id
module UnexceptionalIO (
	UnexceptionalIO,
	fromIO,
	runUnexceptionalIO,
	runEitherIO,
	-- * Unsafe entry points
	fromIO',
	unsafeFromIO,
	-- * Utilities
	syncIO
) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap, (<=<))
import Control.Monad.Fix (MonadFix(..))
import Data.Dynamic (Dynamic)
import System.Exit (ExitCode)
import qualified Control.Exception as Ex

-- | IO without any non-error, synchronous exceptions
newtype UnexceptionalIO a = UnexceptionalIO (IO a)

instance Functor UnexceptionalIO where
	fmap = liftM

instance Applicative UnexceptionalIO where
	pure = return
	(<*>) = ap

instance Monad UnexceptionalIO where
	return = UnexceptionalIO . return
	(UnexceptionalIO x) >>= f = UnexceptionalIO (x >>= runUnexceptionalIO . f)

	fail s = error $ "UnexceptionalIO cannot fail (" ++ s ++ ")"

instance MonadFix UnexceptionalIO where
	mfix f = UnexceptionalIO (mfix $ runUnexceptionalIO . f)

-- | Catch any non-error, synchronous exceptions in an 'IO' action
fromIO :: IO a -> UnexceptionalIO (Either Ex.SomeException a)
fromIO = unsafeFromIO . syncIO

-- | Re-embed 'UnexceptionalIO' into 'IO'
runUnexceptionalIO :: UnexceptionalIO a -> IO a
runUnexceptionalIO (UnexceptionalIO io) = io

-- | Re-embed 'UnexceptionalIO' and possible exception back into 'IO'
runEitherIO :: (Ex.Exception e) => UnexceptionalIO (Either e a) -> IO a
runEitherIO = either Ex.throwIO return <=< runUnexceptionalIO

-- | You promise that 'e' covers all non-error, synchronous exceptions
--   thrown by this 'IO' action
--
-- This function is partial if you lie
fromIO' :: (Ex.Exception e) => IO a -> UnexceptionalIO (Either e a)
fromIO' =
	(return . either (Left . maybePartial . Ex.fromException) Right) <=< fromIO
	where
	maybePartial (Just x) = x
	maybePartial Nothing = error "UnexceptionalIO.fromIO' exception of unspecified type"

-- | You promise there are no exceptions thrown by this 'IO' action
unsafeFromIO :: IO a -> UnexceptionalIO a
unsafeFromIO = UnexceptionalIO

-- | Catch all exceptions, except for asynchronous exceptions found in @base@
syncIO :: IO a -> IO (Either Ex.SomeException a)
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
