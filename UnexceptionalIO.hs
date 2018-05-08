{-# LANGUAGE CPP #-}
-- | When you've caught all the exceptions that can be handled safely,
--   this is what you're left with.
--
-- > runEitherIO . fromIO ≡ id
--
-- It is intended that you use qualified imports with this library.
--
-- > import UnexceptionalIO (UIO)
-- > import qualified UnexceptionalIO as UIO
module UnexceptionalIO (
	UIO,
	Unexceptional(..),
	fromIO,
	run,
	runEitherIO,
	-- * Unsafe entry points
#ifdef __GLASGOW_HASKELL__
	fromIO',
#endif
	unsafeFromIO,
	-- * Pseudo exceptions
	SomeNonPseudoException,
#ifdef __GLASGOW_HASKELL__
	PseudoException(..),
	ProgrammerError(..),
	ExternalError(..),
	-- * Pseudo exception helpers
	bracket,
#if MIN_VERSION_base(4,6,0)
	forkFinally,
	fork
#endif
#endif
) where

import Control.Applicative (Applicative(..), (<|>))
import Control.Monad (liftM, ap, (<=<))
import Control.Monad.Fix (MonadFix(..))
#ifdef __GLASGOW_HASKELL__
import System.Exit (ExitCode)
import Control.Exception (try)
import qualified Control.Exception as Ex
import qualified Control.Concurrent as Concurrent
#if MIN_VERSION_base(4,11,0)
import qualified Control.Exception.Base as Ex
#endif

-- | Not everything handled by the exception system is a run-time error
-- you can handle.  This is the class of pseudo-exceptions you usually
-- can do nothing about, just log or exit.
--
-- Additionally, except for 'ExitCode' any of these psuedo-exceptions
-- you could never guarentee to have caught, since they can come
-- from anywhere at any time, we could never guarentee that 'UIO' does
-- not contain them.
data PseudoException =
	ProgrammerError ProgrammerError | -- ^ Mistakes programmers make
	ExternalError   ExternalError   | -- ^ Errors thrown by the runtime
	Exit ExitCode                     -- ^ Process exit requests
	deriving (Show)

instance Ex.Exception PseudoException where
	toException (ProgrammerError e) = Ex.toException e
	toException (ExternalError e)   = Ex.toException e
	toException (Exit e)            = Ex.toException e

	fromException e =
		ProgrammerError <$> Ex.fromException e <|>
		ExternalError   <$> Ex.fromException e <|>
		Exit            <$> Ex.fromException e

-- | Pseudo-exceptions caused by a programming error
--
-- Partial functions, 'error', 'undefined', etc
data ProgrammerError =
#if MIN_VERSION_base(4,9,0)
	TypeError Ex.TypeError               |
#endif
	ArithException Ex.ArithException     |
	ArrayException Ex.ArrayException     |
	AssertionFailed Ex.AssertionFailed   |
	ErrorCall Ex.ErrorCall               |
	NestedAtomically Ex.NestedAtomically |
	NoMethodError Ex.NoMethodError       |
	PatternMatchFail Ex.PatternMatchFail |
	RecConError Ex.RecConError           |
	RecSelError Ex.RecSelError           |
	RecUpdError Ex.RecSelError
	deriving (Show)

instance Ex.Exception ProgrammerError where
#if MIN_VERSION_base(4,9,0)
	toException (TypeError e)           = Ex.toException e
#endif
	toException (ArithException e)      = Ex.toException e
	toException (ArrayException e)      = Ex.toException e
	toException (AssertionFailed e)     = Ex.toException e
	toException (ErrorCall e)           = Ex.toException e
	toException (NestedAtomically e)    = Ex.toException e
	toException (NoMethodError e)       = Ex.toException e
	toException (PatternMatchFail e)    = Ex.toException e
	toException (RecConError e)         = Ex.toException e
	toException (RecSelError e)         = Ex.toException e
	toException (RecUpdError e)         = Ex.toException e

	fromException e =
#if MIN_VERSION_base(4,9,0)
		TypeError        <$> Ex.fromException e <|>
#endif
		ArithException   <$> Ex.fromException e <|>
		ArrayException   <$> Ex.fromException e <|>
		AssertionFailed  <$> Ex.fromException e <|>
		ErrorCall        <$> Ex.fromException e <|>
		NestedAtomically <$> Ex.fromException e <|>
		NoMethodError    <$> Ex.fromException e <|>
		PatternMatchFail <$> Ex.fromException e <|>
		RecConError      <$> Ex.fromException e <|>
		RecSelError      <$> Ex.fromException e <|>
		RecUpdError      <$> Ex.fromException e

-- | Pseudo-exceptions thrown by the runtime environment
data ExternalError =
#if MIN_VERSION_base(4,10,0)
	CompactionFailed Ex.CompactionFailed                   |
#endif
#if MIN_VERSION_base(4,11,0)
	FixIOException Ex.FixIOException                       |
#endif
#if MIN_VERSION_base(4,7,0)
	AsyncException Ex.SomeAsyncException                   |
#else
	AsyncException Ex.AsyncException                       |
#endif
	BlockedIndefinitelyOnSTM Ex.BlockedIndefinitelyOnSTM   |
	BlockedIndefinitelyOnMVar Ex.BlockedIndefinitelyOnMVar |
	Deadlock Ex.Deadlock                                   |
	NonTermination Ex.NonTermination
	deriving (Show)

instance Ex.Exception ExternalError where
#if MIN_VERSION_base(4,10,0)
	toException (CompactionFailed e)          = Ex.toException e
#endif
#if MIN_VERSION_base(4,11,0)
	toException (FixIOException e)            = Ex.toException e
#endif
	toException (AsyncException e)            = Ex.toException e
	toException (BlockedIndefinitelyOnMVar e) = Ex.toException e
	toException (BlockedIndefinitelyOnSTM e)  = Ex.toException e
	toException (Deadlock e)                  = Ex.toException e
	toException (NonTermination e)            = Ex.toException e

	fromException e =
#if MIN_VERSION_base(4,10,0)
		CompactionFailed          <$> Ex.fromException e <|>
#endif
#if MIN_VERSION_base(4,11,0)
		FixIOException            <$> Ex.fromException e <|>
#endif
		AsyncException            <$> Ex.fromException e <|>
		BlockedIndefinitelyOnSTM  <$> Ex.fromException e <|>
		BlockedIndefinitelyOnMVar <$> Ex.fromException e <|>
		Deadlock                  <$> Ex.fromException e <|>
		NonTermination            <$> Ex.fromException e

-- | Every 'Ex.SomeException' but 'PseudoException'
newtype SomeNonPseudoException = SomeNonPseudoException Ex.SomeException deriving (Show)

instance Ex.Exception SomeNonPseudoException where
	toException (SomeNonPseudoException e) = e

	fromException e = case Ex.fromException e of
		Just pseudo -> const Nothing (pseudo :: PseudoException)
		Nothing -> Just (SomeNonPseudoException e)

throwIO :: (Ex.Exception e) => e -> IO a
throwIO = Ex.throwIO
#else
-- Haskell98 import 'IO' instead
import System.IO.Error (IOError, ioError, try)

type SomeNonPseudoException = IOError

throwIO :: SomeNonPseudoException -> IO a
throwIO = ioError
#endif

-- | IO without any 'PseudoException'
newtype UIO a = UIO (IO a)

instance Functor UIO where
	fmap = liftM

instance Applicative UIO where
	pure = return
	(<*>) = ap

instance Monad UIO where
	return = UIO . return
	(UIO x) >>= f = UIO (x >>= run . f)

	fail s = error $ "UnexceptionalIO cannot fail (" ++ s ++ ")"

instance MonadFix UIO where
	mfix f = UIO (mfix $ run . f)

-- | Polymorphic base without any 'PseudoException'
class (Monad m) => Unexceptional m where
	lift :: UIO a -> m a

instance Unexceptional UIO where
	lift = id

instance Unexceptional IO where
	lift = run

-- | Catch any exception but 'PseudoException' in an 'IO' action
fromIO :: (Unexceptional m) => IO a -> m (Either SomeNonPseudoException a)
fromIO = unsafeFromIO . try

-- | Re-embed 'UIO' into 'IO'
run :: UIO a -> IO a
run (UIO io) = io

-- | Re-embed 'UIO' and possible exception back into 'IO'
#ifdef __GLASGOW_HASKELL__
runEitherIO :: (Ex.Exception e) => UIO (Either e a) -> IO a
#else
runEitherIO :: UIO (Either SomeNonPseudoException a) -> IO a
#endif
runEitherIO = either throwIO return <=< run

#ifdef __GLASGOW_HASKELL__
-- | You promise that 'e' covers all exceptions but 'PseudoException'
--   thrown by this 'IO' action
--
-- This function is partial if you lie
fromIO' :: (Ex.Exception e, Unexceptional m) => IO a -> m (Either e a)
fromIO' =
	(return . either (Left . maybePartial . Ex.fromException . Ex.toException) Right) <=< fromIO
	where
	maybePartial (Just x) = x
	maybePartial Nothing = error "UnexceptionalIO.fromIO' exception of unspecified type"
#endif

-- | You promise there are no exceptions but 'PseudoException' thrown by this 'IO' action
unsafeFromIO :: (Unexceptional m) => IO a -> m a
unsafeFromIO = lift . UIO

#ifdef __GLASGOW_HASKELL__
-- | When you're doing resource handling, 'PseudoException' matters.
--   You still need to use the 'Ex.bracket' pattern to handle cleanup.
bracket :: (Unexceptional m) => UIO a -> (a -> UIO ()) -> (a -> UIO c) -> m c
bracket acquire release body =
	unsafeFromIO $ Ex.bracket (run acquire) (run . release) (run . body)

#if MIN_VERSION_base(4,6,0)
-- | Mirrors 'Concurrent.forkFinally', but since the body is 'UIO',
--   the thread must terminate successfully or because of 'PseudoException'
forkFinally :: (Unexceptional m) => UIO a -> (Either PseudoException a -> UIO ()) -> m Concurrent.ThreadId
forkFinally body handler = unsafeFromIO $ Concurrent.forkFinally (run body) $ \result ->
	case result of
		Left e -> case Ex.fromException e of
			Just pseudo -> run $ handler $ Left pseudo
			Nothing -> error $ "Bug in UnexceptionalIO: forkFinally caught a non-PseudoException: " ++ show e
		Right x -> run $ handler $ Right x

-- | Mirrors 'Concurrent.forkIO', but re-throws any 'PseudoException'
--   to the parent thread
fork :: (Unexceptional m) => UIO () -> m Concurrent.ThreadId
fork body = do
	parent <- unsafeFromIO Concurrent.myThreadId
	forkFinally body $
		either (unsafeFromIO . Concurrent.throwTo parent) (const $ return ())
#endif
#endif
