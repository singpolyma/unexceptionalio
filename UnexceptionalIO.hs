{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
#endif
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
#ifdef __GLASGOW_HASKELL__
	fromIO',
#endif
	run,
	runEitherIO,
	-- * Unsafe entry points
	unsafeFromIO,
	-- * Pseudo exceptions
	SomeNonPseudoException,
#if MTL_SUPPORT
        HasSomeNonPseudoException(fromSomeNonPseudoException),
        liftIO,
#endif
#ifdef __GLASGOW_HASKELL__
	PseudoException(..),
	ProgrammerError(..),
	ExternalError(..),
	-- * Pseudo exception helpers
	bracket,
#if MIN_VERSION_base(4,7,0)
	forkFinally,
	fork,
	ChildThreadError(..)
#endif
#endif
) where

import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative(..), (<|>), (<$>))
import Control.Monad (liftM, ap, (<=<))
import Control.Monad.Fix (MonadFix(..))
#if MTL_SUPPORT
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import qualified Control.Monad.Trans as Trans (MonadTrans(lift))
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT)
#endif
#ifdef __GLASGOW_HASKELL__
import System.Exit (ExitCode)
import Control.Exception (try)
import Data.Typeable (Typeable)
import qualified Control.Exception as Ex
import qualified Control.Concurrent as Concurrent
#if MIN_VERSION_base(4,11,0)
import qualified Control.Exception.Base as Ex
#endif

-- | Not everything handled by the exception system is a run-time error
-- you can handle.  This is the class of unrecoverable pseudo-exceptions.
--
-- Additionally, except for 'ExitCode' any of these pseudo-exceptions
-- you could never guarantee to have caught.  Since they can come
-- from anywhere at any time, we could never guarentee that 'UIO' does
-- not contain them.
data PseudoException =
	ProgrammerError ProgrammerError | -- ^ Mistakes programmers make
	ExternalError   ExternalError   | -- ^ Errors thrown by the runtime
	Exit ExitCode                     -- ^ Process exit requests
	deriving (Show, Typeable)

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
	deriving (Show, Typeable)

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
	deriving (Show, Typeable)

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
newtype SomeNonPseudoException = SomeNonPseudoException Ex.SomeException deriving (Show, Typeable)

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

#if MTL_SUPPORT
-- Instance for standard monad transformers
instance Unexceptional m => Unexceptional (ExceptT e m) where
  lift = Trans.lift . lift
instance Unexceptional m => Unexceptional (ReaderT r m) where
  lift = Trans.lift . lift
instance (Unexceptional m, Monoid w) => Unexceptional (RWST r w s m) where
  lift = Trans.lift . lift
instance Unexceptional m => Unexceptional (StateT s m) where
  lift = Trans.lift . lift
instance (Unexceptional m, Monoid w) => Unexceptional (WriterT w m) where
  lift = Trans.lift . lift

-- | If you are using MonadError you will want to create
-- 'HasSomeNonPseudoException' instances so that the error
-- type can receive such errors via 'withExceptT'.
class HasSomeNonPseudoException e where
  fromSomeNonPseudoException :: SomeNonPseudoException -> e
instance HasSomeNonPseudoException SomeNonPseudoException where
  fromSomeNonPseudoException = id

-- | A function with an intentional name conflict with the MonadIO
-- method 'Control.Monad.IO.Class.liftIO'.
liftIO :: (MonadError e m, Unexceptional m, HasSomeNonPseudoException e) => IO a -> m a
liftIO io = fromIO io >>= either (throwError . fromSomeNonPseudoException) return
#endif

-- | Like IO, but throws only 'PseudoException'
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

-- | Monads in which 'UIO' computations may be embedded
class (Monad m) => Unexceptional m where
	lift :: UIO a -> m a

instance Unexceptional UIO where
	lift = id

instance Unexceptional IO where
	lift = run

-- | Catch any exception but 'PseudoException' in an 'IO' action
fromIO :: (Unexceptional m) => IO a -> m (Either SomeNonPseudoException a)
fromIO = unsafeFromIO . try

#ifdef __GLASGOW_HASKELL__
-- | Catch any 'e' in an 'IO' action, with a default mapping for
--   unexpected cases
fromIO' :: (Ex.Exception e, Unexceptional m) =>
	(SomeNonPseudoException -> e) -- ^ Default if an unexpected exception occurs
	-> IO a
	-> m (Either e a)
fromIO' f = (return . either (\e -> Left $ fromMaybe (f e) $ castException e) Right) <=< fromIO

castException :: (Ex.Exception e1, Ex.Exception e2) => e1 -> Maybe e2
castException = Ex.fromException . Ex.toException
#endif

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

-- | You promise there are no exceptions but 'PseudoException' thrown by this 'IO' action
unsafeFromIO :: (Unexceptional m) => IO a -> m a
unsafeFromIO = lift . UIO

#ifdef __GLASGOW_HASKELL__
-- | When you're doing resource handling, 'PseudoException' matters.
--   You still need to use the 'Ex.bracket' pattern to handle cleanup.
bracket :: (Unexceptional m) => UIO a -> (a -> UIO ()) -> (a -> UIO c) -> m c
bracket acquire release body =
	unsafeFromIO $ Ex.bracket (run acquire) (run . release) (run . body)

#if MIN_VERSION_base(4,7,0)
-- | Mirrors 'Concurrent.forkFinally', but since the body is 'UIO',
--   the thread must terminate successfully or because of 'PseudoException'
forkFinally :: (Unexceptional m) => UIO a -> (Either PseudoException a -> UIO ()) -> m Concurrent.ThreadId
forkFinally body handler = unsafeFromIO $ Concurrent.forkFinally (run body) $ \result ->
	case result of
		Left e -> case Ex.fromException e of
			Just pseudo -> run $ handler $ Left pseudo
			Nothing -> error $ "Bug in UnexceptionalIO: forkFinally caught a non-PseudoException: " ++ show e
		Right x -> run $ handler $ Right x

-- | Mirrors 'Concurrent.forkIO', but re-throws errors to the parent thread
--
-- * Ignores manual thread kills, since those are on purpose.
-- * Re-throws async exceptions ('SomeAsyncException') as is.
-- * Re-throws 'ExitCode' as is in an attempt to exit with the requested code.
-- * Wraps synchronous 'PseudoException' in async 'ChildThreadError'.
fork :: (Unexceptional m) => UIO () -> m Concurrent.ThreadId
fork body = do
	parent <- unsafeFromIO Concurrent.myThreadId
	forkFinally body $ either (handler parent) (const $ return ())
	where
	handler parent e
		-- Thread manually killed. I assume on purpose
		| Just Ex.ThreadKilled <- castException e = return ()
		-- Async exception, nothing to do with this thread, propogate directly
		| Just (Ex.SomeAsyncException _) <- castException e =
			unsafeFromIO $ Concurrent.throwTo parent e
		-- Attempt to manually end the process,
		-- not an async exception, so a bit dangerous to throw async'ly, but
		-- you really do want this to reach the top as-is for the exit code to
		-- work.
		| Just e <- castException e =
			unsafeFromIO $ Concurrent.throwTo parent (e :: ExitCode)
		-- Non-async PseudoException, so wrap in an async wrapper before
		-- throwing async'ly
		| otherwise = unsafeFromIO $ Concurrent.throwTo parent (ChildThreadError e)

-- | Async signal that a child thread ended due to non-async PseudoException
newtype ChildThreadError = ChildThreadError PseudoException deriving (Show, Typeable)

instance Ex.Exception ChildThreadError where
	toException = Ex.asyncExceptionToException
	fromException = Ex.asyncExceptionFromException
#endif
#endif
