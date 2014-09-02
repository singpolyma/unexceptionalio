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
	unsafeFromIO
) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap, (<=<))
import Control.Monad.Fix (MonadFix(..))
import Control.Error (syncIO, mapEitherT, EitherT(..), fmapLT)
import Control.Exception (SomeException, Exception, fromException, throwIO)

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
fromIO :: IO a -> EitherT SomeException UnexceptionalIO a
fromIO = mapEitherT unsafeFromIO . syncIO

-- | Re-embed 'UnexceptionalIO' into 'IO'
runUnexceptionalIO :: UnexceptionalIO a -> IO a
runUnexceptionalIO (UnexceptionalIO io) = io

-- | Re-embed 'UnexceptionalIO' and possible exception back into 'IO'
runEitherIO :: (Exception e) => EitherT e UnexceptionalIO a -> IO a
runEitherIO = either throwIO return <=< runUnexceptionalIO . runEitherT

-- | You promise that 'e' covers all non-error, synchronous exceptions
--   thrown by this 'IO' action
--
-- This function is partial if you lie
fromIO' :: (Exception e) => IO a -> EitherT e UnexceptionalIO a
fromIO' = fmapLT (maybePartial . fromException) . fromIO
	where
	maybePartial (Just x) = x
	maybePartial Nothing = error "UnexceptionalIO.fromIO' exception of unspecified type"

-- | You promise there are no exceptions thrown by this 'IO' action
unsafeFromIO :: IO a -> UnexceptionalIO a
unsafeFromIO = UnexceptionalIO
