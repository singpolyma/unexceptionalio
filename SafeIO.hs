-- A type of IO that does not contain any non-error, synchronous exceptions
module SafeIO (SafeIO, unsafeFromIO, fromIO, fromIO', runSafeIO, runEitherIO) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap, (<=<))
import Control.Monad.Fix (MonadFix(..))
import Control.Error (syncIO, mapEitherT, EitherT(..), fmapLT)
import Control.Exception (SomeException, Exception, fromException, throwIO)
import Control.Monad.IO.Class (liftIO, MonadIO)

newtype SafeIO a = SafeIO (IO a)

instance Functor SafeIO where
	fmap = liftM

instance Applicative SafeIO where
	pure = return
	(<*>) = ap

instance Monad SafeIO where
	return = SafeIO . return
	(SafeIO x) >>= f = SafeIO (x >>= runSafeIO . f)

	fail = error "SafeIO does not have exceptions."

instance MonadFix SafeIO where
	mfix f = SafeIO (mfix $ runSafeIO . f)

-- | You promise there are no exceptions thrown by this IO action
unsafeFromIO :: IO a -> SafeIO a
unsafeFromIO = SafeIO

-- | Lift IO action and catch any non-error synchronous exceptions
fromIO :: IO a -> EitherT SomeException SafeIO a
fromIO = mapEitherT unsafeFromIO . syncIO

-- | You promise that e covers all exceptions thrown by this IO action
-- This function is partial if you lie
fromIO' :: (Exception e) => IO a -> EitherT e SafeIO a
fromIO' = fmapLT (maybePartial . fromException) . fromIO
	where
	maybePartial (Just x) = x
	maybePartial Nothing = error "SafeIO.fromIO' exception of unspecified type"

-- | Re-embed SafeIO into IO
runSafeIO :: (MonadIO m) => SafeIO a -> m a
runSafeIO (SafeIO io) = liftIO io

-- | Re-embed SafeIO with possible exception back into IO
runEitherIO :: (MonadIO m, Exception e) => EitherT e SafeIO a -> m a
runEitherIO = either (liftIO . throwIO) return <=< runSafeIO . runEitherT