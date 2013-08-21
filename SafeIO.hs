module SafeIO (SafeIO, unsafeFromIO, fromIO, runSafeIO) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Fix (MonadFix(..))
import Control.Error (syncIO, EitherT(..))
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO, MonadIO)

newtype SafeIO a = SafeIO (IO a)

instance Functor SafeIO where
	fmap = liftM

instance Applicative SafeIO where
	pure = return
	(<*>) = ap

instance Monad SafeIO where
	return = SafeIO . return
	(SafeIO x) >>= f = SafeIO (x >>= f')
		where
		f' x = let SafeIO io = f x in io

	fail = error "SafeIO does not have exceptions."

instance MonadFix SafeIO where
	mfix f = SafeIO (mfix f')
		where
		f' x = let SafeIO io = f x in io

unsafeFromIO :: IO a -> SafeIO a
unsafeFromIO = SafeIO

fromIO :: IO a -> EitherT SomeException SafeIO a
fromIO = EitherT . unsafeFromIO . runEitherT . syncIO

runSafeIO :: (MonadIO m) => SafeIO a -> m a
runSafeIO (SafeIO io) = liftIO io