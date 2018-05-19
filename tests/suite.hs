{-# LANGUAGE CPP, DeriveDataTypeable #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)

import Data.Typeable (Typeable)
import Control.Monad
import System.Exit
import qualified Control.Exception as Ex

import qualified UnexceptionalIO as UIO

data CustomException = CustomException deriving (Show, Typeable)
instance Ex.Exception CustomException

class TestClass a where
	testClassMethod :: a -> ()

data CantTestClass = CantTestClass
instance TestClass CantTestClass

data BadRecord = BadRecord { badfld :: String } | OtherBadRecord { otherfld :: String }

fromIOCatches :: IO () -> Assertion
fromIOCatches io = do
	caught <- UIO.run $ UIO.fromIO io
	either
		(const $ return ())
		(\x -> assertFailure $ "fromIO did not catch: " ++ show x)
		caught

fromIOPasses :: IO () -> Assertion
fromIOPasses io = do
	caught <- Ex.try $ UIO.run $ UIO.fromIO io
	either
		(\(Ex.SomeException _) -> return ())
		(\x -> assertFailure $ "fromIO caught: " ++ show x)
		caught

tests :: [Test]
tests =
	[
		testGroup "fromIO catches runtime errors" [
			testCase "fail" (fromIOCatches $ fail "boo"),
			testCase "userError" (fromIOCatches $ Ex.throwIO $ userError "boo"),
			testCase "CustomException" (fromIOCatches $ Ex.throwIO CustomException)
		],
		testGroup "fromIO passes through programmer errors" [
#if MIN_VERSION_base(4,9,0)
			testCase "TypeError" (fromIOPasses $ Ex.throwIO $ Ex.TypeError "boo"),
#endif
			testCase "error" (fromIOPasses $ error "boo"),
			testCase "undefined" (fromIOPasses undefined),
			testCase "ArithException" (fromIOPasses $ void (return $! 1 `div` 0)),
			testCase "assert" (fromIOPasses $ Ex.assert False (return ())),
			testCase "pattern match fail" (fromIOPasses $ (\(Just x) -> return ()) Nothing),
			testCase "array out of bounds" (fromIOPasses $ Ex.throwIO $ Ex.IndexOutOfBounds "boo"),
			testCase "array uninitialized" (fromIOPasses $ Ex.throwIO $ Ex.UndefinedElement "boo"),
			testCase "no method" (fromIOPasses $ print $ testClassMethod CantTestClass),
			testCase "use uninitialized record field" (fromIOPasses $ print $ badfld BadRecord {}),
			testCase "use not present record field" (fromIOPasses $ print $ otherfld BadRecord {}),
			testCase "update not present record field" (fromIOPasses $ void (return $! (BadRecord {} { otherfld = "hai" })))
		],
		testGroup "fromIO passes through termination" [
#if MIN_VERSION_base(4,8,0)
			testCase "die" (fromIOPasses $ die "exit time"),
#endif
			testCase "exitSuccess" (fromIOPasses exitSuccess),
			testCase "exitFailure" (fromIOPasses exitFailure)
		],
		testGroup "fromIO passes through exceptions from the runtime" [
#if MIN_VERSION_base(4,8,0)
			testCase "AllocationLimitExceeded" (fromIOPasses $ Ex.throwIO Ex.AllocationLimitExceeded),
#endif
#if MIN_VERSION_base(4,7,0)
			testCase "ChildThreadError" (fromIOPasses $ Ex.throwIO $ UIO.ChildThreadError $ UIO.ProgrammerError $ UIO.ArithException Ex.DivideByZero),
#endif
			testCase "NonTermination" (fromIOPasses $ Ex.throwIO Ex.NonTermination),
			testCase "StackOverflow" (fromIOPasses $ Ex.throwIO Ex.StackOverflow),
			testCase "HeapOverflow" (fromIOPasses $ Ex.throwIO Ex.HeapOverflow),
			testCase "ThreadKilled" (fromIOPasses $ Ex.throwIO Ex.ThreadKilled),
			testCase "UserInterrupt" (fromIOPasses $ Ex.throwIO Ex.UserInterrupt),
			testCase "BlockedIndefinitelyOnMVar" (fromIOPasses $ Ex.throwIO Ex.BlockedIndefinitelyOnMVar),
			testCase "BlockedIndefinitelyOnSTM" (fromIOPasses $ Ex.throwIO Ex.BlockedIndefinitelyOnSTM),
			testCase "Deadlock" (fromIOPasses $ Ex.throwIO Ex.Deadlock),
			testCase "NestedAtomically" (fromIOPasses $ Ex.throwIO Ex.NestedAtomically)
		]
	]

main :: IO ()
main = defaultMain tests
