{-# LANGUAGE CPP, DeriveDataTypeable #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)

import Data.Typeable (Typeable)
import Control.Monad
import Control.Exception as Ex
import System.Exit

import qualified UnexceptionalIO as UIO

data CustomException = CustomException deriving (Show, Typeable)
instance Exception CustomException

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
	caught <- try $ UIO.run $ UIO.fromIO io
	either
		(\(SomeException _) -> return ())
		(\x -> assertFailure $ "fromIO caught: " ++ show x)
		caught

tests :: [Test]
tests =
	[
		testGroup "fromIO catches runtime errors" [
			testCase "fail" (fromIOCatches $ fail "boo"),
			testCase "userError" (fromIOCatches $ throwIO $ userError "boo"),
			testCase "CustomException" (fromIOCatches $ throwIO CustomException)
		],
		testGroup "fromIO passes through programmer errors" [
			testCase "error" (fromIOPasses $ error "boo"),
			testCase "undefined" (fromIOPasses undefined),
			testCase "ArithException" (fromIOPasses $ void (return $! 1 `div` 0)),
			testCase "assert" (fromIOPasses $ assert False (return ())),
			testCase "pattern match fail" (fromIOPasses $ (\(Just x) -> return ()) Nothing),
			testCase "array out of bounds" (fromIOPasses $ throwIO $ IndexOutOfBounds "boo"),
			testCase "array uninitialized" (fromIOPasses $ throwIO $ UndefinedElement "boo"),
			testCase "no method" (fromIOPasses $ print $ testClassMethod CantTestClass),
			testCase "use uninitialized record field" (fromIOPasses $ print $ badfld BadRecord {}),
			testCase "use not present record field" (fromIOPasses $ print $ otherfld BadRecord {}),
			testCase "update not present record field" (fromIOPasses $ void (return $! (BadRecord {} { otherfld = "hai" }))),
			testCase "TypeError" (fromIOPasses $ throwIO $ Ex.TypeError "boo")
		],
		testGroup "fromIO passes through termination" [
			testCase "exitSuccess" (fromIOPasses exitSuccess),
			testCase "exitFailure" (fromIOPasses exitFailure),
			testCase "die" (fromIOPasses $ die "exit time")
		],
		testGroup "fromIO passes through asynchronous exceptions from the runtime" [
			testCase "NonTermination" (fromIOPasses $ throwIO Ex.NonTermination),
			testCase "StackOverflow" (fromIOPasses $ throwIO Ex.StackOverflow),
			testCase "HeapOverflow" (fromIOPasses $ throwIO Ex.HeapOverflow),
			testCase "ThreadKilled" (fromIOPasses $ throwIO Ex.ThreadKilled),
			testCase "UserInterrupt" (fromIOPasses $ throwIO Ex.UserInterrupt),
			testCase "BlockedIndefinitelyOnMVar" (fromIOPasses $ throwIO Ex.BlockedIndefinitelyOnMVar),
			testCase "BlockedIndefinitelyOnSTM" (fromIOPasses $ throwIO Ex.BlockedIndefinitelyOnSTM),
			testCase "Deadlock" (fromIOPasses $ throwIO Ex.Deadlock),
			testCase "NestedAtomically" (fromIOPasses $ throwIO Ex.NestedAtomically),
			testCase "AllocationLimitExceeded" (fromIOPasses $ throwIO Ex.AllocationLimitExceeded)
		]
	]

main :: IO ()
main = defaultMain tests
