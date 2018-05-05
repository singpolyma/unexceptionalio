import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)

import Control.Monad
import Control.Exception
import System.Exit
import Data.Array

import UnexceptionalIO

data CustomException = CustomException deriving (Show)
instance Exception CustomException

data CantShow = CantShow
instance Show CantShow

data BadRecord = BadRecord { badfld :: String } | OtherBadRecord { otherfld :: String }

fromIOCatches :: IO () -> Assertion
fromIOCatches io = do
	caught <- runUIO $ fromIO io
	either
		(const $ return ())
		(\x -> assertFailure $ "fromIO did not catch: " ++ show x)
		caught

fromIOPasses :: IO () -> Assertion
fromIOPasses io = do
	caught <- try $ runUIO $ fromIO io
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
			testCase "array out of bounds" (fromIOPasses $ void (return $! (listArray (0,1) [0..] ! 100))),
			testCase "no method" (fromIOPasses $ print CantShow),
			testCase "use uninitialized record field" (fromIOPasses $ print $ badfld BadRecord {}),
			testCase "use not present record field" (fromIOPasses $ print $ otherfld BadRecord {}),
			testCase "update not present record field" (fromIOPasses $ void (return $! (BadRecord {} { otherfld = "hai" }))),
			testCase "TypeError" (fromIOPasses $ throwIO $ TypeError "boo")
		],
		testGroup "fromIO passes through termination" [
			testCase "exitSuccess" (fromIOPasses exitSuccess),
			testCase "exitFailure" (fromIOPasses exitFailure),
			testCase "die" (fromIOPasses $ die "exit time")
		],
		testGroup "fromIO passes through asynchronous exceptions from the runtime" [
			testCase "NonTermination" (fromIOPasses $ throwIO NonTermination),
			testCase "StackOverflow" (fromIOPasses $ throwIO StackOverflow),
			testCase "HeapOverflow" (fromIOPasses $ throwIO HeapOverflow),
			testCase "ThreadKilled" (fromIOPasses $ throwIO ThreadKilled),
			testCase "UserInterrupt" (fromIOPasses $ throwIO UserInterrupt),
			testCase "BlockedIndefinitelyOnMVar" (fromIOPasses $ throwIO BlockedIndefinitelyOnMVar),
			testCase "BlockedIndefinitelyOnSTM" (fromIOPasses $ throwIO BlockedIndefinitelyOnSTM),
			testCase "Deadlock" (fromIOPasses $ throwIO Deadlock),
			testCase "NestedAtomically" (fromIOPasses $ throwIO NestedAtomically),
			testCase "AllocationLimitExceeded" (fromIOPasses $ throwIO AllocationLimitExceeded)
		]
	]

main :: IO ()
main = defaultMain tests
