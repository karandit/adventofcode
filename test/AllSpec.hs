import Test.HUnit
import qualified Spec2015
import qualified Spec2020

main = runTestTT tests

tests = TestList
            [ Spec2015.tests
            , Spec2020.tests
            ]
