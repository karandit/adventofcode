import qualified Spec2015
import qualified Spec2020
import qualified Spec2021
import Test.Tasty

main = defaultMain tests

tests =
  testGroup
    "tests"
    [ Spec2015.tests,
      Spec2020.tests,
      Spec2021.tests
    ]