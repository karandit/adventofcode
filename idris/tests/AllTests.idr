module AllTests

import Test.Golden

tests : IO TestPool
tests = testsInDir "tests" (const True) "Advent of Code" [] Nothing

main : IO ()
main = do
  runner [!tests]
