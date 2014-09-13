import Test.HUnit

tests :: Test
tests = TestList [
            TestLabel "SimpleTest" $ TestCase $ do 
                assertEqual "Stuff"  "a" "a"
            ]

main :: IO()
main = do
    _ <- runTestTT tests
    return ()