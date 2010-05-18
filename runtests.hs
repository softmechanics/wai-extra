import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Network.Wai.Parse
import qualified Data.ByteString.Char8 as B8
import Control.Arrow

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Network.Wai.Parse"
    [ testCase "parseQueryString" caseParseQueryString
    , testCase "breakLen" caseBreakLen
    , testCase "breakDiscard" caseBreakDiscard
    ]

caseBreakLen = do
    ([1, 2, 3], 3, [4, 5, 6]) @=? breakLen 4 [1..6]

c2w = toEnum . fromEnum

caseBreakDiscard = do
    breakDiscard (c2w '=') ([], [B8.pack "foo"])
        @?= ((([], [B8.pack "foo"]), 3), ([], []))

caseParseQueryString :: Assertion
caseParseQueryString = do
    let go l r =
            map (B8.pack *** B8.pack) l @=? parseQueryString (B8.pack r)

    go [] ""
    go [("foo", "")] "foo"
    go [("foo", "bar")] "foo=bar"
    go [("foo", "bar"), ("baz", "bin")] "foo=bar&baz=bin"
    go [("%Q", "")] "%Q"
    go [("%1Q", "")] "%1Q"
    go [("%1", "")] "%1"
    go [("/", "")] "%2F"
    go [("/", "")] "%2f"
    go [("foo bar", "")] "foo+bar"
