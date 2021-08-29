import           Data.Aeson                       (decode')
import qualified Data.ByteString.Lazy             as B
import           Data.Maybe                       (isJust)
import           Test.Tasty
import           Test.Tasty.HUnit

import           CoinbasePro.Authenticated.Orders (Order)


main :: IO ()
main = defaultMain listOrderParse


listOrderParse :: TestTree
listOrderParse = testCase "parse orders" $ do
    bytes <- B.readFile "./src/test/data/list-orders.json"
    let orders = (decode' bytes :: Maybe [Order])
    assertBool "failed to parse" $ isJust orders
