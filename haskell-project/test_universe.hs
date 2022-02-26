import Universe (sumBuiltinUsed,countAllStars)
import Control.Monad
import Control.Exception
import Data.IORef

data AssertFailed = AssertFailed String deriving Show
instance Exception AssertFailed

success = putStrLn "TECHIO> success true"

failure = putStrLn "TECHIO> success false"

sendMsg :: String -> String -> IO ()
sendMsg chan msg =
  putStrLn $ "TECHIO> message --channel \"" ++ chan ++ "\" \"" ++ msg ++ "\""

test :: [Int] -> Int -> IO ()
test arg ref = do
  let res = countAllStars arg :: Int
  unless (res == ref) $ do
    throw $ AssertFailed $ "Running countAllStars " ++ show arg ++
      " Expected " ++ show ref ++ ", got " ++ show res

assertHandler (AssertFailed e) = do
  failure
  sendMsg "Oops! 🐞" e
  sendMsg "Hint 💡" "Did you account for both the galaxy at hand and the rest of them? \x1F914"
  return Nothing

main = do
  sumBuiltinUsed <- handle assertHandler $ do
    test [2,3] 5
    test [9,-3] 6
    success
    Just <$> readIORef sumBuiltinUsed

  case sumBuiltinUsed of
    Just True -> do
      sendMsg "My personal Yoda, you are. 🙏" "* ● ¸ .　¸. :° ☾ ° 　¸. ● ¸ .　　¸.　:. • "
      sendMsg "My personal Yoda, you are. 🙏" "           　★ °  ☆ ¸. ¸ 　★　 :.　 .   "
      sendMsg "My personal Yoda, you are. 🙏" "__.-._     ° . .　　　　.　☾ ° 　. *   ¸ ."
      sendMsg "My personal Yoda, you are. 🙏" "'-._\\7'      .　　° ☾  ° 　¸.☆  ● .　　　"
      sendMsg "My personal Yoda, you are. 🙏" " /'.-c    　   * ●  ¸.　　°     ° 　¸.    "
      sendMsg "My personal Yoda, you are. 🙏" " |  /T      　　°     ° 　¸.     ¸ .　　  "
      sendMsg "My personal Yoda, you are. 🙏" "_)_/LI"
    Just False -> do
      sendMsg "Kudos 🌟" "Did you know that you could use the sum function? Try it!"
      sendMsg "Kudos 🌟" ""
      sendMsg "Kudos 🌟" "galaxies = [37, 3, 2]"
      sendMsg "Kudos 🌟" "totalStars = sum galaxies  -- 42"
    Nothing -> return ()
