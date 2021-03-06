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
  sendMsg "Oops! π" e
  sendMsg "Hint π‘" "Did you account for both the galaxy at hand and the rest of them? \x1F914"
  return Nothing

main = do
  sumBuiltinUsed <- handle assertHandler $ do
    test [2,3] 5
    test [9,-3] 6
    success
    Just <$> readIORef sumBuiltinUsed

  case sumBuiltinUsed of
    Just True -> do
      sendMsg "My personal Yoda, you are. π" "* β ΒΈ .γΒΈ. :Β° βΎ Β° γΒΈ. β ΒΈ .γγΒΈ.γ:. β’ "
      sendMsg "My personal Yoda, you are. π" "           γβ Β°  β ΒΈ. ΒΈ γβγ :.γ .   "
      sendMsg "My personal Yoda, you are. π" "__.-._     Β° . .γγγγ.γβΎ Β° γ. *   ΒΈ ."
      sendMsg "My personal Yoda, you are. π" "'-._\\7'      .γγΒ° βΎ  Β° γΒΈ.β  β .γγγ"
      sendMsg "My personal Yoda, you are. π" " /'.-c    γ   * β  ΒΈ.γγΒ°     Β° γΒΈ.    "
      sendMsg "My personal Yoda, you are. π" " |  /T      γγΒ°     Β° γΒΈ.     ΒΈ .γγ  "
      sendMsg "My personal Yoda, you are. π" "_)_/LI"
    Just False -> do
      sendMsg "Kudos π" "Did you know that you could use the sum function? Try it!"
      sendMsg "Kudos π" ""
      sendMsg "Kudos π" "galaxies = [37, 3, 2]"
      sendMsg "Kudos π" "totalStars = sum galaxies  -- 42"
    Nothing -> return ()
