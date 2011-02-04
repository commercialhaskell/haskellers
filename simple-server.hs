import Controller
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = putStrLn "Loaded" >> withHaskellers (run 3000)
