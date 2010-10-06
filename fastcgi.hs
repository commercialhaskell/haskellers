import Controller
import Network.Wai.Handler.FastCGI (runFork)
import Network.HTTP.Enumerator (withHttpEnumerator)
import Control.Concurrent (forkIO)

main :: IO ()
main = withHttpEnumerator $ withHaskellers $ runFork Nothing forkIO 5
