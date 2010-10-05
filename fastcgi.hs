import Controller
import Network.Wai.Handler.FastCGI (run)
import Network.HTTP.Enumerator (withHttpEnumerator)

main :: IO ()
main = withHttpEnumerator $ withHaskellers run
