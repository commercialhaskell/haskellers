{-# LANGUAGE CPP #-}
import Controller (withHaskellers)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Debug (debug)
import System.IO (hPutStrLn, stderr)

main :: IO ()
#if PRODUCTION
main = withHaskellers $ run 5005
#else
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withHaskellers $ run port . debug
#endif
