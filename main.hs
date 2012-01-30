{-# LANGUAGE CPP #-}
import Application (getApplication)
import Network.Wai.Handler.Warp (run)
import Data.Text (pack)

#if PRODUCTION
import System.Environment (getArgs)
main :: IO ()
main = do
    args <- getArgs
    let usage = "Usage: haskellers <port> <approot>"
    (port, approot) <-
        case args of
            [x, y] ->
                case reads x of
                    (i, _):_ -> return (i, y)
                    _ -> error usage
            _ -> error usage
    app <- getApplication (pack approot)
    run port app
#else
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    app <- getApplication $ pack "http://localhost:3000"
    run port $ logStdoutDev app
#endif
