{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy as L

mkReq p = Request
    { method = "GET"
    , secure = False
    , host = "localhost"
    , port = 3000
    , path = p
    , queryString = []
    , requestHeaders = [("Accept", "application/json")]
    , requestBody = L.empty
    }

main = do
    httpLbsRedirect (mkReq "/") >>= (L.putStrLn . responseBody)
    httpLbsRedirect (mkReq "/user/5/") >>= (L.putStrLn . responseBody)
    httpLbsRedirect (mkReq "/user/")
        { queryString = [("ident", "http://openid.aol.com/snoyberg")]
        } >>= (L.putStrLn . responseBody)
