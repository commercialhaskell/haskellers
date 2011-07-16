{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.User
    ( getUserR
    , getByIdentR
    , getFlagR
    , postFlagR
    , adminControls
    ) where

import Haskellers
import Handler.Root (gravatar)
import Data.List (sortBy, intercalate)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.UTF8 as SU
import OpenSSL.Cipher
import OpenSSL.EVP.Base64
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Form.Jquery (urlJqueryJs)
import Data.Time (getCurrentTime)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read
import Data.Maybe (fromJust)

getByIdentR :: Handler RepJson
getByIdentR = do
    identS <- runFormGet' $ stringInput "ident"
    x <- runDB $ getBy $ UniqueIdent identS
    render <- getUrlRender
    case x of
        Nothing -> notFound
        Just (_, Ident { identUser = uid }) -> jsonToRepJson $ jsonMap
            [ ("id", jsonScalar $ T.unpack $ toSinglePiece uid)
            , ("url", jsonScalar $ T.unpack $ render $ UserR $ toSinglePiece uid)
            ]

getUserR :: Text -> Handler RepHtmlJson
getUserR input = do
    (uid, u) <-
        case Data.Text.Read.decimal input :: Either String (Int, Text) of
            Right (x, "") -> runDB $ do
                let uid = fromJust $ fromSinglePiece input
                liftIO $ print $ "Looking for: " ++ show x ++ ", uid == " ++ show uid
                u <- get404 uid
                mun <- getBy $ UniqueUsernameUser uid
                case mun of
                    Nothing -> return (uid, u)
                    Just (_, Username _ un) ->
                        lift $ redirect RedirectPermanent $ UserR un
            _ -> runDB $ do
                mun <- getBy $ UniqueUsername input
                case mun of
                    Nothing -> lift notFound
                    Just (_, Username uid _) -> do
                        u <- get404 uid
                        return (uid, u)
    mv <- maybeAuth
    let viewerIsAdmin = maybe False (userAdmin . snd) mv

    skills <- runDB $ do
        x <- selectList [UserSkillUserEq uid] [] 0 0 >>= mapM (\(_, y) -> do
            let sid = userSkillSkill y
            s <- get404 sid
            return (sid, T.unpack $ skillName s))
        return $ sortBy (comparing snd) x
    packages <- runDB
              $ fmap (map $ T.unpack . packageName . snd)
              $ selectList [PackageUserEq uid] [PackageNameAsc] 0 0
    screenNames <- runDB $ selectList [ScreenNameUserEq uid]
                    [ScreenNameServiceAsc, ScreenNameNameAsc] 0 0
    let email = fromMaybe "fake@email.com" $ userEmail u
    y <- getYesod
    let json = jsonMap
            $ ((:) ("id", jsonScalar $ T.unpack $ toSinglePiece uid))
            . ((:) ("name", jsonScalar $ T.unpack $ userFullName u))
            . (case userWebsite u of
                Nothing -> id
                Just w -> (:) ("website", jsonScalar $ T.unpack w))
            . (case userHaskellSince u of
                Nothing -> id
                Just e -> (:) ("haskell-since", jsonScalar $ show e))
            . (case userDesc u of
                Nothing -> id
                Just d -> (:) ("description", jsonScalar $ T.unpack $ unTextarea d))
            . ((:) ("skills", jsonList $ map (jsonScalar . snd) skills))
            $ []
    let percentEncode = id -- FIXME
    let packdeps = "http://packdeps.haskellers.com/specific/?" ++
            intercalate "&"
                (map (\x -> "package=" ++ percentEncode x) packages)
    flip defaultLayoutJson json $ do
        setTitle $ toHtml $ "Haskellers profile for " `mappend` userFullName u
        addCassius $(cassiusFile "user")
        addScriptEither $ urlJqueryJs y
        addJulius $(juliusFile "user")
        $(hamletFile "user")
  where
    notOne 1 = False
    notOne _ = True

mailhidePublic :: Text
mailhidePublic = "01_o4fjI3uXdNz6rLrIquvlw=="

mailhidePrivate :: S.ByteString
mailhidePrivate = S8.pack "\x42\x40\x54\x79\x07\x8c\x47\xb0\x50\xd7\x9a\x33\xc6\x09\x69\x1c"

emailLink :: Text -> Text
emailLink email = unsafePerformIO $ do
    enc <- encryptAddress email
    return $ T.concat
        [ "http://www.google.com/recaptcha/mailhide/d?k="
        , mailhidePublic
        , "&c="
        , enc
        ]

encryptAddress :: Text -> IO Text
encryptAddress =
    fmap (T.pack . map b64Url . S8.unpack . encodeBase64BS) . encrypt . pad . T.unpack
  where
    b64Url '+' = '-'
    b64Url '/' = '_'
    b64Url c   = c

pad :: String -> S.ByteString
pad s =
    let bs' = SU.fromString s
        blockSize = 16
        numpad = blockSize - (S.length bs' `mod` blockSize)
        padding = S.replicate numpad $ fromIntegral numpad
     in bs' `S.append` padding

encrypt :: S.ByteString -> IO S.ByteString
encrypt bs = do
    ctx <- newAESCtx Encrypt mailhidePrivate $ S.replicate 16 0
    aesCBC ctx bs

getFlagR :: UserId -> Handler RepHtml
getFlagR uid = do
    u <- runDB $ get404 uid
    let userLink = userR ((uid, u), Nothing)
    defaultLayout $ do
        setTitle "Report a user"
        addCassius $(cassiusFile "flag")
        addWidget $(hamletFile "flag")

postFlagR :: UserId -> Handler ()
postFlagR uid = do
    mvid <- fmap (fmap fst) maybeAuth
    mmsg <- runFormPost' $ maybeStringInput "message"
    let msg = fromMaybe "" mmsg

    u <- runDB $ do
        u <- get404 uid
        now <- liftIO getCurrentTime
        _ <- insert Message
            { messageClosed = False
            , messageWhen = now
            , messageFrom = mvid
            , messageRegarding = Just uid
            , messageText = Textarea $ "User has been reported\n\n" `mappend` msg
            }
        return u
    setMessage "The user has been reported to the admins. Thanks!"
    redirect RedirectTemporary $ userR ((uid, u), Nothing)

adminControls :: UserId -> User -> Widget ()
adminControls uid u = do
    addCassius $(cassiusFile "admin-controls")
    $(hamletFile "admin-controls")
