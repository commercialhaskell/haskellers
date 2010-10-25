{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Handler.User
    ( getUserR
    , getByIdentR
    , getFlagR
    , postFlagR
    , adminControls
    ) where

#define debugRunDB debugRunDBInner __FILE__ __LINE__

import Haskellers
import Handler.Root (gravatar)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.UTF8 as SU
import OpenSSL.Cipher
import OpenSSL.EVP.Base64
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Form.Jquery (urlJqueryJs)
import Data.Time (getCurrentTime)

getByIdentR :: Handler RepJson
getByIdentR = do
    identS <- runFormGet' $ stringInput "ident"
    x <- debugRunDB $ getBy $ UniqueIdent identS
    render <- getUrlRender
    case x of
        Nothing -> notFound
        Just (_, Ident { identUser = uid }) -> jsonToRepJson $ jsonMap
            [ ("id", jsonScalar $ showIntegral uid)
            , ("url", jsonScalar $ render $ UserR $ showIntegral uid)
            ]

getUserR :: String -> Handler RepHtmlJson
getUserR input = do
    (uid, u) <-
        case readIntegral input of
            Just uid -> debugRunDB $ do
                u <- get404 uid
                mun <- getBy $ UniqueUsernameUser uid
                case mun of
                    Nothing -> return (uid, u)
                    Just (_, Username _ un) ->
                        lift $ redirect RedirectPermanent $ UserR un
            Nothing -> debugRunDB $ do
                mun <- getBy $ UniqueUsername input
                case mun of
                    Nothing -> lift notFound
                    Just (_, Username uid _) -> do
                        u <- get404 uid
                        return (uid, u)
    mv <- maybeAuth
    let viewerIsAdmin = maybe False (userAdmin . snd) mv
    skills <- debugRunDB $ do
        x <- selectList [UserSkillUserEq uid] [] 0 0
        y <- mapM (get404 . userSkillSkill . snd) x
        return $ sort $ map skillName y
    packages <- debugRunDB
              $ fmap (map $ packageName . snd)
              $ selectList [PackageUserEq uid] [PackageNameAsc] 0 0
    screenNames <- debugRunDB $ selectList [ScreenNameUserEq uid]
                    [ScreenNameServiceAsc, ScreenNameNameAsc] 0 0
    let email = fromMaybe "fake@email.com" $ userEmail u
    y <- getYesod
    let json = jsonMap
            $ ((:) ("id", jsonScalar $ showIntegral uid))
            . ((:) ("name", jsonScalar $ userFullName u))
            . (case userWebsite u of
                Nothing -> id
                Just w -> (:) ("website", jsonScalar w))
            . (case userHaskellSince u of
                Nothing -> id
                Just e -> (:) ("haskell-since", jsonScalar $ show e))
            . (case userDesc u of
                Nothing -> id
                Just d -> (:) ("description", jsonScalar $ unTextarea d))
            . ((:) ("skills", jsonList $ map jsonScalar skills))
            $ []
    flip defaultLayoutJson json $ do
        setTitle $ string $ "Haskellers profile for " ++ userFullName u
        addCassius $(cassiusFile "user")
        addScriptEither $ urlJqueryJs y
        addJulius $(juliusFile "user")
        $(hamletFile "user")
  where
    notOne 1 = False
    notOne _ = True

mailhidePublic :: String
mailhidePublic = "01_o4fjI3uXdNz6rLrIquvlw=="

mailhidePrivate :: S.ByteString
mailhidePrivate = S8.pack "\x42\x40\x54\x79\x07\x8c\x47\xb0\x50\xd7\x9a\x33\xc6\x09\x69\x1c"

emailLink :: String -> String
emailLink email = unsafePerformIO $ do
    enc <- encryptAddress email
    return $ concat
        [ "http://www.google.com/recaptcha/mailhide/d?k="
        , mailhidePublic
        , "&c="
        , enc
        ]

encryptAddress :: String -> IO String
encryptAddress =
    fmap (map b64Url . S8.unpack . encodeBase64BS) . encrypt . pad
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
    u <- debugRunDB $ get404 uid
    let userLink = userR ((uid, u), Nothing)
    defaultLayout $ do
        setTitle $ string "Report a user"
        addCassius $(cassiusFile "flag")
        addWidget $(hamletFile "flag")

postFlagR :: UserId -> Handler ()
postFlagR uid = do
    mvid <- fmap (fmap fst) maybeAuth
    mmsg <- runFormPost' $ maybeStringInput "message"
    let msg = fromMaybe "" mmsg

    u <- debugRunDB $ do
        u <- get404 uid
        now <- liftIO getCurrentTime
        _ <- insert Message
            { messageClosed = False
            , messageWhen = now
            , messageFrom = mvid
            , messageRegarding = Just uid
            , messageText = Textarea $ "User has been reported\n\n" ++ msg
            }
        return u
    setMessage $ string "The user has been reported to the admins. Thanks!"
    redirect RedirectTemporary $ userR ((uid, u), Nothing)

adminControls :: UserId -> User -> Widget ()
adminControls uid u = do
    addCassius $(cassiusFile "admin-controls")
    $(hamletFile "admin-controls")
