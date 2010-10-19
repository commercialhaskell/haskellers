{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module Handler.Skills
    ( postAllSkillsR
    , getAllSkillsR
    , getSkillR
    ) where

#define debugRunDB debugRunDBInner __FILE__ __LINE__

import Haskellers
import Handler.Admin (requireAdmin)
import Control.Applicative

skillFormlet :: Form s m Skill
skillFormlet = fieldsToTable $ Skill
    <$> stringField "Skill name" { ffsId = Just "skill-name" } Nothing

postAllSkillsR :: Handler ()
postAllSkillsR = do
    requireAdmin
    (res, _, _) <- runFormPost skillFormlet
    case res of
        FormSuccess skill -> do
            _ <- debugRunDB $ insert skill
            setMessage "Inserted new skill to skills list"
        _ -> setMessage "Invalid skill entered"
    redirect RedirectTemporary AllSkillsR

getAllSkillsR :: Handler RepHtmlJson
getAllSkillsR = do
    mu <- maybeAuth
    skills' <- debugRunDBInner "skills" 25 $ selectList [] [SkillNameAsc] 0 0 >>= mapM (\(sid, s) -> do
        users <- count [UserSkillSkillEq sid]
        return ((sid, s), users)
        )
    showall <- runFormGet' $ boolInput "show-all"
    (_, form, _) <- runFormGet skillFormlet
    let threshhold = 10
    let skills =
            if showall
                then skills'
                else filter (\(_, x) -> x >= threshhold) skills'
    let hidden = length skills' - length skills
    let areHidden = hidden > 0
    let showAllUrl = (AllSkillsR, [("show-all", "true")])
    render <- getUrlRender
    defaultLayoutJson (do
        setTitle "Browse all skills"
        addStyle $(cassiusFile "skills")
        $(hamletFile "skills")
        ) $ jsonMap
        [ ("skills", jsonList $ flip map skills' $ \((sid, Skill name), users) ->
            jsonMap
                [ ("id", jsonScalar $ showIntegral sid)
                , ("name", jsonScalar name)
                , ("url", jsonScalar $ render $ SkillR sid)
                , ("users", jsonScalar $ show users)
                ])
        ]

getSkillR :: SkillId -> Handler RepHtmlJson
getSkillR sid = do
    skill <- debugRunDB $ get404 sid
    users <- debugRunDB $ do
        uids <- fmap (map $ userSkillUser . snd)
              $ selectList [ UserSkillSkillEq sid
                           ] [] 0 0
        us <- mapM get404 uids
        flip mapM (filter go $ zip uids us) $ \(uid, u) -> do
            mun <- fmap (fmap snd) $ getBy $ UniqueUsernameUser uid
            return ((uid, u), mun)
    render <- getUrlRender
    defaultLayoutJson (do
        setTitle $ string $ skillName skill
        $(hamletFile "skill")
        ) $ jsonMap
        [ ("users", jsonList $ flip map users $ \x@((uid, u), _) -> jsonMap
            [ ("id", jsonScalar $ showIntegral uid)
            , ("url", jsonScalar $ render $ userR x)
            , ("name", jsonScalar $ userFullName u)
            ])
        ]
  where
    go (_, u) = userVerifiedEmail u && userVisible u && not (userBlocked u)
