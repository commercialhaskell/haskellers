{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Handler.Skills
    ( postAllSkillsR
    , getAllSkillsR
    , getSkillR
    ) where

import Haskellers

postAllSkillsR :: Handler ()
postAllSkillsR = do
    _ <- requireAuth
    (res, _, _) <- runFormPost $ stringInput "skill"
    case res of
        FormSuccess skill -> do
            _ <- runDB $ insert $ Skill skill
            setMessage "Inserted new skill to skills list"
        _ -> setMessage "Invalid skill entered"
    redirect RedirectTemporary ProfileR

getAllSkillsR :: Handler RepHtmlJson
getAllSkillsR = do
    skills' <- runDB $ selectList [] [SkillNameAsc] 0 0 >>= mapM (\(sid, s) -> do
        users <- count [UserSkillSkillEq sid]
        return ((sid, s), users)
        )
    showall <- runFormGet' $ boolInput "show-all"
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
    skill <- runDB $ get404 sid
    users <- runDB $ do
        uids <- fmap (map $ userSkillUser . snd)
              $ selectList [ UserSkillSkillEq sid
                           ] [] 0 0
        us <- mapM get404 uids
        return $ filter go $ zip uids us
    render <- getUrlRender
    defaultLayoutJson (do
        setTitle $ string $ skillName skill
        $(hamletFile "skill")
        ) $ jsonMap
        [ ("users", jsonList $ flip map users $ \(uid, u) -> jsonMap
            [ ("id", jsonScalar $ showIntegral uid)
            , ("url", jsonScalar $ render $ UserR uid)
            , ("name", jsonScalar $ userFullName u)
            ])
        ]
  where
    go (_, u) = userVerifiedEmail u && userVisible u && not (userBlocked u)
