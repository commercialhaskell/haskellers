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

getAllSkillsR :: Handler RepJson
getAllSkillsR = do
    skills <- runDB $ selectList [] [] 0 0
    render <- getUrlRender
    jsonToRepJson $ jsonMap
        [ ("skills", jsonList $ flip map skills $ \(sid, Skill name) ->
            jsonMap
                [ ("id", jsonScalar $ showIntegral sid)
                , ("name", jsonScalar name)
                , ("url", jsonScalar $ render $ SkillR sid)
                ])
        ]

getSkillR :: SkillId -> Handler RepJson
getSkillR sid = do
    users <- runDB $ do
        uids <- fmap (map $ userSkillUser . snd) $ selectList [UserSkillSkillEq sid] [] 0 0
        us <- mapM get404 uids
        return $ zip uids us
    render <- getUrlRender
    jsonToRepJson $ jsonMap
        [ ("users", jsonList $ flip map users $ \(uid, u) -> jsonMap
            [ ("id", jsonScalar $ showIntegral uid)
            , ("url", jsonScalar $ render $ UserR uid)
            , ("name", jsonScalar $ userFullName u)
            ])
        ]
