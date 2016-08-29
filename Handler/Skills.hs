module Handler.Skills
    ( postAllSkillsR
    , getAllSkillsR
    , getSkillR
    ) where

import Import
import Handler.Admin (requireAdmin)

skillFormlet :: Form Skill
skillFormlet = renderTable $ Skill
    <$> areq textField "Skill name" { fsId = Just "skill-name" } Nothing

postAllSkillsR :: Handler ()
postAllSkillsR = do
    requireAdmin
    ((res, _), _) <- runFormPostNoToken skillFormlet
    case res of
        FormSuccess skill -> do
            _ <- runDB $ insert skill
            setMessage "Inserted new skill to skills list"
        _ -> setMessage "Invalid skill entered"
    redirect AllSkillsR

getAllSkillsR :: Handler TypedContent
getAllSkillsR = do
    mu <- maybeAuth
    skills' <- runDB $ selectList [] [Asc SkillName] >>= mapM (\(Entity sid s) -> do
        users <- count [UserSkillSkill ==. sid]
        return ((sid, s), users)
        )
    showall <- fmap (maybe False id) $ runInputGet $ iopt boolField "show-all"
    ((_, form), _) <- runFormGet skillFormlet
    let threshhold = 10
    let skills =
            if showall
                then skills'
                else filter (\(_, x) -> x >= threshhold) skills'
    let hidden = length skills' - length skills
    let areHidden = hidden > 0
    let showAllUrl = (AllSkillsR, [("show-all", "yes")])
    render <- getUrlRender
    defaultLayoutJson (do
        setTitle "Browse all skills"
        $(widgetFile "skills")
        ) $ return $ object
        [ "skills" .= array (flip map skills' $ \((sid, Skill name), users) ->
            object
                [ "id"    .= toPathPiece sid
                , "name"  .= name
                , "url"   .= render (SkillR sid)
                , "users" .= show users
                ])
        ]

getSkillR :: SkillId -> Handler TypedContent
getSkillR sid = do
    skill <- runDB $ get404 sid
    users <- runDB $ do
        uids <- fmap (map $ userSkillUser . entityVal)
              $ selectList [ UserSkillSkill ==. sid
                           ] []
        us <- mapM get404 uids
        flip mapM (filter go $ zip uids us) $ \(uid, u) -> do
            mun <- fmap (fmap entityVal) $ getBy $ UniqueUsernameUser uid
            return ((uid, u), mun)
    render <- getUrlRender
    defaultLayoutJson (do
        setTitle $ toHtml $ skillName skill
        $(widgetFile "skill")
        ) $ return $ object
        [ "users" .= array (flip map users $ \x@((uid, u), _) -> object
            [ "id"   .= toPathPiece uid
            , "url"  .= render (userR x)
            , "name" .= userFullName u
            ])
        ]
  where
    go (_, u) = userVerifiedEmail u && userVisible u && not (userBlocked u)
