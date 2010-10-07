{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Handler.Skills
    ( postAllSkillsR
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
