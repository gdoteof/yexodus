{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Player
    ( getPlayerListR
    , getPlayerEditR
    , postPlayerListR
    , getPlayerR
    )
where


import Import 

-- to use Html into forms
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Data.Maybe
import Handler.Table
import Handler.Meta
import Helpers.Models
import Data.Text (unpack,pack)

playerForm :: Form Player
playerForm = renderDivs $ Player
    <$> areq   textField "Name" Nothing
    <*> areq   textField "Nick" Nothing
    <*> aopt   textField "Email" Nothing
    <*> aopt   textField "Phone" Nothing
    <*> aopt   textareaField "Notes" Nothing
    <*> areq   intField "Minutes to Start" (Just 0)
    <*> pure   False



getPlayerListR :: Handler RepHtml
getPlayerListR = do
    players <- runDB $ selectList [] [Desc PlayerName]
    (playerWidget, enctype) <- generateFormPost playerForm
    defaultLayout $ do
        setTitle "Player List"
        $(widgetFile "players")


postPlayerListR :: Handler RepHtml
postPlayerListR = do
    ((res,playerWidget),enctype) <- runFormPost playerForm
    case res of 
         FormSuccess player -> do 
            playerId <- runDB $ insert player
            setMessage $ toHtml $ (playerName player) `mappend` " created"
            redirect $ PlayerR playerId 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "playerAddError")


playerFormDefaults :: Player -> Form Player
playerFormDefaults player = renderDivs $ Player
    <$> areq   textField "Name" (Just $ playerName player)
    <*> areq   textField "Nick" (Just $ playerNick player)
    <*> aopt   textField "Email" (Just $ playerEmail player)
    <*> aopt   textField "Phone" (Just $ playerPhone player)
    <*> aopt   textareaField "Notes" (Just $ playerNote player)
    <*> pure   0
    <*> pure   False

getPlayerEditR :: PlayerId -> Handler RepHtml
getPlayerEditR playerId = do
        player <- runDB $ get404 playerId
        (playerEditWidget, enctype) <- generateFormPost $ playerFormDefaults player
        defaultLayout $ do
                $(widgetFile "player-edit")

getPlayerR :: PlayerId -> Handler RepHtml
getPlayerR playerId = do
     mplayerSession <- runDB $ selectFirst [GamingSessionPlayer ==. playerId, GamingSessionEnd ==. Nothing] [] 
     player <- runDB (get404 playerId)
     (playerWidgetDefaults, enctype) <- generateFormPost (playerFormDefaults player)
     let minutes =  playerMinutes player
     let notes   = case playerNote player of
                        Nothing -> Textarea "No notes"
                        Just n -> n
     table :: Text <- case mplayerSession of
                           Nothing -> do
                                          return $ pack "Not checked into a table" -- should add warning here.
                           Just s  -> do
                                          t <- runDB $ get $ gamingSessionTable $ entityVal  s 
                                          return  (tableName $ fromJust t)
     
     defaultLayout $ do 
                   $(widgetFile "player")
   {-where table = case mplayerSession of
                        Nothing -> undefined
                        Just s  -> do
                               t <- runDB $ get (gamingSessionTable $ entityVal s) 
                               return undefined -- (tableName $ fromJust t)
   -}
