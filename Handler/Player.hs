{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Player
    ( getPlayerListR
    , postPlayerListR
    , getPlayerEditR
    , postPlayerEditR
    , getPlayerEditTimeR
    , postPlayerEditTimeR
    , getPlayerR
    )
where


import Import 

-- to use Html into forms
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Yesod.Auth
import Data.Maybe
import Handler.Table
import Handler.Meta
import Helpers.Models
import Data.Text (unpack,pack)
import Database.Persist.Query.Internal
import Numeric

playerForm :: Entity User -> Form Player
playerForm user = renderDivs $ Player
    <$> areq   textField "Name" Nothing
    <*> areq   textField "Nick" Nothing
    <*> aopt   textField "Email" Nothing
    <*> aopt   textField "Phone" Nothing
    <*> aopt   textareaField "Notes" Nothing
    <*> pure   0
    <*> pure   False
    <*> pure   (fromJust $ userAccount $ entityVal user)

playerFormDefaults :: Entity User -> Player -> Form Player
playerFormDefaults user player = renderDivs $ Player
    <$> areq   textField     "Name"  (Just $ playerName  player)
    <*> areq   textField     "Nick"  (Just $ playerNick  player)
    <*> aopt   textField     "Email" (Just $ playerEmail player)
    <*> aopt   textField     "Phone" (Just $ playerPhone player)
    <*> aopt   textareaField "Notes" (Just $ playerNote player)
    <*> pure   (playerMinutes player)
    <*> pure   False
    <*> pure   (fromJust $ userAccount $ entityVal user)


getPlayerListR :: Handler RepHtml
getPlayerListR = do
    user <- requireAuth
    case (userAccount $ entityVal user) of 
        Just accountId -> do 
            players <- runDB $ selectList [PlayerAccount ==. accountId] [Desc PlayerName]
            (playerWidget, enctype) <- generateFormPost $ playerForm user
            defaultLayout $ do
                setTitle "Player List"
                $(widgetFile "players")
        _ -> do 
            noAccount

postPlayerListR :: Handler RepHtml
postPlayerListR = do
    user <- requireAuth    
    ((res,playerWidget),enctype) <- runFormPost $ playerForm user
    case res of 
         FormSuccess player -> do 
            playerId <- runDB $ insert player
            setMessage $ toHtml $ (playerName player) `mappend` " created"
            redirect $ PlayerR playerId 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "playerAddError")


getPlayerEditR :: PlayerId -> Handler RepHtml
getPlayerEditR playerId = do
        user <- requireAuth
        player <- runDB $ get404 playerId
        (playerEditWidget, enctype) <- generateFormPost $ playerFormDefaults user player
        defaultLayout $ do
                $(widgetFile "player-edit")

postPlayerEditR :: PlayerId -> Handler RepHtml
postPlayerEditR playerId = do
    user <- requireAuth
    player <- runDB $ get404 playerId
    ((res,playerWidget),enctype) <- runFormPost $ playerFormDefaults user player
    case res of 
         FormSuccess player -> do 
            runDB $ replace playerId $ player
            setMessage $ toHtml $ (playerName player) `mappend` " edited"
            redirect $ PlayerR playerId 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "playerAddError")

getPlayerR :: PlayerId -> Handler RepHtml
getPlayerR playerId = do
     user <- requireAuth    
     mplayerSession <- runDB $ selectFirst [GamingSessionPlayer ==. playerId, GamingSessionEnd ==. Nothing] [] 
     player <- runDB (get404 playerId)
     (playerWidgetDefaults, enctype) <- generateFormPost (playerFormDefaults user player)
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

data AddSubtract = Addition | Subtraction
    deriving (Show, Eq, Enum, Bounded)

data HoursMinutes = Minutes | Hours
    deriving (Show, Eq, Enum, Bounded)

playerTimeForm ::  Form (AddSubtract, Int, HoursMinutes)
playerTimeForm = renderDivs $ (,,)
    <$> areq   (radioFieldList addSubtract) "" (Just Addition)
    <*> areq   intField "This many:" (Just 0)
    <*> areq   (radioFieldList hoursMinutes) "" (Just Minutes)
      where 
        addSubtract :: [(Text, AddSubtract)]
        addSubtract  = [("Add", Addition), ("Subtract", Subtraction)]
        hoursMinutes :: [(Text, HoursMinutes)]
        hoursMinutes  = [("Minutes", Minutes), ("Hours", Hours)]

getPlayerEditTimeR :: PlayerId -> Handler RepHtml
getPlayerEditTimeR playerId = do
    player <- runDB $ get404 playerId
    user <- requireAuth
    let hours = showFFloat (Just 3) (fromRational (( toRational $ playerMinutes player) / 60)) ""
    (playerEditTimeWidget, enctype) <- generateFormPost $ playerTimeForm 
    defaultLayout $ do
        setTitle "Editing time" 
        $(widgetFile "player/player-edit-time")

postPlayerEditTimeR :: PlayerId -> Handler RepHtml
postPlayerEditTimeR  playerId = do
    player <- runDB $ get404 playerId
    ((res,playerEditTimeWidget),enctype) <- runFormPost $ playerTimeForm
    case res of 
         FormSuccess (addSubtract, time, hoursMinutes) -> do 
            let minutes = case hoursMinutes of
                      Hours -> time * 60
                      Minutes -> time
            let op = case addSubtract of
                      Addition    -> (+=.)
                      Subtraction -> (-=.)
            runDB $ update playerId [PlayerMinutes `op` minutes]
            setMessage $ toHtml $ (playerName player) `mappend` " created"
            redirect $ PlayerR playerId 
         _ -> redirect $ PlayerEditR playerId 
