{-# LANGUAGE NoMonomorphismRestriction #-}
module Handler.GamingSession
    ( postGamingSessionsR
    , getGamingSessionsR
    , getGamingSessionR
    , postGamingSessionR
    , postGamingSessionCloseR
    , tableMetaWidget
    )
where

import Import
import Data.Time.Clock
import Data.Time.Format
import Database.Persist.Store
import Data.Text hiding (null, map)
import Data.Maybe
import System.Locale (defaultTimeLocale)
import Helpers.Models
import qualified Data.Map as Map
import Data.List as List hiding (insert)
import Debug.Trace
import Handler.Meta
import Handler.Table

postGamingSessionsR :: Handler RepHtml
postGamingSessionsR = do
    start <- liftIO $ getCurrentTime
    gs <- runInputPost $ GamingSession
                start
                Nothing
                <$> (textToKey <$> (ireq textField "player"))
                <*> (textToKey <$> (ireq textField "table"))
                <*> iopt intField "seat"
                <*> pure 0
    gsId <- runDB $ insert gs
    let startTime = formatTime defaultTimeLocale "%H:%M:%S (%b/%e/%y)" (gamingSessionStart gs)
    player <- runDB (get404 (gamingSessionPlayer gs))
    table <- runDB (get404 (gamingSessionTable gs))
    defaultLayout $(widgetFile "newSession")
  
textToKey a = fromJust . fromPathPiece $ a

getGamingSessionsR :: Handler RepHtml
getGamingSessionsR = do
    user <- requireAuth
    case (userAccount $ entityVal user) of 
        Just accountId -> do 
            records <- runDB $ do
                sessions <- selectList [GamingSessionEnd ==. Nothing] []
                players  <- selectList [PlayerAccount ==. accountId] []
                tables   <- selectList [TableAccount ==. accountId] []
                return $ joinTables3 gamingSessionPlayer gamingSessionTable sessions players tables
            let takenSeats = tableMeta records
            defaultLayout $(widgetFile ("opensessions"))
        Nothing -> noAccount



tableMetaWidget :: [(Entity Table, [Int])] -> Widget
tableMetaWidget tuple = do
      let availSeats = map makeArray tuple
      $(widgetFile "tableMetaWidget") 
      where makeArray ts@(t,ss) = (t,openSeatsArray)
              where
                    openSeatsArray = List.take (tableSeats (entityVal t))  (repeat 0)


getGamingSessionR :: GamingSessionId -> Handler RepHtml
getGamingSessionR gamingSessionId = do 
    session <- runDB (get404 gamingSessionId)
    defaultLayout [whamlet|Get <h1>{#show session}|]

postGamingSessionR :: GamingSessionId -> Handler RepHtml
postGamingSessionR gamingSessionId = do 
    session <- runDB (get404 gamingSessionId)
    defaultLayout [whamlet|Post<h1>{#show session}|]

postGamingSessionCloseR :: GamingSessionId -> Handler RepHtml
postGamingSessionCloseR sid= do
    session <- runDB $ get404 sid
    person  <- runDB $ get404 $ gamingSessionPlayer session
    table   <- runDB $ get404 $ gamingSessionTable session
    end <- liftIO $ getCurrentTime
    let minutes = fromIntegral ( round ( (diffUTCTime end (gamingSessionStart session)) / 60))
    let pph = tablePointsHour table --points per hour
    let points = (fromIntegral minutes / 60) * pph
    runDB $ do
        update  sid [GamingSessionEnd =. Just end, GamingSessionPoints =. points]
        update (gamingSessionPlayer session) [PlayerMinutes +=. minutes]
        update (gamingSessionPlayer session) [PlayerMinutesTotal +=. minutes]
        update (gamingSessionPlayer session) [PlayerPoints +=.  points]
        update (gamingSessionPlayer session) [PlayerPointsTotal +=.  points]
    defaultLayout [whamlet|Session closed!|]
        
        

gamingSessionWidget :: GamingSessionId -> Player -> Table -> Widget
gamingSessionWidget sid p t = do
    let session = toPathPiece sid
    buttonId <- lift $ newIdent
    containerId <- lift $ newIdent
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
    toWidget[julius|
      $('##{buttonId}').click(function(){
         $.ajax({
            type: 'POST',
            url:'@{GamingSessionCloseR sid}', 
            success: function(data){ $('##{containerId}').remove();  },
            error: function(jqxhr,textStatus,errorThrown){ alert(textStatus + ': ' + errorThrown); },
            dataType: "html"
         } );
        
      });

    |]
    $(widgetFile "gamingSession/_session_row")



