{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
module Handler.Table
    ( getTablesR
    , postTablesR , getTableR
    , postTableEditR , getTableEditR
    , tableCheckinWidget
    )
where


import Import
import Helpers.Models
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Text.Julius
import Data.Maybe
import Database.Persist.Store
import Debug.Trace 
import qualified Data.Text as T hiding (null)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

tableForm :: Form Table
tableForm = renderDivs $ Table
    <$> areq   textField     "Name"             Nothing
    <*> areq   textField     "Game"             (Just "NL Holdem $1/$2")
    <*> areq   intField      "Points per hour"  (Just 1)
    <*> areq   intField      "Number of Seats"  (Just 9)
    <*> aopt   textField     "Description"      Nothing

tableFormDefaults :: Table -> Form Table
tableFormDefaults table = renderDivs $ Table
    <$> areq   textField     "Name"             (Just $ tableName table)
    <*> areq   textField     "Game"             (Just $ tableGame table)
    <*> pure   0 --points per hour
    <*> areq   intField      "Number of Seats"  (Just $ tableSeats table)
    <*> aopt   textField     "Description"      Nothing

getTablesR :: Handler RepHtml
getTablesR = do
    tables <- runDB $ selectList [] [Desc TableName]
    (tableWidget, enctype) <- generateFormPost tableForm
    defaultLayout $ do
        setTitle "Tables"
        $(widgetFile "tables")


postTablesR :: Handler RepHtml
postTablesR = do
    ((res,tableWidget),enctype) <- runFormPost tableForm
    case res of 
         FormSuccess table -> do 
            tableId <- runDB $ insert table
            setMessage $ toHtml $ (tableName table) `mappend` " created"
            redirect $ TableR tableId 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "tableAddError")


getTableR :: TableId ->  Handler RepHtml
getTableR tableId = do
     table <- runDB (get404 tableId)
     defaultLayout $ do 
                   setTitle ( "Tables")
                   $(widgetFile "table")

getTableEditR :: TableId ->  Handler RepHtml
getTableEditR tableId = do
     table <- runDB (get404 tableId)
     (tableEditWidget, enctype) <- generateFormPost $ tableFormDefaults table
     defaultLayout $ do 
                   setTitle ( "Tables")
                   $(widgetFile "table-edit")

postTableEditR :: TableId -> Handler RepHtml
postTableEditR tableId = do
    table <- runDB $ get404 tableId
    ((res,tableWidget),enctype) <- runFormPost $ tableFormDefaults table
    case res of 
         FormSuccess table -> do 
            runDB $ replace tableId table
            setMessage $ toHtml $ (tableName table) `mappend` " updated"
            redirect $ TableR tableId 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "tableAddError")

tableCheckinWidget :: PlayerId -> Widget
tableCheckinWidget playerId= do
     tables <- lift $ runDB $ selectList [] []
     tableTuple <-  lift $ mapM addIdent tables
     addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
     $(widgetFile "tableCheckinWidget")

addIdent :: Entity Table -> Handler (Text, TableId, Table, [Int])
addIdent (Entity tableId table) = do
  relevantSessions <- runDB $ selectList [GamingSessionTable ==. tableId] []
  identity <- newIdent
  let seats = []
  return (identity,  tableId, table, seats)

tableClickHandlerWidget :: String -> TableId -> PlayerId -> Maybe Int ->  Widget
tableClickHandlerWidget elemId tableId playerId seatId = do
  let seatNumber = if seatId == Nothing 
                    then "null" 
                    else show  $ fromJust seatId
  liftIO $ print seatNumber
  let pid = toPathPiece $ playerId
  let tid = toPathPiece $ tableId
  toWidget[julius|
        $('##{elemId}').click(function(){
            $.ajax({
              type: 'POST',
              url:'@{GamingSessionsR}', 
              data: { player: #{show pid}, table: #{show tid}, seat: #{seatNumber} },
              success: function(data){ alert('success'); console.log(data);},
              error: function(jqxhr,textStatus,errorThrown){ alert(textStatus + ': ' + errorThrown); },
           } );
            
        });
      |]

