module Handler.Meta(checkinWidget
                   ,checkoutWidget
                   ) where

import Import
import Helpers.Models
import Data.Maybe
import Data.List (sort)

type MetaTable         = Entity Table
type MetaGamingSession = Entity GamingSession
type MetaPlayer        = Entity Player

data PlayerSlot         = Empty | PlayerSlot { playerSlotPlayer :: MetaPlayer
                                             , playerSlotSession :: MetaGamingSession , playerSlotSeat :: Int
                                             } deriving (Show, Eq)

--should probably update this to just user playerSlotSeat and add test
instance Ord PlayerSlot where
        (PlayerSlot _ s1 _) `compare` (PlayerSlot _ s2 _) = 
                (fromJust $ gamingSessionSeat $ entityVal s1) `compare` (fromJust $ gamingSessionSeat$ entityVal s2)

data GamingTable        = GamingTable { gamingTableTable       :: MetaTable
                                      , playerSlots :: [PlayerSlot]
                                      } deriving (Show)

data SeatingArrangement = SeatingArrangement { gamingTables :: [GamingTable] } deriving (Show)
                          
getSeatingArrangement :: Handler SeatingArrangement
getSeatingArrangement = do
        tables <- runDB $ selectList [] [Desc TableName]
        gamingTables <- mapM getGamingTable tables
        return $ SeatingArrangement gamingTables

getGamingTable :: MetaTable -> Handler GamingTable
getGamingTable table = do
        sessions    <- runDB $ selectList [GamingSessionTable ==. entityKey table, GamingSessionEnd ==. Nothing] []
        playerSlots <- mapM getPlayerSlot sessions
        let seatedTable = fillEmpties (tableSeats $ entityVal table) 1 $ sort playerSlots
        return $ GamingTable table seatedTable

getPlayerSlot :: MetaGamingSession -> Handler PlayerSlot
getPlayerSlot session =  do
        let pid = gamingSessionPlayer (entityVal session)
        mplayer <- runDB $ get pid
        case mplayer of
            Nothing     -> return Empty --should be catching exception here
            Just player -> return $ PlayerSlot (Entity pid player) session seat
        where seat = fromJust $ gamingSessionSeat $ entityVal $ session
        
slotSeat :: PlayerSlot -> Maybe Int
slotSeat a = case a of
                  Empty -> Nothing
                  PlayerSlot _ s _ -> gamingSessionSeat $ entityVal s 

type Max = Int
type Accu = Int
fillEmpties :: Max -> Accu -> [PlayerSlot] -> [PlayerSlot]
fillEmpties 0 _ _  = []
fillEmpties max n [] = take (max - n + 1) $ repeat Empty
fillEmpties max n slots@(s:ss) = if max == n 
                                 then [s]
                                 else   
                                          if fromJust (slotSeat s) == n
                                                  then s     : (fillEmpties max (n+1) ss)
                                                  else Empty : (fillEmpties max (n+1) slots)

checkinWidget :: PlayerId -> Widget
checkinWidget playerId = do
        seatingArrangement <- lift $  getSeatingArrangement
        let tables = gamingTables seatingArrangement
        $(widgetFile "checkinWidget")

checkoutWidget ::  Widget
checkoutWidget = do
        seatingArrangement <- lift $  getSeatingArrangement
        let tables = gamingTables seatingArrangement
        $(widgetFile "checkoutWidget")

--I needed this function to wrap the [PlayerSlot] to have the seat number before the session was started
--the seat number is only stored in the Session; and Empties don't have seat numbers.
--yet, we need the seat number to create the gaming session.
withSeatNumbers :: Int -> [PlayerSlot] -> [(Int,PlayerSlot)]
withSeatNumbers _ [] = []
withSeatNumbers startAt (s:ss) = (startAt, s) : withSeatNumbers (startAt+1) ss


seatCheckinWidget :: MetaTable -> PlayerId -> Int ->  Widget
seatCheckinWidget table playerId seatNumber = do
  addScriptRemote externalJquery
  elemId <- lift $ newIdent
  let tid = toPathPiece $ entityKey table
  let pid = toPathPiece $ playerId
  toWidget[julius|
        $('##{elemId}').click(function(){
            $.ajax({
              type: 'POST',
              url:'@{GamingSessionsR}', 
              data: { player: #{show pid}, table: #{show tid}, seat: #{show seatNumber} },
              success: function(data){document.location="@{GamingSessionsR}"},
              error: function(jqxhr,textStatus,errorThrown){ alert(textStatus + ': ' + errorThrown); },
           });
            
        });
      |]
  $(widgetFile "seatCheckinWidget")

seatCheckoutWidget :: GamingSessionId -> MetaPlayer -> Int ->  Widget
seatCheckoutWidget sessionId player seatNumber = do
  addScriptRemote externalJquery
  elemId <- lift $ newIdent
  let playerName' = playerName $ entityVal $ player
  toWidget[julius|
          $('##{elemId}').click(function(){
           $.ajax({
              type: 'POST',
              url:'@{GamingSessionCloseR sessionId}', 
              //success: function(data){ $('##{elemId}').remove();  },
              success: function(data){document.location="@{GamingSessionsR}"},
              error: function(jqxhr,textStatus,errorThrown){ alert(textStatus + ': ' + errorThrown); },
              dataType: "html"
           } );
          
          });
  |]
  $(widgetFile "seatCheckoutWidget")

--Helper functins
tableId t _ = entityKey t
tableId t _ = entityKey t

playerId :: PlayerSlot
playerId = undefined


isEmpty :: PlayerSlot -> Bool
isEmpty Empty = True
isEmpty _ = False

externalJquery = "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
