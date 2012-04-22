module Handler.Meta(
        checkinWidget
) where

import Import
import Helpers.Models
import Data.Maybe
import Data.List (sort)

type MetaTable         = Entity Table
type MetaGamingSession = Entity GamingSession
type MetaPlayer        = Entity Player

data PlayerSlot         = Empty | PlayerSlot { playerSlotPlayer :: MetaPlayer
                                             , playerSlotSession :: MetaGamingSession 
                                             , playerSlotSeat :: Int
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
        let seatedTable = reverse $ fillEmpties (tableSeats $ entityVal table) $ sort playerSlots
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

fillEmpties :: Int -> [PlayerSlot] -> [PlayerSlot]
fillEmpties 0 _ = []
fillEmpties n [] = take n $ repeat Empty
fillEmpties n slots@(s:ss) = if fromJust (slotSeat s) == n
                  then s : fillEmpties (n-1) ss
                  else Empty : fillEmpties (n-1) slots

checkinWidget :: PlayerId -> Widget
checkinWidget playerId = do
        seatingArrangement <- lift $  getSeatingArrangement
        let tables = gamingTables seatingArrangement
        $(widgetFile "checkinWidget")
        where
          withSeatNumbers _ [] = []
          withSeatNumbers startAt (s:ss) = ((startAt), s) : withSeatNumbers (startAt+1) ss




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


--Helper functins
tableId t _ = entityKey t

playerId :: PlayerSlot
playerId = undefined


isEmpty :: PlayerSlot -> Bool
isEmpty Empty = True
isEmpty _ = False

externalJquery = "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
