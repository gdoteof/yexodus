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
                                             } deriving (Show, Eq)

instance Ord PlayerSlot where
        (PlayerSlot _ s1) `compare` (PlayerSlot _ s2) = 
                (fromJust $ gamingSessionSeat $ entityVal s1) `compare` (fromJust $ gamingSessionSeat$ entityVal s2)

data GamingTable        = GamingTable { gamingTableTable       :: MetaTable
                                      , playerSlots :: [PlayerSlot]
                                      } deriving (Show)

data SeatingArrangement = SeatingArrangement [GamingTable] deriving (Show)
                          
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
        -- liftIO $ putStrLn $ show seatedTable
        return $ GamingTable table seatedTable

getPlayerSlot :: MetaGamingSession -> Handler PlayerSlot
getPlayerSlot session =  do
        let pid = gamingSessionPlayer (entityVal session)
        mplayer <- runDB $ get pid
        case mplayer of
            Nothing     -> return Empty --should be catching exception here
            Just player -> return $ PlayerSlot (Entity pid player) session
        
slotSeat :: PlayerSlot -> Maybe Int
slotSeat a = case a of
                  Empty -> Nothing
                  PlayerSlot _ s -> gamingSessionSeat $ entityVal s 

fillEmpties :: Int -> [PlayerSlot] -> [PlayerSlot]
fillEmpties 0 _ = []
fillEmpties n [] = take n $ repeat Empty
fillEmpties n slots@(s:ss) = if fromJust (slotSeat s) == n
                  then s : fillEmpties (n-1) ss
                  else Empty : fillEmpties (n-1) slots

checkinWidget :: Widget
checkinWidget = do
        serialized <- lift $  getSeatingArrangement
        toWidget [whamlet|"CheckinWidget ---->#{show serialized}<----"|]

