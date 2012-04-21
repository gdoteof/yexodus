module Handler.Meta(
        checkinWidget
) where

import Import
import Helpers.Models
import Data.Maybe

type MetaTable         = Entity Table
type MetaGamingSession = Entity GamingSession
type MetaPlayer        = Entity Player

data PlayerSlot         = Empty | PlayerSlot MetaPlayer MetaGamingSession deriving (Show, Eq)

data GamingTable        = GamingTable { table       :: MetaTable
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
        let emptyTable = take 10 $ repeat Empty
        liftIO $ putStrLn $ show emptyTable
        return $ GamingTable table []

getPlayerSlot :: MetaGamingSession -> Handler PlayerSlot
getPlayerSlot session =  do
        let pid = gamingSessionPlayer (entityVal session)
        mplayer <- runDB $ get pid
        case mplayer of
            Nothing     -> return Empty
            Just player -> return $ PlayerSlot (Entity pid player) session
        

checkinWidget :: Widget
checkinWidget = do
        serialized <- lift $  getSeatingArrangement
        toWidget [whamlet|"CheckinWidget ---->#{show serialized}<----"|]

