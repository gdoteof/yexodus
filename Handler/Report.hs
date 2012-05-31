module Handler.Report( getTop100R
       , getReportFormR
       , postReportFormR
       ) where

import Import
import Handler.Meta
import Helpers.Models
import Data.Time.Clock
import Data.Time (Day)
import Data.Time.LocalTime
import Data.Maybe (fromJust)
import Yesod.Form.Jquery
import Text.Groom
import Data.List (groupBy, sort, sortBy)
import Data.Function (on)
import Data.List (head)
import Control.Arrow
import Text.CSV
import Data.Text (pack,unpack)

data PlayerReport = PlayerReport {
     startUTC   :: UTCTime 
   , endUTC     :: UTCTime 
   , numPlayers :: Maybe Int
   }


type EarnedMinutes = Int
type GivenMinutes = Int
type EarnedPoints = Double
type GivenPoints = Double

type PrintablePR = [(Entity Player, [Entity GamingSession], [Entity ManualSession], EarnedMinutes, EarnedPoints, GivenMinutes, GivenPoints)]

    
createPrintableReport :: PlayerReport -> Handler PrintablePR
createPrintableReport pr = do 
    user <- requireAuth
    let userId = (fromJust $ userAccount $ entityVal user)
    report <- runDB $ do
        sessions   <- selectList [GamingSessionStart >=. startUTC pr , GamingSessionEnd <=. Just (endUTC pr) ] 
                                 [Desc GamingSessionStart]
        players    <- selectList [PlayerAccount ==. userId] []
        manSessions <- selectList [ManualSessionAccount ==. userId] []
        let gsPlayer = joinTables gamingSessionPlayer sessions players
        let msPlayer = joinTables manualSessionPlayer manSessions players
        let gsPlayer' = collapse' $ sortBy soryByPlayer gsPlayer
        let msPlayer' = collapse' $ sortBy soryByPlayer msPlayer
        let report' = reverse.sortBy sortByTime $ marry'  gsPlayer' msPlayer'
        return report' 

    case numPlayers pr of
        Just num -> return $ take num report
        Nothing -> return report
    where collapse' l = map (map fst &&& snd.head)  $  groupBy ((==) `on` snd) l
          soryByPlayer (s1,p1) (s2,p2) 
            | p1 > p2 = GT
            | p1 < p2 = LT
            | otherwise = EQ
          sortByTime (_,_,_,time,_,_,_) (_,_,_,time2,_,_,_)
            | time > time2 = GT
            | time < time2 = LT
            | otherwise = EQ
          marry' :: [([Entity GamingSession], Entity Player)] ->  [([Entity ManualSession], Entity Player)] -> PrintablePR
          marry' [] [] = []
          marry' [] (y:ys)  = (snd y, []   , fst y, 0                 , 0                , sumManMinutes (fst y), sumManPoints (fst y)) : []
          marry' (x:xs) []  = (snd x, fst x, []   , sumMinutes (fst x), sumPoints (fst x), 0                    , 0                   ) : []
          marry' gs@(x:xs) ms@(y:ys) 
            | (snd x == snd y) = (snd x, fst x, fst y, sumMinutes (fst x), sumPoints (fst x), sumManMinutes (fst y), sumManPoints (fst y)) : marry' xs ys
            | (snd x < snd y) =  (snd x, fst x, []   , sumMinutes (fst x), sumPoints (fst x), 0 ,                    0                   ) : marry' xs ms
            | otherwise       =  (snd y, []   , fst y, 0                 , 0                , sumManMinutes (fst y), sumManPoints (fst y)) : marry' gs ys

          sumMinutes :: [Entity GamingSession] -> EarnedMinutes
          sumMinutes = sum . map gamingSessionMinutes 
          sumPoints :: [Entity GamingSession] -> EarnedPoints
          sumPoints gss = sum . map (gamingSessionPoints . entityVal) $ gss
          sumManMinutes :: [Entity ManualSession] -> GivenMinutes
          sumManMinutes mss = sum . map (manualSessionMinutes . entityVal) $ mss
          sumManPoints :: [Entity ManualSession] -> GivenPoints
          sumManPoints mss = sum . map (manualSessionPoints . entityVal) $ mss




type EndDay = Day
type StartDay = Day
type NumPlayers = Maybe Int

data ReportSubmission = ReportSubmission StartDay EndDay NumPlayers


reportForm :: AccountId -> Form ReportSubmission
reportForm accountId = renderDivs $ ReportSubmission <$>
        areq  (jqueryDayField (JqueryDaySettings True True "c-5:now" (Left 1))) "Start" Nothing -- (my settings for "at least 18 years old but not older than 80")
    <*> areq  (jqueryDayField (JqueryDaySettings True True "c-5:now" (Left 1))) "End"   Nothing -- (my settings for "at least 18 years old but not older than 80")
    <*> aopt intField "How many (leave blank for all)" Nothing 

getTop100R :: Handler RepHtml
getTop100R = do
    user <- requireAuth
    case (userAccount $ entityVal user) of 
        Just accountId -> do 
            report <- runDB $ selectList [PlayerAccount ==. accountId] [Desc PlayerMinutes, LimitTo 100]
            defaultLayout $(widgetFile "report/top100")
        _ -> do 
            noAccount

withNumbers :: Int -> [Entity Player] -> [(Int,Entity Player)]
withNumbers _ [] = []
withNumbers startAt (s:ss) = (startAt, s) : withNumbers (startAt+1) ss

getReportFormR :: Handler RepHtml
getReportFormR = do
    user <- requireAuth
    (reportFormWidget,enctype) <- generateFormPost $ reportForm (fromJust $ userAccount $ entityVal user)
    defaultLayout $ do
        $(widgetFile "report/getReportForm")

postReportFormR :: Handler RepHtml
postReportFormR = do
    user <- requireAuth
    account <- case (userAccount $ entityVal user) of 
        Just accountId -> do 
            runDB $ get accountId
        _              -> noAccount -- redirect user on no account
    ((res,reportFormWidget),enctype) <- runFormPost $ reportForm (fromJust $ userAccount $ entityVal user)
    case res of
        FormSuccess (ReportSubmission startDay endDay numPlayers) -> do
            let startUTC' = localTimeToUTC (accountTimeZone account) $ LocalTime startDay midnight
            let endUTC'   = localTimeToUTC (accountTimeZone account) $ LocalTime endDay   midnight
            report <- createPrintableReport $ PlayerReport startUTC' endUTC' numPlayers
            --let csvReady = map (\(eplayer,esessions,eMsessions,earnedMinutes,earnedPoints,manualMinutes,manualPoints)-> [(unpack $ playerName $ entityVal eplayer), (show $ length esessions), (show $ length eMsessions) , (show $ earnedMinutes), (show $  earnedPoints) , (show $ manualMinutes), (show $ manualPoints), (show $  (+) earnedMinutes manualMinutes), (show $  (+) earnedPoints manualPoints)]) report
            --liftIO $ putStrLn $ printCSV $ ["Player", "# Sessions", " # Audits", "Time Clocked", "Points Earned", "Time Auditing", "Points Auditing", "Total Time", "Total Points"] : csvReady
            defaultLayout $ do
                $(widgetFile "report/postReportForm")
        _ -> do
            setMessage "Form Failure"
            redirect ReportFormR
