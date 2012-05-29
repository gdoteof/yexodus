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
import Data.List (groupBy)
import Data.Function (on)
import Data.List (head)
import Control.Arrow


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

getReport :: PlayerReport -> Handler PrintablePR
getReport pr = do 
    user <- requireAuth
    let userId = (fromJust $ userAccount $ entityVal user)
    report <- runDB $ do
        sessions   <- selectList [GamingSessionStart >=. startUTC pr , GamingSessionEnd <=. Just (endUTC pr) ] 
                                 [Desc GamingSessionStart]
        players    <- selectList [PlayerAccount ==. userId] []
        manSessions <- selectList [ManualSessionAccount ==. userId] []
        let gsPlayer = joinTables gamingSessionPlayer sessions players
        let msPlayer = joinTables manualSessionPlayer manSessions players
        return $ zipWith (\x y -> ( (snd x)
                                  , (fst x)
                                  , (fst y)
                                  , (sumMinutes (fst x))
                                  , (sumPoints (fst x))
                                  , sumManMinutes (fst y)
                                  , sumManPoints (fst y)
                                  )) 
                         (collapse' gsPlayer) 
                         (collapse' msPlayer)
    return  report
    where collapse' l = map (map fst &&& snd.head)  $  groupBy ((==) `on` snd) l
          sumMinutes :: [Entity GamingSession] -> Int
          sumMinutes = sum . map gamingSessionMinutes 

          sumPoints :: [Entity GamingSession] -> Double
          sumPoints gss = sum . map (gamingSessionPoints . entityVal) $ gss

          sumManMinutes :: [Entity ManualSession] -> Int
          sumManMinutes mss = sum . map (manualSessionMinutes . entityVal) $ mss

          sumManPoints :: [Entity ManualSession] -> Double
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
        FormSuccess (ReportSubmission startDay endDay numPlayers) ->
            defaultLayout $ do
                let startUTC' = localTimeToUTC (accountTimeZone account) $ LocalTime startDay midnight
                let endUTC'   = localTimeToUTC (accountTimeZone account) $ LocalTime endDay   midnight
                dbData <- lift $ runDB $ do
                    sessions <- selectList [GamingSessionStart >=. startUTC', GamingSessionEnd <=. Just endUTC'] [Desc GamingSessionStart]
                    players  <- selectList [PlayerAccount ==. (fromJust $ userAccount $ entityVal user)] []
                    return $ joinTables gamingSessionPlayer sessions players
                let report = map (map fst &&& snd.head)  $  groupBy ((==) `on` snd) dbData
                
                liftIO $ putStrLn $ groom  $ report
                $(widgetFile "report/postReportForm")
        _ -> do
            setMessage "Form Failure"
            redirect ReportFormR
