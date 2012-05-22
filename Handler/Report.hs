module Handler.Report( getTop100R
       , getReportFormR
        ,) where

import Import
import Handler.Meta
import Data.Time.Clock
import Data.Time (Day)
import Data.Maybe (fromJust)
import Yesod.Form.Jquery

type EndDay = Day
type StartDay = Day
type NumPlayers = Maybe Int

data ReportSubmission = ReportSubmission StartDay EndDay NumPlayers


reportForm :: AccountId -> Form ReportSubmission
reportForm accountId = renderDivs $ ReportSubmission
    <$> areq  (jqueryDayField def { jdsChangeYear = True
        , jdsYearRange = "-1:0" 
        }) "Start" Nothing
    <*> areq  (jqueryDayField def
        { jdsChangeYear = True
        , jdsYearRange = "-1:0" 
        }) "End" Nothing
    <*> aopt intField "How many" (Just (Just 0)) --This seems weird

getTop100R :: Handler RepHtml
getTop100R = do
    user <- requireAuth
    case (userAccount $ entityVal user) of 
        Just accountId -> do 
            report <- runDB $ selectList [] [Desc PlayerMinutes, LimitTo 100]
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
    defaultLayout $(widgetFile "report/getReportForm")
