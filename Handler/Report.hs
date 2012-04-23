module Handler.Report(getReportR) where

import Import
import Handler.Meta

getReportR :: Handler RepHtml
getReportR = do
        report <- runDB $ selectList [] [Desc PlayerMinutes, LimitTo 100]
        defaultLayout $(widgetFile "Report")

withSeatNumbers :: Int -> [Entity Player] -> [(Int,Entity Player)]
withSeatNumbers _ [] = []
withSeatNumbers startAt (s:ss) = (startAt, s) : withSeatNumbers (startAt+1) ss

