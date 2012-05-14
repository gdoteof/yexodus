module Handler.Account
    ( getAccountR
    , getAccountListR
    , postAccountListR
    , getAccountEditR
    , postAccountEditR
    , postAccountAddUserR
    )
where

import Import
import Data.Maybe

accountForm :: Form Account
accountForm = renderDivs $ Account
    <$> areq   textField "Name" Nothing


getAccountR :: AccountId -> Handler RepHtml
getAccountR accountId = do
        account <- runDB $ get404 accountId
        defaultLayout $(widgetFile "account/account")
        
getAccountListR :: Handler RepHtml
getAccountListR = do
        accounts <- runDB $ selectList [] [Asc AccountName]
        (accountWidget, enctype) <- generateFormPost $ accountForm
        defaultLayout $ do
            $(widgetFile "account/accounts")

postAccountListR :: Handler RepHtml
postAccountListR = do
    ((res,accountWidget),enctype) <- runFormPost $ accountForm
    case res of 
         FormSuccess account -> do 
            accountId <- runDB $ insert account
            redirect $ AccountR accountId
         _ -> defaultLayout [whamlet|Form error|]

getAccountEditR :: AccountId -> Handler RepHtml
getAccountEditR accountId = do
        account <- runDB $ get404 accountId
        (accountWidget, enctype) <- generateFormPost $ accountFormDefaults $ Entity accountId account
        (accountAddUserWidget, enctype2) <- generateFormPost $ accountAddUserForm $ accountId
        defaultLayout $ do 
          setTitle "Account Exit"
          $(widgetFile "account/accountEdit")

accountFormDefaults :: Entity Account -> Form Account
accountFormDefaults eaccount = renderDivs $ Account
    <$> areq   textField "Name" (Just $ accountName $ entityVal $ eaccount)

postAccountEditR :: AccountId -> Handler RepHtml
postAccountEditR = undefined

accountAddUserForm :: AccountId -> Form (Text, AccountId)
accountAddUserForm accountId = renderDivs $ (,)
    <$> areq   textField "User email address" Nothing 
    <*> pure accountId

postAccountAddUserR :: AccountId -> Handler RepHtml
postAccountAddUserR accountId = do
    ((res,accountAddUserWidget),enctype) <- runFormPost $ accountAddUserForm accountId
    case res of
        FormSuccess (email, accountId) -> do
            meemail <- runDB $ getBy $ UniqueEmail email
            case meemail of
                Just eemail  -> do
                    runDB $ update (fromJust $ emailUser $ entityVal eemail) [UserAccount =. (Just accountId)]
                    setMessage $ toHtml $ email `mappend` " added to account" 
                    redirect $ AccountR accountId
                Nothing -> do
                    setMessage $ toHtml $ "No user registered with " `mappend` email
                    redirect $ AccountEditR accountId
