module Handler.Articles where

import Import

getArticlesR :: Handler Value
getArticlesR = do
    now <- liftIO getCurrentTime
    articles <-
        runDB $
        selectList [ArticlePublishDate <=. now] [Desc ArticlePublishDate]
    returnJson articles

postArticlesR :: Handler Value
postArticlesR = do
    article <- requireJsonBody :: Handler Article
    userId <- requireAuthId
    let article' = article {articleAuthorId = userId}
    insertedArticle <- runDB $ insertEntity article'
    returnJson insertedArticle
