module Handler.Articles where

import Import

import Model.Article

getArticlesR :: Handler Value
getArticlesR = do
    now <- liftIO getCurrentTime
    articles <-
        runDB $
        selectList [ArticlePublishDate <=. now] [Desc ArticlePublishDate]
    returnArticles $ entityVal <$> articles

postArticlesR :: Handler Value
postArticlesR = do
    article <- requireJsonBody :: Handler Article
    userId <- requireAuthId
    article' <- runDB $ insertEntity $ article {articleAuthorId = userId}
    returnArticle $ entityVal article'
