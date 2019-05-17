module Handler.Article where

import Import

getArticleR :: ArticleId -> Handler Value
getArticleR articleId = runDB $ get404 articleId >>= returnJson

putArticleR :: ArticleId -> Handler Value
putArticleR articleId = do
    newArticle <- requireJsonBody
    requireOwnedArticle articleId >> runDB (replace articleId newArticle) >>=
        returnJson

deleteArticleR :: ArticleId -> Handler Value
deleteArticleR articleId =
    requireOwnedArticle articleId >> runDB (delete articleId) >> sendResponse ()

requireOwnedArticle :: ArticleId -> Handler (Entity Article)
requireOwnedArticle articleId = do
    userId <- requireAuthId
    maybeArticle <-
        runDB $
        selectFirst [ArticleId ==. articleId, ArticleAuthorId ==. userId] []
    case maybeArticle of
        Nothing      -> sendResponseStatus status403 ()
        Just article -> return article
