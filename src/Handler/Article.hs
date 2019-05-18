module Handler.Article where

import Import

import Model.Article

getArticleR :: ArticleId -> Handler Value
getArticleR articleId = runDB (get404 articleId) >>= returnArticle

putArticleR :: ArticleId -> Handler Value
putArticleR articleId = do
    article <- articleFromRequest
    ownedArticle articleId >> runDB (replace articleId article) >>
        returnArticle article

deleteArticleR :: ArticleId -> Handler Value
deleteArticleR articleId =
    ownedArticle articleId >> runDB (delete articleId) >> sendResponse ()
