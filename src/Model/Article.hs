module Model.Article where

import           Import

import           Data.Aeson
import           Data.List  ( nub )
import           Data.Map   ( (!) )
import qualified Data.Map   as Map

data ArticleRequest =
    ArticleRequest
        { body        :: Text
        , publishDate :: UTCTime
        }
    deriving (Generic, Show)

instance FromJSON ArticleRequest

data ArticleResponse =
    ArticleResponse
        { body        :: Text
        , publishDate :: UTCTime
        , author      :: AuthorResponse
        }
    deriving (Generic, Show)

instance ToJSON ArticleResponse where
    toEncoding = genericToEncoding defaultOptions

data AuthorResponse =
    AuthorResponse
        { name   :: Text
        , email  :: Maybe Text
        , avatar :: Maybe Text
        }
    deriving (Generic, Show)

instance ToJSON AuthorResponse where
    toEncoding = genericToEncoding defaultOptions

userToResponse :: User -> AuthorResponse
userToResponse User {..} = AuthorResponse userIdent Nothing Nothing

articleToResponse :: Article -> User -> ArticleResponse
articleToResponse Article {..} user =
    ArticleResponse articleBody articlePublishDate $ userToResponse user

getArticleAuthor :: Article -> Handler User
getArticleAuthor Article {..} = do
    maybeUser <- runDB $ get articleAuthorId
    case maybeUser of
        Just user -> return user
        Nothing   -> sendResponseStatus status500 ()

returnArticle :: Article -> Handler Value
returnArticle article =
    getArticleAuthor article >>= returnJson . articleToResponse article

returnArticles :: [Article] -> Handler Value
returnArticles articles = do
    let authorIds = nub $ articleAuthorId <$> articles
    authors <- runDB $ selectList [UserId <-. authorIds] []
    let authorMap = Map.fromList $ (\(Entity uid u) -> (uid, u)) <$> authors
    let articles' =
            (\article@Article {..} ->
                 let author = authorMap ! articleAuthorId
                  in articleToResponse article author) <$>
            articles
    returnJson articles'

articleFromRequest :: Handler Article
articleFromRequest = do
    userId <- requireAuthId
    ArticleRequest {..} <- requireJsonBody
    return
        Article
            { articleBody = body
            , articlePublishDate = publishDate
            , articleAuthorId = userId
            }

ownedArticle :: ArticleId -> Handler Article
ownedArticle articleId = do
    userId <- requireAuthId
    maybeArticle <-
        runDB $
        selectFirst [ArticleId ==. articleId, ArticleAuthorId ==. userId] []
    case maybeArticle of
        Nothing                 -> sendResponseStatus status403 ()
        Just (Entity _ article) -> return article
