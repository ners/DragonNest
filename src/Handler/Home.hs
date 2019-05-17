module Handler.Home where

import Import

getHomeR :: Handler Value
getHomeR = sendResponseStatus status404 ()
