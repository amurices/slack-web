{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Web.Slack.Conversations where

import Data.Aeson.TH
import GHC.Generics (Generic)
import Web.HttpApiData
import Web.FormUrlEncoded
import Web.Slack.Common
import Web.Slack.Util
import Data.Text (Text)

data ConversationsHistoryReq =
  ConversationsHistoryReq
    { conversationsHistoryReqChannel :: Text
    , conversationsHistoryReqCursor :: Maybe Text
    , conversationsHistoryReqInclusive :: Maybe Bool
    , conversationsHistoryReqLatest :: Maybe SlackTimestamp
    , conversationsHistoryReqOldest :: Maybe SlackTimestamp
    , conversationsHistoryReqLimit :: Maybe Integer
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "conversationsHistoryReq") ''ConversationsHistoryReq)

instance ToForm ConversationsHistoryReq where
  -- can't use genericToForm because slack expects booleans as 0/1
  toForm ConversationsHistoryReq{..} =
      [ ("channel", toQueryParam conversationsHistoryReqChannel)
      , ("cursor", toQueryParam conversationsHistoryReqCursor)
      , ("inclusive", toQueryParam (if (maybe False id conversationsHistoryReqInclusive) then 1::Int else 0))
      , ("latest", toQueryParam conversationsHistoryReqLatest)
      , ("oldest", toQueryParam conversationsHistoryReqOldest)
      , ("limit", toQueryParam conversationsHistoryReqLimit)
      ]

data ResponseMetadata =
  ResponseMetadata
    { responseMetadataNextCursor :: Text
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "responseMetadata") ''ResponseMetadata)

data ConversationsHistoryRsp =
  ConversationsHistoryRsp
    { conversationsHistoryRspMessages :: [Message]
    , conversationsHistoryRspHasMore :: Bool
    , conversationsHistoryRspPinCount :: Integer
    , conversationsHistoryRspResponseMetadata :: ResponseMetadata
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "conversationsHistoryRsp") ''ConversationsHistoryRsp)
