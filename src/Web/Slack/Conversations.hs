{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Web.Slack.Conversations where

import Data.Aeson.TH
import GHC.Generics (Generic)
import Web.HttpApiData
import Web.FormUrlEncoded
import Web.Slack.Common
import Web.Slack.Util
import Web.Slack.Channel (Channel (..))
import Data.Text (Text)
import Data.Maybe (catMaybes)
import GHC.Exts (fromList)
import System.IO.Unsafe (unsafePerformIO)

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
  toForm ConversationsHistoryReq{..}
    = fromList
    . catMaybes
    $ [ Just . ("channel", ) . toQueryParam $ conversationsHistoryReqChannel
      , ("inclusive", )
        . toQueryParam
        . (\b -> if b then 1::Int else 0)
        <$> conversationsHistoryReqInclusive
      , ("cursor", ) . toQueryParam <$> conversationsHistoryReqCursor
      , ("latest", ) . toQueryParam <$> conversationsHistoryReqLatest
      , ("oldest", ) . toQueryParam <$> conversationsHistoryReqOldest
      , ("limit", ) . toQueryParam <$> conversationsHistoryReqLimit
      ]

mkConversationsHistoryReq :: Text -> ConversationsHistoryReq
mkConversationsHistoryReq channel = ConversationsHistoryReq
                                      { conversationsHistoryReqChannel = channel
                                      , conversationsHistoryReqCursor = Nothing
                                      , conversationsHistoryReqInclusive = Nothing
                                      , conversationsHistoryReqLatest = Nothing
                                      , conversationsHistoryReqOldest = Nothing
                                      , conversationsHistoryReqLimit = Nothing
                                      }

data ConversationsHistoryRsp =
  ConversationsHistoryRsp
    { conversationsHistoryRspMessages :: [Message]
    , conversationsHistoryRspHasMore :: Bool
    , conversationsHistoryRspPinCount :: Integer
    , conversationsHistoryRspResponseMetadata :: Maybe ResponseMetadata
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "conversationsHistoryRsp") ''ConversationsHistoryRsp)

-- List

data ConversationsListReq =
  ConversationsListReq
    { conversationsListReqCursor :: Maybe Text
    , conversationsListReqExcludeArchived :: Maybe Bool
    , conversationsListReqLimit :: Maybe Integer
    , conversationsListReqTypes :: Maybe Text
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "conversationsListReq") ''ConversationsListReq)

nuTap :: Show a => a -> a
nuTap x = unsafePerformIO (putStrLn $ show x) `seq` x

instance ToForm ConversationsListReq where
  -- can't use genericToForm because slack expects booleans as 0/1
  toForm ConversationsListReq{..}
    = fromList
    . catMaybes
    $ [ ("cursor", ) . toQueryParam <$> conversationsListReqCursor
      , ("exclude_archived", ) . toQueryParam <$> conversationsListReqExcludeArchived
      , ("limit",  ) . toQueryParam <$> conversationsListReqLimit
      , ("types",  ) . toQueryParam <$> conversationsListReqTypes
      ]

mkConversationsListReq :: ConversationsListReq
mkConversationsListReq = ConversationsListReq
                           { conversationsListReqCursor = Nothing
                           , conversationsListReqExcludeArchived = Nothing
                           , conversationsListReqLimit = Nothing
                           , conversationsListReqTypes = Nothing
                           }

data Purpose =
  Purpose
    { purposeValue :: Text
    , purposeCreator :: Text
    , purposeLastSet :: Integer
    }
  deriving (Eq, Generic, Show)

data Topic =
  Topic
    { topicValue :: Text
    , topicCreator :: Text
    , topicLastSet :: Integer
    }
  deriving (Eq, Generic, Show)

data Conversation =
  Conversation
    { conversationId :: Text
    , conversationName :: Maybe Text
    , conversationIsChannel :: Maybe Bool
    , conversationIsGroup :: Maybe Bool
    , conversationIsIm :: Maybe Bool
    , conversationCreated :: Maybe Integer
    , conversationCreator :: Maybe UserId
    , conversationIsArchived :: Bool
    , conversationIsGeneral :: Maybe Bool
    , conversationUnlinked :: Maybe Integer
    , conversationNameNormalized :: Maybe Text
    , conversationIsShared :: Maybe Bool
    , conversationIsExtShared :: Maybe Bool
    , conversationIsOrgShared :: Maybe Bool
    -- , conversationPendingShared :: Not documented
    , conversationIsPendingExtShared :: Maybe Bool
    , conversationIsMember :: Maybe Bool
    , conversationIsPrivate :: Maybe Bool
    , conversationIsMpim :: Maybe Bool
    , conversationTopic :: Maybe Topic
    , conversationPurpose :: Maybe Purpose
    , conversationPreviousNames :: Maybe [Text]
    , conversationNumMembers :: Maybe Integer
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "conversation") ''Conversation)
$(deriveJSON (jsonOpts "purpose") ''Purpose)
$(deriveJSON (jsonOpts "topic") ''Topic)

data ConversationsListRsp =
  ConversationsListRsp
    { conversationsListRspChannels :: [Conversation]
    , conversationsListRspResponseMetadata :: Maybe ResponseMetadata
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "conversationsListRsp") ''ConversationsListRsp)
