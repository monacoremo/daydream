{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PostgrestToElm.ApiStructure
  ( ApiStructure (..),
    ApiTable (..),
    ApiProc (..),
    fromDbStructure,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Int (Int32)
import qualified Hasql.Connection as Hasql
import qualified PostgREST.Types as PostgREST

-- API TYPES

data ApiStructure
  = ApiStructure
      { apiTables :: [ApiTable],
        apiProcs :: [ApiProc]
      }
  deriving (Generic, Show, Eq, Aeson.ToJSON)

data ApiTable
  = ApiTable
      { apiTableName :: Text,
        apiTableCols :: [ApiTableCol],
        apiTableInsertable :: Bool
      }
  deriving (Generic, Show, Eq, Aeson.ToJSON)

data ApiTableCol
  = ApiTableCol
      { apiTableColName :: Text,
        apiTableColType :: Text,
        apiTableColPosition :: Int32,
        apiTableColNullable :: Bool,
        apiTableColUpdatable :: Bool,
        apiTableColHasDefault :: Bool,
        apiTableColPrimaryKey :: Bool,
        apiTableColForeignKey :: Bool
      }
  deriving (Generic, Show, Eq, Aeson.ToJSON)

data ApiProc
  = ApiProc
      { apiProcName :: Text,
        apiProcArgs :: [ApiProcArg],
        apiProcReturnsScalar :: Bool,
        apiProcReturnsSet :: Bool
      }
  deriving (Generic, Show, Eq, Aeson.ToJSON)

data ApiProcArg
  = ApiProcArg
      { apiProcArgName :: Text,
        apiProcArgType :: Text,
        apiProcArgRequired :: Bool
      }
  deriving (Generic, Show, Eq, Aeson.ToJSON)

-- CONVERT POSTGREST TO API TYPES

fromDbStructure :: Text -> Hasql.Connection -> PostgREST.DbStructure -> IO ApiStructure
fromDbStructure schema _ dbStructure =
  return
    ApiStructure
      { apiTables =
          map (parseTable dbStructure)
            $ filter (\t -> PostgREST.tableSchema t == schema)
            $ PostgREST.dbTables dbStructure,
        apiProcs =
          map parseApiProc $ concat $ HashMap.elems $ PostgREST.dbProcs dbStructure
      }

parseApiProc :: PostgREST.ProcDescription -> ApiProc
parseApiProc proc =
  ApiProc
    { apiProcName = PostgREST.pdName proc,
      apiProcArgs = map parseProcArg $ PostgREST.pdArgs proc,
      apiProcReturnsSet = case PostgREST.pdReturnType proc of
        PostgREST.Single _ ->
          False
        PostgREST.SetOf _ ->
          True,
      apiProcReturnsScalar = procReturnsScalar proc
    }

parseProcArg :: PostgREST.PgArg -> ApiProcArg
parseProcArg arg =
  ApiProcArg
    { apiProcArgName = PostgREST.pgaName arg,
      apiProcArgType = PostgREST.pgaType arg,
      apiProcArgRequired = PostgREST.pgaReq arg
    }

procReturnsScalar :: PostgREST.ProcDescription -> Bool
procReturnsScalar proc =
  let retType =
        PostgREST.pdReturnType proc
   in case retType of
        PostgREST.Single (PostgREST.Scalar _) ->
          True
        _ ->
          False

parseTable :: PostgREST.DbStructure -> PostgREST.Table -> ApiTable
parseTable dbStructure table =
  ApiTable
    { apiTableName = PostgREST.tableName table,
      apiTableCols =
        map (parseTableCol dbStructure)
          $ filter (\c -> PostgREST.colTable c == table)
          $ PostgREST.dbColumns dbStructure,
      apiTableInsertable = PostgREST.tableInsertable table
    }

parseTableCol :: PostgREST.DbStructure -> PostgREST.Column -> ApiTableCol
parseTableCol dbStructure col =
  ApiTableCol
    { apiTableColName = PostgREST.colName col,
      apiTableColType = PostgREST.colType col,
      apiTableColPosition = PostgREST.colPosition col,
      apiTableColNullable = PostgREST.colNullable col,
      apiTableColUpdatable = PostgREST.colUpdatable col,
      apiTableColHasDefault = Maybe.isJust $ PostgREST.colDefault col,
      apiTableColPrimaryKey = isPrimaryKey dbStructure col,
      apiTableColForeignKey = Maybe.isJust $ PostgREST.colFK col
    }

isPrimaryKey :: PostgREST.DbStructure -> PostgREST.Column -> Bool
isPrimaryKey dbStructure col =
  let pk =
        PostgREST.PrimaryKey
          { PostgREST.pkTable = PostgREST.colTable col,
            PostgREST.pkName = PostgREST.colName col
          }
   in elem pk (PostgREST.dbPrimaryKeys dbStructure)
