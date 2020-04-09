module PostgrestToElm.DbStructure
  ( get,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text
import GHC.Generics (Generic)
import GHC.Int (Int32)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as HT
import PostgREST.DbStructure (getDbStructure, getPgVersion)
import qualified PostgREST.Types as PostgREST
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

get :: Connection.Connection -> [PostgREST.Schema] -> Text -> IO PostgREST.DbStructure
get connection schema role =
  do
    Right pgVersion <- Session.run getPgVersion connection
    Right dbStructure <-
      Session.run
        (HT.transaction HT.RepeatableRead HT.Read $ getDbStructure schema pgVersion)
        connection
    return dbStructure
