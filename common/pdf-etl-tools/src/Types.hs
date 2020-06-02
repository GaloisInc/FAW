{- | Types - types needed globally
-}

module Types where

-- system:
import           Data.List
import qualified Data.Text as T

-- package: mongoDB
import qualified Database.MongoDB as DB

-- local:
import InvocationTypes

---- types -------------------------------------------------------------------

-- | the global options (CL options post-processed and passed to the program)
data Globals =
  Globals
    { g_database     :: T.Text
    , g_collection   :: T.Text
    , g_server       :: DB.Host -- shows/reads as "hostname[:port]"
    , g_verbose      :: Verbose
    , g_invokerList  :: [Invoker_Src]
    }
  deriving (Eq,Show)


data TimeoutOverride = TO_NoOverride
                     | TO_NoTimeout
                     | TO_Timeout Int  -- ^ time in seconds
                     deriving (Eq,Show)

---- teeny Verbosity abstraction ---------------------------------------------

type Verbose = Bool
verbosely, quietly :: Verbose
verbosely = True
quietly   = False


---- abstraction over Globals ------------------------------------------------
-- ... were we to enforce abstraction.

findInvoker :: Globals -> InvokerName -> Maybe Invoker_Src
findInvoker o iname =
  find (\i-> invName i == iname) (g_invokerList o)
