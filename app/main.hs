import Prelude              (IO, (>>=))
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)
import Network.Wai.Handler.FastCGI (run)
import System.Environment

main :: IO ()
main = do
   env <- getEnv "YESOD_ENVIRONMENT"
   withArgs [env] (fromArgs parseExtra >>= makeApplication >>= run)
