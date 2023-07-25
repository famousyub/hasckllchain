{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Bcc.Prelude
import           Hedgehog.Main (defaultMain)

#ifdef UNIX
import qualified Test.Bcc.Node.FilePermissions
#endif
import qualified Test.Bcc.Node.Json
import qualified Test.Bcc.Node.POM

main :: IO ()
main = defaultMain
#ifdef UNIX
  [ Test.Bcc.Node.Json.tests
  , Test.Bcc.Node.POM.tests
  , Test.Bcc.Node.FilePermissions.tests
  ]
#else
  [ Test.Bcc.Node.Json.tests
  , Test.Bcc.Node.POM.tests
  ]
#endif

