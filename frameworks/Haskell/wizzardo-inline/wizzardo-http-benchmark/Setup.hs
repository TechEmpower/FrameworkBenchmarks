import Distribution.Simple
import Language.Java.Inline.Cabal (addJarsToClasspath, gradleHooks)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))

main = do
    here <- getCurrentDirectory
    defaultMainWithHooks $
      addJarsToClasspath [here </> "build/libs/wizzardo-http-benchmark.jar"] $
      gradleHooks simpleUserHooks
