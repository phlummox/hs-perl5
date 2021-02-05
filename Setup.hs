

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

{- |

The contents needed for the "cc-options", "ld-options",
and "extra-libraries" Cabal-file fields are obtained by
calling functions from the Perl @ExtUtils::Embed@ module (see the
documentation for it at the
<https://perldoc.perl.org/ExtUtils::Embed Perldoc website>).

These are output to a .buildinfo file, which cabal picks up and
uses to augment the fields from the .cabal file: see
<https://www.haskell.org/cabal/release/cabal-1.22.2.0/doc/users-guide/developing-packages.html#system-dependent-parameters the cabal docco>.

Currently this Setup file is known to work with versions of Cabal from
>= 1.24 to <= 3.0.
On Linux.

-}
import Control.Monad

import Distribution.Simple
import Distribution.Verbosity                 (Verbosity)
import Distribution.Simple.Utils              (
                                                info
                                              , warn
#if MIN_VERSION_Cabal(2,2,0)
                                              , findHookedPackageDesc
#endif
                                              )
import Distribution.PackageDescription        ( HookedBuildInfo
                                              ,updatePackageDescription
                                              ,emptyHookedBuildInfo
                                              , PackageDescription(..) )
import Distribution.Simple.LocalBuildInfo     (localPkgDescr
                                              , LocalBuildInfo
                                              , buildDir )
import Distribution.Simple.Setup              (ConfigFlags(..)
                                              , fromFlag
                                              , configVerbosity
                                              , BuildFlags(..)
                                              )
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec (readHookedBuildInfo)
#else
import Distribution.PackageDescription.Parse  (readHookedBuildInfo)
#endif

import System.Process (readCreateProcess, shell)
import Data.Maybe (fromMaybe)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- if you want to see what's being executed, you can
--   #define VERBOSE_CONFIGURE 1
-- (or give ghc options which will do so for you).


is_verbose_configure :: Bool
is_verbose_configure =
#if VERBOSE_CONFIGURE
  True
#else
  False
#endif


-- |
-- List of
-- @
--    (field-name, perl expression to get field body)
-- @
-- to be output to a .buildinfo file
buildInfoFields :: [(String,String)]
buildInfoFields = [
  -- All perl expressions:
  -- - are run with the module ExtUtils::Embed loaded.
  -- - are wrapped in single quotes when passed to the shell,
  --   so don't use single quotes in your expression.
  --
  -- NB when invoked with no params, "ccopts" and "ldopts" output
  -- results to stdout

  ("cc-options",        "ccopts")

                        -- ^ output: typically something like:
                        --     -D_REENTRANT -D_GNU_SOURCE -DDEBIAN -fwrapv
                        --     -fno-strict-aliasing -pipe
                        --     -I/usr/local/include -D_LARGEFILE_SOURCE
                        --     -D_FILE_OFFSET_BITS=64
                        --     -I/usr/lib/x86_64-linux-gnu/perl/5.26/CORE

  ,("ld-options",       "ldopts")

                        -- ^ output: typically something like:
                        --     -Wl,-E  -fstack-protector-strong
                        --     -L/usr/local/lib
                        --     -L/usr/lib/x86_64-linux-gnu/perl/5.26/CORE
                        --     -lperl -ldl -lm -lpthread -lc -lcrypt

  , ("extra-libraries", "print(join \" \", (map { $_ =~ s/^-l//; $_; } " ++
                        "( grep /^-l/, (split \" \", ldopts(1)))))")

                        -- ^ output: just the libraries to link against: e.g.
                        --      perl dl m pthread c crypt

  , ("include-dirs",    unlines [
                            "my $output;"
                          -- .. capture STDOUT to $output:
                          , "{"
                          , "open local(*STDOUT), \">\", \\$output;"
                          , "perl_inc();"
                          , "}"
                          , "$output =~ s/^\\s+-I//;"
                          , "print $output;"
                        ])

                        -- ^ output: just the include dirs: e.g.
                        --      /usr/lib/x86_64-linux-gnu/perl/5.26/CORE

  , ("extra-lib-dirs",  "print(join \", \", (map { $_ =~ s/^-L//; $_; } "
                        ++ "( grep /^-L/, (split \" \", ldopts(1)))))")

                        -- ^ output: just the lib dirs: e.g.
                        --      /usr/local/lib
                        --      /usr/lib/x86_64-linux-gnu/perl/5.26/CORE
  ]

-- | turn @buildInfoFields@ above into an
-- associative list, mapping from field to value.
get_buildinfo_fields :: IO [(String, String)]
get_buildinfo_fields =
    forM buildInfoFields $ \(fld, perl_expr) -> do
            let cmd  = "perl -MExtUtils::Embed -e '"
                          ++ perl_expr ++ "'"
            let cmd' = if is_verbose_configure
                       then "set -x; " ++ cmd
                       else cmd
            (fld,) <$> readCreateProcess (shell cmd') ""

-- Setup will get normally be called (at least) twice --
-- once for `configure`, once for `build`.
--
-- We do writeBuildInfo regardless, every time, 'cos
-- no harm in doing so. (Not normally, anyway.)
main :: IO ()
main = do
          buildInfoFields <- get_buildinfo_fields
          let buildInfoConts = unlines
               [ line | (fld, val) <- buildInfoFields,
                        let line = fld ++ ": " ++ val
               ]
          writeFile "hs-perl5.buildinfo" buildInfoConts
          defaultMainWithHooks myUserHooks
  where
  myUserHooks = autoconfUserHooks {
        postConf = customPostConf
      , preBuild = customPreBuild
      }

  -- we override preBuild, because our .buildinfo file is
  -- in a non-usual place (namely, the working directory, instead of somewhere
  -- in a dist/ or new-dist/ directory).
  -- (tho in Cabal < 2.2, it seems this can be left off.)
  customPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
  customPreBuild args flags =
      getHookedBuildInfo verbosity
    where
      verbosity = fromFlag (buildVerbosity flags)

  -- more or less duplicates behaviour of old "defaultUserHooks" -- copied
  -- from Distribution.Simple. Feel free to fix/tidy if time
  -- or inclination is available.
  customPostConf :: Args -> ConfigFlags ->
        PackageDescription -> LocalBuildInfo -> IO ()
  customPostConf args flags pkg_descr lbi = do
      let verbosity = fromFlag (configVerbosity flags)
      pbi <- getHookedBuildInfo verbosity
      let pkg_descr' = updatePackageDescription pbi pkg_descr
          lbi' = lbi { localPkgDescr = pkg_descr' }
      postConf simpleUserHooks args flags pkg_descr' lbi'

  -- always fetches the .buildinfo file from the current
  -- directory, ".".
  getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
  getHookedBuildInfo verbosity = do
#if MIN_VERSION_Cabal(3,0,1)
                maybe_infoFile <- findHookedPackageDesc verbosity "."
#elif MIN_VERSION_Cabal(2,2,0)
                maybe_infoFile <- findHookedPackageDesc "."
#else
                maybe_infoFile <- defaultHookedPackageDesc
#endif
                case maybe_infoFile of
                  Nothing       -> error "should have .buildinfo file!"
                  Just infoFile -> do
                    warn verbosity $ "Reading parameters from " ++ infoFile
                    readHookedBuildInfo verbosity infoFile


