

{-# LANGUAGE CPP #-}

{- |

The contents needed for the "cc-options", "ld-options",
and "extra-libraries" Cabal-file fields are obtained by
calling functions from the Perl @ExtUtils::Embed@ module (see the
documentation for it at the
<https://perldoc.perl.org/ExtUtils::Embed Perldoc website>).

These are output to a .buildinfo file, which cabal picks up and
uses to augment the fields from the .cabal file: see
<https://www.haskell.org/cabal/release/cabal-1.22.2.0/doc/users-guide/developing-packages.html#system-dependent-parameters the cabal docco>.

-}
import Control.Monad

import Distribution.Simple
import Distribution.Verbosity                 (Verbosity)
import Distribution.Simple.Utils              (info)
import Distribution.PackageDescription        ( HookedBuildInfo
                                              ,updatePackageDescription
                                              ,emptyHookedBuildInfo
                                              , PackageDescription(..) )
import Distribution.Simple.LocalBuildInfo     (localPkgDescr
                                              , LocalBuildInfo)
import Distribution.Simple.Setup              (ConfigFlags(..)
                                              , fromFlag
                                              , configVerbosity
                                              )
import Distribution.PackageDescription.Parse  (readHookedBuildInfo)
#ifdef CABAL_PARSEC
import Distribution.PackageDescription.Parsec
#endif

import System.Process (readCreateProcess, shell)
import System.IO

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- if you want to see what's being executed, you can
--   #define VERBOSE_CONFIGURE 1
-- (or give ghc options which will do so for you).


is_verbose_configure :: Bool
is_verbose_configure =
#ifdef VERBOSE_CONFIGURE
  True
#else
  False
#endif



-- |
-- List of
-- @
--    (field-name, shell command to get field body)
-- @
-- to be output to a .buildinfo file
buildInfoFields :: [(String,String)]
buildInfoFields = [
    -- when invoked with no params, "ccopts" and "ldopts" output
    -- results to stdout
    ("cc-options", "perl -MExtUtils::Embed -e ccopts")
    -- ^ typically something like:
    --      -D_REENTRANT -D_GNU_SOURCE -DDEBIAN -fwrapv -fno-strict-aliasing
    --      -pipe -I/usr/local/include -D_LARGEFILE_SOURCE
    --      -D_FILE_OFFSET_BITS=64  -I/usr/lib/x86_64-linux-gnu/perl/5.26/CORE
  , ("ld-options", "perl -MExtUtils::Embed -e ldopts")
    -- ^ typically something like:
    --      -Wl,-E  -fstack-protector-strong -L/usr/local/lib
    --      -L/usr/lib/x86_64-linux-gnu/perl/5.26/CORE -lperl -ldl -lm
    --      -lpthread -lc -lcrypt
  , ("extra-libraries", "perl -MExtUtils::Embed -e 'print(join \" \", (map { $_ =~ s/^-l//; $_; } ( grep /^-l/, (split \" \", ldopts(1)))))'")
    -- ^ just the libraries: e.g.
    --      perl dl m pthread c crypt
  ]

-- | generate conts of .buildinfo file
generate_buildinfo_file_conts :: IO String
generate_buildinfo_file_conts =
  fmap unlines $
    forM buildInfoFields $ \(fld, cmd) -> do
            let cmd' = if is_verbose_configure
                       then "set -x; " ++ cmd
                       else cmd

            out_line <- ((fld ++ ": ") ++) <$> readCreateProcess (shell cmd') ""
            when is_verbose_configure $
              hPutStrLn stderr ("> " ++ out_line)
            return out_line


main :: IO ()
--main = writeBuildInfo >> defaultMainWithHooks defaultUserHooks
--                                                ^ deprecated
main = writeBuildInfo >> defaultMainWithHooks myUserHooks
     where
     writeBuildInfo =
        generate_buildinfo_file_conts >>= writeFile "hs-perl5.buildinfo"

     -- more or less duplicates behaviour of old "defaultUserHooks" -- copied
     -- from Distribution.Simple. Feel free to fix/tidy if you've got the time
     -- or inclination.
     myUserHooks = autoconfUserHooks {
          confHook = confHook autoconfUserHooks,
          postConf = oldCompatPostConf
          }
        where
              oldCompatPostConf :: Args -> ConfigFlags ->
                    PackageDescription -> LocalBuildInfo -> IO ()
              oldCompatPostConf args flags pkg_descr lbi
                  = do let verbosity = fromFlag (configVerbosity flags)
                       pbi <- getHookedBuildInfo verbosity
                       let pkg_descr' = updatePackageDescription pbi pkg_descr
                           lbi' = lbi { localPkgDescr = pkg_descr' }
                       postConf simpleUserHooks args flags pkg_descr' lbi'

              getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
              getHookedBuildInfo verbosity = do
                maybe_infoFile <- defaultHookedPackageDesc
                case maybe_infoFile of
                  Nothing       -> return emptyHookedBuildInfo
                  Just infoFile -> do
                    info verbosity $ "Reading parameters from " ++ infoFile
                    readHookedBuildInfo verbosity infoFile



