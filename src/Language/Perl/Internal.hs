
{- |

Thin wrappers around Perl API functions. Subject to
sudden and violent change.

== return value of perl functions

What is in the returned list of SVs for an "eval_..." function? It consists of one, __or two__,
NULL-terminated lists. Or to be more precise: a pointer to an allocated array
of SVs is returned, and it may contain one or two NULLs.

Apparently, the possibilities are (call the underlying
array of returned SV values "@res@"):

* case 1: @res@ is an array of length 2; res[0] contains an error
  value, and res[1] contains NULL.
* case 2: @res@ is an array of length 3; res[0] and res[1]
  /both/ contain an error value, but only the second is intended
  for use by Haskell code, it seems. Why, I'm not sure. res[2] contains
  NULL.
* case 3: @res@ is an array of some other length. In this case,
  res[0] will be NULL, and the array elements /after/ it
  comprise a NULL-terminated list of return values.

I'm not sure yet why both cases 1 and 2 exist.  The system seems a
little baroque, and I'm inclined towards changing it.
My suggestion is:

* the last two parameters will both take args of type pointer-to-pointer-to-SV,
  and will return (a) a NULL terminated list of errors, and (b) a NULL terminated
  list of return values. The second list will only be non-empty if the /first/
  list (of errors) is empty.

= Methods where caller must free returned memory

* 'hsperl_eval_c' - prefer 'hsperl_eval'
* 'hsperl_apply_c' - prefer 'hsperl_apply'

-}

module Language.Perl.Internal
  where

import Control.Exception

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import Language.Perl.Internal.Types

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

foreign import ccall "hsperl_make_cv"
    hsperl_make_cv :: StablePtr Callback -> IO SV

-- |
-- @
-- hsperl_init argc argv
-- @
--
-- Allocates and initializes an 'Interpreter' instance,
-- and runs it as if called at the command line with @argv@ strings as
-- arguments (minus the program name, naturally)
-- and the original environment.
--
-- As a side effect, initializes global data structures needed by
-- Perl API functions.
--
-- Internally, calls:
--
-- * <https://perldoc.perl.org/perlapi#perl_alloc>,
-- * <https://perldoc.perl.org/perlapi#perl_construct>,
-- * <https://perldoc.perl.org/perlapi#perl_parse>,
-- * <https://perldoc.perl.org/perlapi#perl_run>.
foreign import ccall "hsperl_init"
    hsperl_init :: CInt -> Ptr CString -> IO Interpreter

-- |
-- This is the undef SV. Always refer to this as &PL_sv_undef.
--
-- source: <https://perldoc.perl.org/perlapi#PL_sv_undef>
foreign import ccall "hsperl_sv_undef"
    hsperl_sv_undef :: IO SV

-- | This is the @true@ SV. See \"PL_sv_no". Always refer to this as &PL_sv_yes.
--
-- source: <https://perldoc.perl.org/perlapi#PL_sv_yes>
foreign import ccall "hsperl_sv_yes"
    hsperl_sv_yes :: IO SV

foreign import ccall "hsperl_sv_no"
    hsperl_sv_no :: IO SV

-- | warning: only handles non-null ASCII strings - not Unicode-safe.
--
-- caller should free returned memory.
foreign import ccall "hsperl_eval"
    hsperl_eval_c :: CString -> CInt -> CInt -> IO (Ptr SV)

-- | args: str, len of str, flags
hsperl_eval :: CString -> CInt -> CInt -> (Ptr SV -> IO b) -> IO b
hsperl_eval str len flags between =
    bracket acquire release between
  where
    acquire = hsperl_eval_c str len flags
    release = free

-- |
-- Creates a new SV and copies a string into it. The reference count for the SV
-- is set to 1. Note that if len is zero, Perl will create a zero length
-- string. You are responsible for ensuring that the source string is at least
-- len bytes long. If the s argument is NULL the new SV will be undefined.
--
-- See <https://perldoc.perl.org/perlapi#newSVpvn>
--
-- warning: only handles non-null ASCII strings - not Unicode-safe.
foreign import ccall "hsperl_newSVpvn"
    hsperl_newSVpvn :: CString -> CInt -> IO SV

-- | Returns a pointer to the string in the SV, or a stringified form of the SV
-- if the SV does not contain a string.
--
-- See <https://perldoc.perl.org/perlapi#SvPV_nolen>
foreign import ccall "hsperl_SvPV"
    hsperl_SvPV :: SV -> IO CString


-- |
-- Coerces the given SV to IV and returns it. The returned value in many
-- circumstances will get stored in sv's IV slot, but not in all cases. (Use
-- "sv_setiv" to make sure it does).
--
-- See "SvIVx" for a version which guarantees to evaluate sv only once.
--
-- Source: <https://perldoc.perl.org/perlapi#SvIV>
foreign import ccall "hsperl_SvIV"
    hsperl_SvIV :: SV -> IO CInt



-- |
-- Coerces the given SV to NV and returns it. The returned value in many
-- circumstances will get stored in sv's NV slot, but not in all cases. (Use
-- "sv_setnv" to make sure it does).
--
-- See "SvNVx" for a version which guarantees to evaluate sv only once.
--
-- Source: <https://perldoc.perl.org/perlapi#SvNV>
foreign import ccall "hsperl_SvNV"
    hsperl_SvNV :: SV -> IO CDouble

-- |
-- Creates a new SV and copies an integer into it. The reference count for the
-- SV is set to 1.
--
-- Source: <https://perldoc.perl.org/perlapi#newSViv>
foreign import ccall "hsperl_newSViv"
    hsperl_newSViv :: CInt -> IO SV

-- |
-- Creates a new SV and copies a floating point value into it. The reference
-- count for the SV is set to 1.
--
-- Source: <https://perldoc.perl.org/perlapi#newSVnv>
foreign import ccall "hsperl_newSVnv"
    hsperl_newSVnv :: CDouble -> IO SV

-- | source:
-- <https://perldoc.perl.org/perlapi#perl_destruct>
foreign import ccall "perl_destruct"
    perl_destruct :: Interpreter -> IO CInt

-- |
-- Releases a Perl 'Interpreter'. See
-- <https://perldoc.perl.org/perlembed perlembed>.
--
-- See <https://perldoc.perl.org/perlapi#perl_free>.
foreign import ccall "perl_free"
    perl_free :: Interpreter -> IO ()

-- |
-- @
-- hsperl_apply subroutine receiver args context
-- @
--
-- Apply a subroutine to the specified args.
-- The subroutine can be either a plain subroutine
-- or a method.
--
-- The first parameter, @subroutine@, is either the name of a subroutine (i.e.
-- a string) or a reference to a subroutine.
--
-- The second parameter specifies the method receiver
-- (if a method is being called). If a plain subroutine
-- is being called, NULL should be passed.
-- If a method is being called, this should be either
-- a reference to an object (for a normal method) or
-- a reference to a class (for a static method).
--
-- Then follows a NULL terminated list of args, and the
-- usual calling 'Context', (encoded as an int, with potentially
-- other flags bitwise OR'ed into it).
--
-- Return values are given according to the same protocol as
-- 'hsperl_eval'.
--
-- Caller should free returned memory.
--
-- args: sub, method, args, flags
foreign import ccall "hsperl_apply"
    hsperl_apply_c :: SV -> SV -> Ptr SV -> CInt -> IO (Ptr SV)

hsperl_apply ::
  SV -> SV -> Ptr SV -> CInt -> (Ptr SV -> IO b) -> IO b
hsperl_apply sub receiver args flags between =
    bracket acquire release between
  where
    acquire = hsperl_apply_c sub receiver args flags
    release = free

foreign import ccall "hsperl_SvTRUE"
    hsperl_SvTRUE :: SV -> IO Bool

-- |
-- Returns the SV of the specified Perl scalar. 
-- Flags are passed to @gv_fetchpv@.
--
-- If @GV_ADD@ is set and the Perl variable does not exist then it will be
-- created. If @flags@ is zero and the variable does not exist then NULL is
-- returned.
--
-- NOTE: the perl_ form of this function is deprecated.
--
-- source: <https://perldoc.perl.org/perlapi#get_sv>
foreign import ccall "hsperl_get_sv"
    hsperl_get_sv :: CString -> IO SV

-- |
-- Turn on the UTF-8 status of an 'SV' (the data is not changed, just the
-- flag). Do not use frivolously.
--
-- source: <https://perldoc.perl.org/perlapi#SvUTF8_on>.
foreign import ccall "hsperl_SvUTF8_on"
    hsperl_SvUTF8_on :: SV -> IO ()

-- |
-- @
-- hsperl_sv_2pvutf8 sv lp
-- @
--
-- Return a pointer to the UTF-8-encoded representation of the SV, and set
-- @*lp@ to its length. May cause the SV to be upgraded to UTF-8 as a
-- side-effect.
--
-- source: <https://perldoc.perl.org/perlapi#sv_2pvutf8>
foreign import ccall "hsperl_sv_2pvutf8"
    hsperl_sv_2pvutf8 :: SV -> Ptr CSize -> IO (Ptr CChar)

-- |
-- @hsperl_get_cv name flags@: Uses strlen to get the length of name, then calls
-- get_cvn_flags.
--
-- NOTE: the perl_ form of this function is deprecated.
--
-- See <https://perldoc.perl.org/perlapi#get_cv>
foreign import ccall "hsperl_get_cv"
    hsperl_get_cv :: CString -> IO SV

-- | If it's desired Perl be extra-hygienic about cleaning up resources,
-- this should be called (a) after allocation, but before initialization, and (b) before
-- destruction. Currently, our initialization code already includes a call to it.
-- (Multiple calls are harmless, though.)
foreign import ccall "hsperl_set_destruct_level"
    hsperl_set_destruct_level :: IO ()


