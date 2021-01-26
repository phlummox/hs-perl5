
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

-}

module Language.Perl5.Internal
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Language.Perl5.Types


foreign import ccall "perl5_make_cv"
    perl5_make_cv :: StablePtr Callback -> IO SV
foreign import ccall "perl5_init"
    perl5_init :: CInt -> Ptr CString -> IO Interpreter
foreign import ccall "perl5_sv_undef"
    perl5_sv_undef :: IO SV
foreign import ccall "perl5_sv_yes"
    perl5_sv_yes :: IO SV
foreign import ccall "perl5_sv_no"
    perl5_sv_no :: IO SV
foreign import ccall "perl5_eval"
    perl5_eval :: CString -> CInt -> CInt -> IO (Ptr SV)
foreign import ccall "perl5_newSVpvn"
    perl5_newSVpvn :: CString -> CInt -> IO SV
foreign import ccall "perl5_SvPV"
    perl5_SvPV :: SV -> IO CString
foreign import ccall "perl5_SvIV"
    perl5_SvIV :: SV -> IO CInt
foreign import ccall "perl5_SvNV"
    perl5_SvNV :: SV -> IO CDouble
foreign import ccall "perl5_newSViv"
    perl5_newSViv :: CInt -> IO SV
foreign import ccall "perl5_newSVnv"
    perl5_newSVnv :: CDouble -> IO SV
foreign import ccall "perl_destruct"
    perl_destruct :: Interpreter -> IO CInt
foreign import ccall "perl_free"
    perl_free :: Interpreter -> IO ()
foreign import ccall "perl5_apply"
    perl5_apply :: SV -> SV -> Ptr SV -> CInt -> IO (Ptr SV)
foreign import ccall "perl5_SvTRUE"
    perl5_SvTRUE :: SV -> IO Bool
foreign import ccall "perl5_get_sv"
    perl5_get_sv :: CString -> IO SV
foreign import ccall "perl5_get_cv"
    perl5_get_cv :: CString -> IO SV
