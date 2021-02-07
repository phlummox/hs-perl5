#ifndef HS_PERL_GLUE_H
#define HS_PERL_GLUE_H 1

// apparently defined by old ./Configure.PL
// for Windows
//#define dirent DIRENT

// possibly previously used for
// Windows compatibility?
//#define _INTPTR_T_DEFINED
//#define _UINTPTR_T_DEFINED

// almost certainly pointless
//#undef RETURN

#include "EXTERN.h"
#include "perl.h"
#include "embed.h"

// Perl constants

SV *   hsperl_sv_undef ();
SV *   hsperl_sv_yes ();
SV *   hsperl_sv_no ();

// Construct scalar values

char * hsperl_SvPV ( SV * sv );
SV *   hsperl_newSVpvn ( char * pv, int len );
SV *   hsperl_newSViv ( IV iv );
SV *   hsperl_newSVnv ( NV nv );

// Other operations on SVs

IV     hsperl_SvIV ( SV * sv );
NV     hsperl_SvNV ( SV * sv );
bool   hsperl_SvTRUE ( SV * sv );
void   hsperl_SvUTF8_on(SV *sv);
char*  hsperl_sv_2pvutf8(SV* sv, STRLEN* lp);
SV *   hsperl_get_sv ( const char *name );

// Operations on CVs

CV *   hsperl_get_cv ( const char *name );

// Return-value marshalling

SV **  hsperl_return_conv (int count);

// Operations on code/subroutines

SV **  hsperl_eval(char *code, int len, int cxt);
SV **  hsperl_apply(SV *sub, SV *inv, SV** args, int cxt);

// Interpreter initialization
PerlInterpreter * hsperl_init ( int argc, char **argv );

// Use Haskell callbacks in Perl

#include <HsFFI.h>

SV * hsperl_make_cv ( HsStablePtr *sub );

// exported by `src/Language/Perl.hs`:
extern SV ** hsPerlApply ( HsStablePtr *sub, SV **args, int cxt );

#endif
// HS_PERL_GLUE_H

