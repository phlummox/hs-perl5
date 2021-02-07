#ifndef HsPerl5DefinedH
#define HsPerl5DefinedH 1

#define dirent DIRENT
#define _INTPTR_T_DEFINED
#define _UINTPTR_T_DEFINED
#undef RETURN

#include "EXTERN.h"
#include "perl.h"
#include "embed.h"


/*
PerlInterpreter * perl5_init ( int argc, char **argv );
bool perl5_SvROK(SV *inv);
bool perl5_can(SV *inv, char *subname);
*/
SV *   hsperl_sv_undef ();
SV *   hsperl_sv_yes ();
SV *   hsperl_sv_no ();
SV **  hsperl_eval(char *code, int len, int cxt);
char * hsperl_SvPV ( SV * sv );
SV *   hsperl_newSVpvn ( char * pv, int len );
SV **  hsperl_return_conv (int count);
SV **  hsperl_apply(SV *sub, SV *inv, SV** args, int cxt);
SV *   hsperl_newSViv ( IV iv );
IV     hsperl_SvIV ( SV * sv );
NV     hsperl_SvNV ( SV * sv );
SV *   hsperl_newSVnv ( NV nv );
bool   hsperl_SvTRUE ( SV * sv );
SV *   hsperl_get_sv ( const char *name );
CV *   hsperl_get_cv ( const char *name );
void   hsperl_SvUTF8_on(SV *sv);
char*  hsperl_sv_2pvutf8(SV* sv, STRLEN* lp);

#include <HsFFI.h>
extern SV ** hsPerlApply ( HsStablePtr *sub, SV **args, int cxt );
SV * hsperl_make_cv ( HsStablePtr *sub );

#endif
