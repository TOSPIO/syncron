#ifndef WILDMATCH_H
#define WILDMATCH_H

#include <stdlib.h>

#define WM_CASEFOLD 1
#define WM_PATHNAME 2

#define WM_ABORT_MALFORMED 2
#define WM_NOMATCH 1
#define WM_MATCH 0
#define WM_ABORT_ALL -1
#define WM_ABORT_TO_STARSTAR -2

#define sane_istest(x,mask) ((sane_ctype[(unsigned char)(x)] & (mask)) != 0)
#define is_glob_special(x) sane_istest(x,GIT_GLOB_SPECIAL)

struct wildopts;

int wildmatch(const char *pattern, const char *text,
              unsigned int flags,
              struct wildopts *wo);
#endif
