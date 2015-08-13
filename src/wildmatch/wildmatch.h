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
#define GIT_SPACE 0x01
#define GIT_DIGIT 0x02
#define GIT_ALPHA 0x04
#define GIT_GLOB_SPECIAL 0x08
#define GIT_REGEX_SPECIAL 0x10
#define GIT_PATHSPEC_MAGIC 0x20
#define GIT_CNTRL 0x40
#define GIT_PUNCT 0x80

enum {
	S = GIT_SPACE,
	A = GIT_ALPHA,
	D = GIT_DIGIT,
	G = GIT_GLOB_SPECIAL,	/* *, ?, [, \\ */
	R = GIT_REGEX_SPECIAL,	/* $, (, ), +, ., ^, {, | */
	P = GIT_PATHSPEC_MAGIC, /* other non-alnum, except for ] and } */
	X = GIT_CNTRL,
	U = GIT_PUNCT,
	Z = GIT_CNTRL | GIT_SPACE
};

const unsigned char sane_ctype[256] = {
	X, X, X, X, X, X, X, X, X, Z, Z, X, X, Z, X, X,		/*   0.. 15 */
	X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X,		/*  16.. 31 */
	S, P, P, P, R, P, P, P, R, R, G, R, P, P, R, P,		/*  32.. 47 */
	D, D, D, D, D, D, D, D, D, D, P, P, P, P, P, G,		/*  48.. 63 */
	P, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A,		/*  64.. 79 */
	A, A, A, A, A, A, A, A, A, A, A, G, G, U, R, P,		/*  80.. 95 */
	P, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A,		/*  96..111 */
	A, A, A, A, A, A, A, A, A, A, A, R, R, U, P, X,		/* 112..127 */
	/* Nothing in the 128.. range */
};

#define sane_istest(x,mask) ((sane_ctype[(unsigned char)(x)] & (mask)) != 0)
#define is_glob_special(x) sane_istest(x,GIT_GLOB_SPECIAL)

struct wildopts;

int wildmatch(const char *pattern, const char *text,
              unsigned int flags,
              struct wildopts *wo);
#endif
