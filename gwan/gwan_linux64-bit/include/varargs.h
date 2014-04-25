#if !(__GNUC__ || __ICC)

#ifndef _VARARGS_H
#define _VARARGS_H

// for macros using a variable number of arguments like (see trace.c):
// # define TRACE(fmt,...) printf("%s:%d] "fmt, __func__,__LINE__,__VA_ARGS__)

# define va_dcl
# define va_alist __va_alist
# undef va_start
# define va_start(v,l)	__builtin_va_start(v,l)

#endif // _VARARGS_H

#endif

