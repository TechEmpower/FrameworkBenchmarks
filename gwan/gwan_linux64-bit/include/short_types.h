// ============================================================================
// C servlet header for the G-WAN Web Application Server (http://trustleap.ch/)
// ----------------------------------------------------------------------------
// short_types.h: short default types
// ============================================================================

#ifndef _SHORT_TYPES_H
#define _SHORT_TYPES_H

typedef   signed char       s8;
typedef unsigned char       u8;
typedef   signed short int s16;
typedef unsigned short int u16;
typedef   signed int       s32;
typedef unsigned int       u32;

typedef unsigned long      ulong;

# ifdef _WIN32
typedef          __int64   s64;
typedef unsigned __int64   u64;
# else
#  if __WORDSIZE == 64
typedef          long int s64;
typedef long unsigned int u64;
#  else
typedef          long long int s64;
typedef long long unsigned int u64;
#  endif
# endif

#endif // _SHORT_TYPES_H

// ============================================================================
// End of Source Code
// ============================================================================

