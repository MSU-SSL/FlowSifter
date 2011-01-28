#ifndef util_h
#define util_h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#if __STDC__
#define myattribute __attribute__
#else
#define myattribute(x)
#endif

inline int safe_vsnprintf(char* str, size_t size, const char* format, va_list al)
	{
	int result = vsnprintf(str, size, format, al);
	str[size-1] = '\0';
	return result;
	}

// Note: returns a pointer into a shared buffer.
extern const char* fmt(const char* format, ...)
	myattribute((format (printf, 1, 2)));

#endif
