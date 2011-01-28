#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "util.h"

void internal_error(const char* fmt, ...)
	{
	va_list al;

	fprintf(stderr, "internal error: ");
	va_start(al, fmt);
	vfprintf(stderr, fmt, al);
	va_end(al);
	fprintf(stderr, "\n");
#ifdef DEBUG
	abort();
#endif
	exit(1);
	}

const char* fmt(const char* format, ...)
	{
	static char* buf = 0;
	static unsigned int buf_len = 1024;

	if ( ! buf )
		buf = (char*) malloc(buf_len);

	va_list al;
	va_start(al, format);
	int n = safe_vsnprintf(buf, buf_len, format, al);
	va_end(al);

	if ( (unsigned int) n >= buf_len )
		{ // Not enough room, grow the buffer.
		buf_len = n + 32;
		buf = (char*) realloc(buf, buf_len);

		// Is it portable to restart?
		va_start(al, format);
		n = safe_vsnprintf(buf, buf_len, format, al);
		va_end(al);

		if ( (unsigned int) n >= buf_len )
			internal_error("confusion reformatting in fmt()");
		}

	return buf;
	}
