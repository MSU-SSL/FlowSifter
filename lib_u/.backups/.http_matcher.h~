#ifndef HTTP_MATCHER_H
#define HTTP_MATCHER_H

#include "http_pac_fast.h"



int matchHTTPMethod(binpac::FastParser * parser, const char * dataBegin, const char * dataEnd, bool isFinished);
int matchHTTPUri(binpac::FastParser * parser, const char * dataBegin, const char * dataEnd, bool isFinished);
int matchHTTPHeaderName(binpac::FastParser * parser, const char * dataBegin, const char * dataEnd, bool isFinished);
int matchHTTPHeaderValue(binpac::FastParser * parser, const char * dataBegin, const char * dataEnd, bool isFinished);


#endif