#HTTP grammar
HTTP -> HTTP_START [bodychunked := 0; bodylength := 0]
     	HEADERS CRLF BODY HTTP;
HTTP -> ;

CRLF -> /\r?\n/;
SP -> /\x20/;
LWS -> /(\r?\n)?[ \t]+/;
CHAR -> /[\x00-\x7f]/; #ascii 0-127
NONWS -> /[^\x00-\x1f\x7f ]+/; # excludes cr, lf, tab, space
TEXT -> /[^\x00-\x1f\x7f]+/;   # excludes cr, lf, tab
QDTEXT -> /[^\x00-\x1f\x7f"]+/;   # excludes cr, lf, tab, quote
TOKEN -> /[^\x00-\x1f()<>@,;:\\"\/[]?={} ]+/; # excludes CTLs and separators
URL -> NONWS;

#HTTP_START -> /GET/ REQUESTPART ;
#HTTP_START -> /POST/ REQUESTPART ;
#HTTP_START -> /PUT/ REQUESTPART ;
#HTTP_START -> /HEAD/ REQUESTPART ;
#HTTP_START -> /OPTIONS/ REQUESTPART ;
#HTTP_START -> /DELETE/ REQUESTPART ;
#HTTP_START -> /TRACE/ REQUESTPART ;
#HTTP_START -> /CONNECT/ REQUESTPART ;

#Response
HTTP_START -> VERSION SP STATUS TAILOP CRLF;
#Request
HTTP_START 10 -> TOKEN SP URL SP VERSION CRLF [httprequest := 1];

#Response part
TAILOP -> SP TEXT;
TAILOP -> ;

# capture the HTTP version string
VERSION -> /HTTP\/1\.0/ [httpversion := 0];
VERSION -> /HTTP\/1\.1/ [httpversion := 1];
VERSION 10 -> /HTTP\/[0-9]+\.[0-9]+/;

# Status code
STATUS -> /\d\d\d/;

#Header Field overall structure (including final CRLF)
HEADERS -> HEADER CRLF HEADERS;
HEADERS -> ;

#Each individual header - special attention to content length & transfer encoding as they determine body format
HEADER -> /(?i:Content-Length):\s*/ [bodylength := getnum()];
HEADER -> /(?i:Transfer-Encoding:\s*chunked)/ [bodychunked := 1];
#HEADER -> /(?i:Connection:\s*Keep-Alive)/ [keepalive := 1];
#HEADER -> /(?i:Connection:\s*close)/ [close := 1];
HEADER 10 -> TOKEN /:/ VALUE;

VALUE -> TEXT VALUE;
VALUE -> LWS VALUE;
VALUE -> ;

#body types
## Content length known
BODY [bodylength  > 0] -> // [bodylength := skip(bodylength)];
BODY [bodylength == 0] -> BODY_NO_LEN;

## Chunked body
BODY_NO_LEN [bodychunked == 1] -> CHUNK_BODY;
BODY_NO_LEN [bodychunked == 0] -> BODY_VERSION;

## HTTP/1.0: skip rest of flow
BODY_VERSION [httpversion == 0] -> // [bodylength := drop_tail()];
## HTTP/1.1, assume bodylength = 0, eat any nulls used as keepalive
BODY_VERSION [httpversion == 1] -> /\x00*/ ;
#BODY_VERSION [httpversion == 1; httprequest == 0] -> // [bodylength := drop_tail()] ;

BODY_XML -> CRLF [bodyend := bodyend + pos()] XML;

CHUNK_BODY -> // [chunksize := gethex()] CHUNK_EXTENSION CRLF [chunksize := skip(chunksize)] CRLF CHUNK_BODY;
CHUNK_BODY 99 -> /0/ CRLF HEADERS CRLF;
CHUNK_BODY 99 -> /0;/ TEXT CRLF HEADERS CRLF;

CHUNK_EXTENSION -> /;/ TEXT;
CHUNK_EXTENSION -> ;

XML -> XVER XENV [bodyend := skip_to(bodyend)];

XVER -> /<\?xml\ version="1\.0"\?>\s*/ ;

XENV -> /<soap:Envelope\ xmlns:soap="http:\/\/www\.w3\.org\/2003\/05\/soap-envelope">/ XHDR XBDY /<\/soap:Envelope>/ ;

XHDR -> /\s*<soap:Header>\s*<\/soap:Header>\s*/ ;

XBDY -> /\s*<soap:Body>\s*/ ARRAY /\s*<\/soap:Body>\s*/;

ARRAY -> /<array>/ ARRAY /<\/array>/ ARRAY ;
ARRAY -> ;

#GETBASETEN -> ;
#GETBASETEN -> /0/ GETBASETEN[bodylength := 0 + bodylength * 10] ;
#GETBASETEN -> /1/ GETBASETEN[bodylength := 1 + bodylength * 10] ;
#GETBASETEN -> /2/ GETBASETEN[bodylength := 2 + bodylength * 10] ;
#GETBASETEN -> /3/ GETBASETEN[bodylength := 3 + bodylength * 10] ;
#GETBASETEN -> /4/ GETBASETEN[bodylength := 4 + bodylength * 10] ;
#GETBASETEN -> /5/ GETBASETEN[bodylength := 5 + bodylength * 10] ;
#GETBASETEN -> /6/ GETBASETEN[bodylength := 6 + bodylength * 10] ;
#GETBASETEN -> /7/ GETBASETEN[bodylength := 7 + bodylength * 10] ;
#GETBASETEN -> /8/ GETBASETEN[bodylength := 8 + bodylength * 10] ;
#GETBASETEN -> /9/ GETBASETEN[bodylength := 9 + bodylength * 10] ;

#GETHEX -> ;
#GETHEX -> /0/ GETHEX[chunksize := 0 + chunksize * 16] ;
#GETHEX -> /1/ GETHEX[chunksize := 1 + chunksize * 16] ;
#GETHEX -> /2/ GETHEX[chunksize := 2 + chunksize * 16] ;
#GETHEX -> /3/ GETHEX[chunksize := 3 + chunksize * 16] ;
#GETHEX -> /4/ GETHEX[chunksize := 4 + chunksize * 16] ;
#GETHEX -> /5/ GETHEX[chunksize := 5 + chunksize * 16] ;
#GETHEX -> /6/ GETHEX[chunksize := 6 + chunksize * 16] ;
#GETHEX -> /7/ GETHEX[chunksize := 7 + chunksize * 16] ;
#GETHEX -> /8/ GETHEX[chunksize := 8 + chunksize * 16] ;
#GETHEX -> /9/ GETHEX[chunksize := 9 + chunksize * 16] ;
#GETHEX -> /A/ GETHEX[chunksize := 10 + chunksize * 16] ;
#GETHEX -> /B/ GETHEX[chunksize := 11 + chunksize * 16] ;
#GETHEX -> /C/ GETHEX[chunksize := 12 + chunksize * 16] ;
#GETHEX -> /D/ GETHEX[chunksize := 13 + chunksize * 16] ;
#GETHEX -> /E/ GETHEX[chunksize := 14 + chunksize * 16] ;
#GETHEX -> /F/ GETHEX[chunksize := 15 + chunksize * 16] ;
#GETHEX -> /a/ GETHEX[chunksize := 10 + chunksize * 16] ;
#GETHEX -> /b/ GETHEX[chunksize := 11 + chunksize * 16] ;
#GETHEX -> /c/ GETHEX[chunksize := 12 + chunksize * 16] ;
#GETHEX -> /d/ GETHEX[chunksize := 13 + chunksize * 16] ;
#GETHEX -> /e/ GETHEX[chunksize := 14 + chunksize * 16] ;
#GETHEX -> /f/ GETHEX[chunksize := 15 + chunksize * 16] ;
