# $Id:$

%include binpac-lib.pac


##
## Protocol parser specification
##

%extern{
#include "http-baseconn.h"
%}

analyzer HTTP withcontext {
  connection: HTTP_Conn;
  flow:   HTTP_Flow;
};

enum ExpectBody {
  BODY_EXPECTED,
  BODY_NOT_EXPECTED,
  BODY_MAYBE,
};

enum DeliveryMode {
  UNKNOWN_DELIVERY_MODE,
  CONTENT_LENGTH,
  CHUNKED,
  MULTIPART,
};

##       token          = 1*<any CHAR except CTLs or separators>
##       separators     = "(" | ")" | "<" | ">" | "@"
##                      | "," | ";" | ":" | "\" | <">
##                      | "/" | "[" | "]" | "?" | "="
##                      | "{" | "}" | SP | HT
##      reserved    = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
##                    "$" | ","

type HTTP_TOKEN = RE/[^()<>@,;:\\"\/\[\]?={} \t]+/;
type HTTP_WS  = RE/[ \t]*/;
type HTTP_URI = RE/[[:alnum:][:punct:]]+/;

type HTTP_PDU(is_orig: bool) = case is_orig of {
  true ->   request:  HTTP_Request;
  false ->  reply:    HTTP_Reply;
};

type HTTP_Request = record {
  request:  HTTP_RequestLine;
  msg:    HTTP_Message(BODY_MAYBE);
};

function expect_reply_body(reply_status: int): ExpectBody
  %{
  // TODO: check if the request is "HEAD"
  if ( (reply_status >= 100 && reply_status < 200) ||
       reply_status == 204 || reply_status == 304 )
    return BODY_NOT_EXPECTED;
  return BODY_EXPECTED;
  %}

type HTTP_Reply = record {
  reply:    HTTP_ReplyLine;
  msg:    HTTP_Message(expect_reply_body(reply.status.stat_num));
};

type HTTP_RequestLine = record {
  method:   HTTP_TOKEN;
  :   HTTP_WS;
  uri:    HTTP_URI;
  :   HTTP_WS;
  version:  HTTP_Version;
} &oneline;

type HTTP_ReplyLine = record {
  version:  HTTP_Version;
  :   HTTP_WS;
  status:   HTTP_Status;
  :   HTTP_WS;
  reason:   bytestring &restofdata;
} &oneline;

type HTTP_Status = record {
  stat_str: RE/[0-9]{3}/;
} &let {
  stat_num: int = bytestring_to_int(stat_str, 10);
};

type HTTP_Version = record {
  :   "HTTP/";
  vers_str: RE/[0-9]+\.[0-9]+/;
} &let {
  vers_num: double = bytestring_to_double(vers_str);
};

type HTTP_Headers = HTTP_Header[] &until($input.length() == 0);

type HTTP_Message(expect_body: ExpectBody) = record {
  headers:  HTTP_Headers;
  body_or_not:  case expect_body of {
    BODY_NOT_EXPECTED -> none:  empty;
    default     -> body:  HTTP_Body(expect_body);
  };
};

#
# Multi-line headers are supported by allowing header names to be
# empty.
#
type HTTP_HEADER_NAME = RE/([^: \t]+:)|/;
type HTTP_Header = record {
  name:   HTTP_HEADER_NAME &transient;
  :   HTTP_WS;
  value:    bytestring &restofdata &transient;
} &let {
  process_header: bool = $context.flow.http_header(name, value);
} &oneline;

type MIME_Line = record {
  line: bytestring &restofdata &transient;
} &oneline;

type MIME_Lines = MIME_Line[]
  &until($context.flow.is_end_of_multipart($input));

# TODO: parse multipart message according to MIME
type HTTP_Body(expect_body: ExpectBody) =
    case $context.flow.delivery_mode() of {

  CONTENT_LENGTH  -> body: bytestring
        &length = $context.flow.content_length(),
        &chunked;

  CHUNKED   -> chunks: HTTP_Chunks;

  MULTIPART -> multipart: MIME_Lines;

  default   -> unknown: HTTP_UnknownBody(expect_body);
};

type HTTP_UnknownBody(expect_body: ExpectBody) = case expect_body of {
  BODY_MAYBE, BODY_NOT_EXPECTED -> maybenot: empty;
  BODY_EXPECTED     -> rest: bytestring &restofflow &chunked;
};

type HTTP_Chunks = record {
  chunks:   HTTP_Chunk[] &until($element.chunk_length == 0);
  headers:  HTTP_Headers;
};

type HTTP_Chunk = record {
  length_line:  bytestring &oneline;
  data:   bytestring &length = chunk_length &chunked;
  opt_crlf: case chunk_length of {
    0 -> none: empty;
    default -> crlf: bytestring &oneline &check(trailing_crlf == "");
  };
} &let {
  chunk_length: int = bytestring_to_int(length_line, 16);
};


##
## Connection and flow definitions
##

connection HTTP_Conn(http_conn: BaseConn) {
  upflow = HTTP_Flow(true);
  downflow = HTTP_Flow(false);
};

flow HTTP_Flow(is_orig: bool) {
  flowunit = HTTP_PDU(is_orig) withcontext (connection, this);
  
  %member{
    int content_length_;
    DeliveryMode delivery_mode_;
    bytestring end_of_multipart_;

    double msg_start_time_;
    int msg_begin_seq_;
    int msg_header_end_seq_;

    bool build_headers_;
  %}

  %init{
    content_length_ = 0;
    delivery_mode_ = UNKNOWN_DELIVERY_MODE;

    msg_start_time_ = 0;
    msg_begin_seq_ = 0;
    msg_header_end_seq_ = -1;

  %}

  %cleanup{
    end_of_multipart_.free();
  %}

  function content_length(): int
    %{
    return content_length_;
    %}

  function delivery_mode(): DeliveryMode
    %{
    return delivery_mode_;
    %}

  function end_of_multipart(): const_bytestring
    %{
    return end_of_multipart_;
    %}
  
  function is_end_of_multipart(line: const_bytestring): bool
    %{
    if ( line.length() < 4 + end_of_multipart_.length() )
      return false;

    int len = end_of_multipart_.length();

    // line =?= "--" end_of_multipart_ "--"
    return ( line[0] == '-' && line[1] == '-' &&
       line[len + 2] == '-' && line[len + 3] == '-' &&
       strncmp((const char*) line.begin() + 2,
        (const char*) end_of_multipart_.begin(),
        len) == 0 );
    %}

    function http_header(name_colon: const_bytestring,
                         value: const_bytestring): bool
    %{
    const_bytestring name(
      name_colon.begin(),
      name_colon.length() > 0 ?
         name_colon.end() - 1 :
	 name_colon.end());
//	printf("HH:%s,%s\n", std_str(name_colon).c_str(), std_str(value).c_str());
//	fflush(stdout);
    if ( bytestring_casecmp(name, "CONTENT-LENGTH") == 0 )
    {
	content_length_ = bytestring_to_int(value, 10);
//	printf("CL:%d",content_length_);
//	fflush(stdout);
	delivery_mode_ = CONTENT_LENGTH;
    }
    else if ( bytestring_casecmp(name, "TRANSFER-ENCODING") == 0 )
    {           
//		printf("TE");fflush(stdout);
                      
	if ( bytestring_caseprefix(value, "CHUNKED") ) {
//		printf("CH");fflush(stdout);
		delivery_mode_ = CHUNKED;
        }
    }
    else if ( bytestring_casecmp(name, "CONTENT-TYPE") == 0 )
    {
	if ( bytestring_caseprefix(value, "MULTIPART") )
	{
		end_of_multipart_.free();
		end_of_multipart_ = extract_boundary(value);
		if ( end_of_multipart_.length() > 0 )
			delivery_mode_ = MULTIPART;
	}
    }
    return true;
    %}
    function extract_boundary(value: const_bytestring): bytestring
	%{
	const char* boundary_prefix = "boundary=";
	const char* boundary_begin = strcasestr(
					(const char*) value.begin(),
					boundary_prefix);
		if ( ! boundary_begin )
		return bytestring();
		boundary_begin += 9;
		const char* boundary_end = strcasestr(boundary_begin, ";");
	if ( ! boundary_end )
		boundary_end = (const char*) value.end();
		return bytestring((const uint8*) boundary_begin,
				(const uint8*) boundary_end);
	%}

};


##
## Sample event
##

%code{
int events = 0;
int debug = 0;
%}

function scb_store_method_uri( method: const_bytestring, 
	                       uri: const_bytestring): bool %{
  
if(debug) {	printf("BMethod:%s\nBURL:%s\n", std_str(method).c_str(), std_str(uri).c_str());	fflush(stdout);}
//Count these extractions
	events += 2;

  return true;
%}

function scb_header_name(name: const_bytestring) : voidptr
%{
if(debug) {	printf("BHeaderName:%s\n", std_str(name).c_str());	fflush(stdout);}
	events += 1;
%}

function scb_header_value(value: const_bytestring) : voidptr
%{
if(debug) {	printf("BHeaderValue:%s\n", std_str(value).c_str());	fflush(stdout);}
	events += 1;
%}

refine typeattr HTTP_RequestLine += &let {
        process_request1: bool = scb_store_method_uri(method, uri);
};

refine typeattr HTTP_Header += &let {
       ignore1: voidptr = scb_header_name(name);
       ignore2: voidptr = scb_header_value(value);
};