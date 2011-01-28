# $Id:$

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

#type HTTP_TOKEN	= RE/[^()<>@,;:\\"\/\[\]?={} \t]+/;
type HTTP_TOKEN	= RE/[^]()<>@,;:\\"\/\[?={} \t]+/;	#windows only
#type HTTP_TOKEN	= bytestring &length=match_HTTP_token($input);
type HTTP_WS	= RE/[ \t]*/;
#type HTTP_URI	= RE/[[:alnum:][:punct:]]+/;
#type HTTP_URI	= RE/\\([]a-zA-Z0-9`~!@#$%^&*()_+=\[{}\\\\\|;':\",-.\/<>?]\\|[^\x01-\x7f]\\)+/;	 #windows only
type HTTP_URI	= RE/\\([]a-zA-Z0-9`~!@#$%^&*()_+=\[{}\\\\\|;':\",-.\/<>?]\\|[^\x01-\x7f]\\)+[ \t]*/;	 #windows only

type HTTP_PDU(is_orig: bool) = case is_orig of {
	true ->		request:	HTTP_Request;
	false ->	reply:		HTTP_Reply;
};

type HTTP_Request = record {
	request:	HTTP_RequestLine;
	msg:		HTTP_Message(BODY_MAYBE);
};

function expect_reply_body(reply_status: int): ExpectBody
	%{
	// TODO: check if the request is "HEAD"
	if ( (reply_status >= 100 && reply_status < 200) ||
	     reply_status == 204 || reply_status == 304 )
		return BODY_NOT_EXPECTED;
	return BODY_EXPECTED;
	%}

#function extract_boundary(value: const_bytestring): bytestring
function extract_boundary(t_fast_parser: FastParser, value: const_bytestring): void
	%{
	const char* boundary_prefix = "boundary=";	
	const char* boundary_begin = strcasestr(
					(const char*) value.begin(),
					boundary_prefix);
					

	if ( ! boundary_begin ) {
		//return bytestring();
		t_fast_parser->end_of_multipart_.set_length(0);
		return;
	}

	
	boundary_begin += 9;

	const char* boundary_end = strcasestr(boundary_begin, ";");
	if ( ! boundary_end ){
		boundary_end = (const char*) value.end();
	}
	

	//return bytestring((const uint8*) boundary_begin,
	//			(const uint8*) boundary_end);
	t_fast_parser->end_of_multipart_.init((const uint8*) boundary_begin,
					(const uint8*) boundary_end - (const uint8*) boundary_begin);
	%}
					
type HTTP_Reply = record {
	reply:		HTTP_ReplyLine;
	msg:		HTTP_Message(expect_reply_body(reply.status.stat_num));
};

type HTTP_RequestLine = record {
	method:		HTTP_TOKEN;
	:		HTTP_WS;
	uri:		HTTP_URI;
	#uri: bytestring &length=parse_http_uri($input);
	#:		HTTP_WS;
	version:	HTTP_Version;
} &oneline
&let {
	matchresult: voidptr = match_http_method(method);
	matchresult: voidptr = match_http_uri(uri);
};

type HTTP_ReplyLine = record {
	version:	HTTP_Version;
	:		HTTP_WS;
	status:		HTTP_Status;
	:		HTTP_WS;
	reason:		bytestring &restofdata;
} &oneline;

type HTTP_Status = record {
	stat_str:	RE/[0-9]{3}/;
} &let {
	stat_num: int = bytestring_to_int(stat_str, 10);
};

type HTTP_Version = record {
	:		"HTTP/";
	vers_str:	RE/[0-9]+\.[0-9]+/;
};

type HTTP_Headers = HTTP_Header[] &until($input.length() == 0);

type HTTP_Message(expect_body: ExpectBody) = record {
	headers:	HTTP_Headers;
	body_or_not:	case expect_body of {
		BODY_NOT_EXPECTED	-> none:	empty;
		default			-> body:	HTTP_Body(expect_body);
	};
};

# Multi-line headers are supported by allowing header names to be
# empty.
#
#type HTTP_HEADER_NAME = RE/|([^: \t]+:)/;
type HTTP_HEADER_NAME = RE/([^: \t]+:)/;
#type HTTP_HEADER_NAME = bytestring &length=match_HTTP_HEADER_NAME($input);
type HTTP_Header = record {
	name:		HTTP_HEADER_NAME &transient;
	:		HTTP_WS;
	value:		bytestring &restofdata &transient;
} &oneline
&let {
	#header_name_field: const_bytestring = record_parsed_field(name);
	header_name_field: normal_bytestring = record_parsed_field(name);  #newly modified
	matchresult: voidptr = match_http_header_name(name);
	process_header: bool = http_header(value);
	matchresult: voidptr = match_http_header_value(value);
};

type MIME_Line = record {
	line:	bytestring &restofdata &transient;
} &oneline;

type MIME_Lines = MIME_Line[]
	#&until($context.flow.is_end_of_multipart($input));
	&until($flow.is_end_of_multipart($input));
	
# TODO: parse multipart message according to MIME
type HTTP_Body(expect_body: ExpectBody) =
		#case $context.flow.delivery_mode() of {
		case $flow.delivery_mode_ of {

	CONTENT_LENGTH	-> body: bytestring
				&length = $flow.content_length_,
				&chunked;

	CHUNKED		-> chunks: HTTP_Chunks;

	MULTIPART	-> multipart: MIME_Lines;

	default		-> unknown: HTTP_UnknownBody(expect_body);
};

type HTTP_UnknownBody(expect_body: ExpectBody) = case expect_body of {
	BODY_MAYBE, BODY_NOT_EXPECTED	-> maybenot: empty;
	BODY_EXPECTED			-> rest: bytestring &restofflow &chunked;
};

type HTTP_Chunks = record {
	chunks:		HTTP_Chunk[] &until(chunk_length == 0);
	headers:	HTTP_Headers;
};

type HTTP_Chunk = record {
	length_line:	bytestring &oneline;
	data:		bytestring &length = chunk_length &chunked;
	opt_crlf:	case chunk_length of {
		0	-> none: empty;
		default	-> crlf: bytestring &oneline &check(trailing_crlf == "");
	};
} &let {
	chunk_length: int = bytestring_to_int(length_line, 16);
};
