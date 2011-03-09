// This file is automatically generated from http.pac.

#ifndef http_pac_h
#define http_pac_h

#include <vector>

#include "binpac.h"


#include <string.h>

#include "binpac_bytestring.h"


#include "http-baseconn.h"

namespace binpac {

namespace HTTP {
class ContextHTTP;
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
extern RegExMatcher HTTP_TOKEN_re_001;

class HTTP_TOKEN;
extern RegExMatcher HTTP_WS_re_002;

class HTTP_WS;
extern RegExMatcher HTTP_URI_re_003;

class HTTP_URI;
class HTTP_PDU;
class HTTP_Request;
class HTTP_Reply;
class HTTP_RequestLine;
class HTTP_ReplyLine;
extern RegExMatcher HTTP_Status_re_008;

class HTTP_Status;
extern RegExMatcher HTTP_Version_re_010;

class HTTP_Version;
class HTTP_Headers;
class HTTP_Message;
extern RegExMatcher HTTP_HEADER_NAME_re_011;

class HTTP_HEADER_NAME;
class HTTP_Header;
class MIME_Line;
class MIME_Lines;
class HTTP_Body;
class HTTP_UnknownBody;
class HTTP_Chunks;
class HTTP_Chunk;
class HTTP_Conn;
class HTTP_Flow;
} // namespace HTTP

int bytestring_casecmp(const_bytestring const & s1, const_charptr const & s2);
bool bytestring_caseprefix(const_bytestring const & s1, const_charptr const & s2);
int bytestring_to_int(const_bytestring const & s, int base);
double bytestring_to_double(const_bytestring const & s);
namespace HTTP {

class ContextHTTP
{
public:
	ContextHTTP(HTTP_Conn * connection, HTTP_Flow * flow, FlowBuffer * flow_buffer);
	~ContextHTTP();
	
	// Member access functions
	HTTP_Conn * connection() const { return connection_; }
	HTTP_Flow * flow() const { return flow_; }
	FlowBuffer * flow_buffer() const { return flow_buffer_; }
	
protected:
	HTTP_Conn * connection_;
	HTTP_Flow * flow_;
	FlowBuffer * flow_buffer_;
};


class HTTP_PDU
{
public:
	HTTP_PDU(bool is_orig);
	~HTTP_PDU();
	bool ParseBuffer(flow_buffer_t t_flow_buffer, ContextHTTP * t_context);
	
	// Member access functions
	int val_case_index() const	{ return val_case_index_; }
	HTTP_Request * request() const
		{
		switch ( val_case_index() )
			{
			case 1:
				break;  // OK
			default:
				throw ExceptionInvalidCase("http.pac:45:request", val_case_index(), "true");
				break;
			}
		return request_;
		}
	HTTP_Reply * reply() const
		{
		switch ( val_case_index() )
			{
			case 0:
				break;  // OK
			default:
				throw ExceptionInvalidCase("http.pac:46:reply", val_case_index(), "false");
				break;
			}
		return reply_;
		}
	bool is_orig() const { return is_orig_; }
	
protected:
	int val_case_index_;
	HTTP_Request * request_;
	HTTP_Reply * reply_;
	bool is_orig_;
};


class HTTP_Request
{
public:
	HTTP_Request();
	~HTTP_Request();
	bool ParseBuffer(flow_buffer_t t_flow_buffer, ContextHTTP * t_context);
	
	// Member access functions
	HTTP_RequestLine * request() const { return request_; }
	HTTP_Message * msg() const { return msg_; }
	
protected:
	HTTP_RequestLine * request_;
	int buffering_state_;
	HTTP_Message * msg_;
	int parsing_state_;
};

ExpectBody expect_reply_body(int reply_status);

class HTTP_Reply
{
public:
	HTTP_Reply();
	~HTTP_Reply();
	bool ParseBuffer(flow_buffer_t t_flow_buffer, ContextHTTP * t_context);
	
	// Member access functions
	HTTP_ReplyLine * reply() const { return reply_; }
	HTTP_Message * msg() const { return msg_; }
	
protected:
	HTTP_ReplyLine * reply_;
	int buffering_state_;
	HTTP_Message * msg_;
	int parsing_state_;
};


class HTTP_RequestLine
{
public:
	HTTP_RequestLine();
	~HTTP_RequestLine();
	bool ParseBuffer(flow_buffer_t t_flow_buffer);
	
	// Member access functions
	bytestring const & method() const { return method_; }
	bytestring const & anonymous_field_004() const { return anonymous_field_004_; }
	bytestring const & uri() const { return uri_; }
	bytestring const & anonymous_field_005() const { return anonymous_field_005_; }
	HTTP_Version * version() const { return version_; }
	bool process_request() const { return process_request_; }
	
protected:
	bytestring method_;
	bytestring anonymous_field_004_;
	bytestring uri_;
	bytestring anonymous_field_005_;
	HTTP_Version * version_;
	bool process_request_;
	int buffering_state_;
};


class HTTP_ReplyLine
{
public:
	HTTP_ReplyLine();
	~HTTP_ReplyLine();
	bool ParseBuffer(flow_buffer_t t_flow_buffer);
	
	// Member access functions
	HTTP_Version * version() const { return version_; }
	bytestring const & anonymous_field_006() const { return anonymous_field_006_; }
	HTTP_Status * status() const { return status_; }
	bytestring const & anonymous_field_007() const { return anonymous_field_007_; }
	bytestring const & reason() const { return reason_; }
	
protected:
	HTTP_Version * version_;
	bytestring anonymous_field_006_;
	HTTP_Status * status_;
	bytestring anonymous_field_007_;
	bytestring reason_;
	int buffering_state_;
};


class HTTP_Status
{
public:
	HTTP_Status();
	~HTTP_Status();
	int Parse(const_byteptr const t_begin_of_data, const_byteptr const t_end_of_data);
	
	// Member access functions
	bytestring const & stat_str() const { return stat_str_; }
	int stat_num() const { return stat_num_; }
	
protected:
	bytestring stat_str_;
	int stat_num_;
};


class HTTP_Version
{
public:
	HTTP_Version();
	~HTTP_Version();
	int Parse(const_byteptr const t_begin_of_data, const_byteptr const t_end_of_data);
	
	// Member access functions
	bytestring const & anonymous_field_009() const { return anonymous_field_009_; }
	bytestring const & vers_str() const { return vers_str_; }
	double vers_num() const { return vers_num_; }
	
protected:
	bytestring anonymous_field_009_;
	bytestring vers_str_;
	double vers_num_;
};


class HTTP_Headers
{
public:
	HTTP_Headers();
	~HTTP_Headers();
	bool ParseBuffer(flow_buffer_t t_flow_buffer);
	
	// Member access functions
	vector<HTTP_Header *> * val() const { return val_; }
	int size() const	{ return val() ? val()->size() : 0; }
	HTTP_Header * operator[](int index) const { BINPAC_ASSERT(val()); return (*val())[index]; }
	
protected:
	vector<HTTP_Header *> * val_;
	HTTP_Header * val__elem_;
	int buffering_state_;
	int val__arraylength_;
	int val__elem__it_;
};


class HTTP_Message
{
public:
	HTTP_Message(ExpectBody expect_body);
	~HTTP_Message();
	bool ParseBuffer(flow_buffer_t t_flow_buffer, ContextHTTP * t_context);
	
	// Member access functions
	HTTP_Headers * headers() const { return headers_; }
	int body_or_not_case_index() const	{ return body_or_not_case_index_; }
	HTTP_Body * body() const
		{
		return body_;
		}
	ExpectBody expect_body() const { return expect_body_; }
	
protected:
	HTTP_Headers * headers_;
	int body_or_not_case_index_;
	HTTP_Body * body_;
	ExpectBody expect_body_;
	int parsing_state_;
};


class HTTP_Header
{
public:
	HTTP_Header();
	~HTTP_Header();
	bool ParseBuffer(flow_buffer_t t_flow_buffer);
	
	// Member access functions
	bytestring const & anonymous_field_012() const { return anonymous_field_012_; }
	void * ignore1() const { return ignore1_; }
	void * ignore2() const { return ignore2_; }
	
protected:
	const_bytestring name_;
	bytestring anonymous_field_012_;
	const_bytestring value_;
	void * ignore1_;
	void * ignore2_;
	int buffering_state_;
};


class MIME_Line
{
public:
	MIME_Line();
	~MIME_Line();
	bool ParseBuffer(flow_buffer_t t_flow_buffer);
	
	// Member access functions
	
protected:
	const_bytestring line_;
	int buffering_state_;
};


class MIME_Lines
{
public:
	MIME_Lines();
	~MIME_Lines();
	bool ParseBuffer(flow_buffer_t t_flow_buffer, ContextHTTP * t_context);
	
	// Member access functions
	vector<MIME_Line *> * val() const { return val_; }
	int size() const	{ return val() ? val()->size() : 0; }
	MIME_Line * operator[](int index) const { BINPAC_ASSERT(val()); return (*val())[index]; }
	
protected:
	vector<MIME_Line *> * val_;
	MIME_Line * val__elem_;
	int buffering_state_;
	int val__arraylength_;
	int val__elem__it_;
};


class HTTP_Body
{
public:
	HTTP_Body(ExpectBody expect_body);
	~HTTP_Body();
	bool ParseBuffer(flow_buffer_t t_flow_buffer, ContextHTTP * t_context);
	
	// Member access functions
	int val_case_index() const	{ return val_case_index_; }
	HTTP_Chunks * chunks() const
		{
		switch ( val_case_index() )
			{
			case 2:
				break;  // OK
			default:
				throw ExceptionInvalidCase("http.pac:133:chunks", val_case_index(), "CHUNKED");
				break;
			}
		return chunks_;
		}
	MIME_Lines * multipart() const
		{
		switch ( val_case_index() )
			{
			case 3:
				break;  // OK
			default:
				throw ExceptionInvalidCase("http.pac:135:multipart", val_case_index(), "MULTIPART");
				break;
			}
		return multipart_;
		}
	HTTP_UnknownBody * unknown() const
		{
		return unknown_;
		}
	ExpectBody expect_body() const { return expect_body_; }
	
protected:
	int val_case_index_;
	const_bytestring body_;
	int buffering_state_;
	HTTP_Chunks * chunks_;
	MIME_Lines * multipart_;
	HTTP_UnknownBody * unknown_;
	ExpectBody expect_body_;
};


class HTTP_UnknownBody
{
public:
	HTTP_UnknownBody(ExpectBody expect_body);
	~HTTP_UnknownBody();
	bool ParseBuffer(flow_buffer_t t_flow_buffer);
	
	// Member access functions
	int val_case_index() const	{ return val_case_index_; }
	ExpectBody expect_body() const { return expect_body_; }
	
protected:
	int val_case_index_;
	const_bytestring rest_;
	int buffering_state_;
	ExpectBody expect_body_;
};


class HTTP_Chunks
{
public:
	HTTP_Chunks();
	~HTTP_Chunks();
	bool ParseBuffer(flow_buffer_t t_flow_buffer);
	
	// Member access functions
	vector<HTTP_Chunk *> * chunks() const { return chunks_; }
	HTTP_Headers * headers() const { return headers_; }
	
protected:
	vector<HTTP_Chunk *> * chunks_;
	HTTP_Chunk * chunks__elem_;
	int chunks__arraylength_;
	int chunks__elem__it_;
	HTTP_Headers * headers_;
	int parsing_state_;
};


class HTTP_Chunk
{
public:
	HTTP_Chunk();
	~HTTP_Chunk();
	bool ParseBuffer(flow_buffer_t t_flow_buffer);
	
	// Member access functions
	bytestring const & length_line() const { return length_line_; }
	int opt_crlf_case_index() const	{ return opt_crlf_case_index_; }
	bytestring const & crlf() const
		{
		return crlf_;
		}
	int chunk_length() const { return chunk_length_; }
	
protected:
	bytestring length_line_;
	int buffering_state_;
	const_bytestring data_;
	int opt_crlf_case_index_;
	bytestring crlf_;
	int chunk_length_;
	int parsing_state_;
};


class HTTP_Conn : public binpac::ConnectionAnalyzer
{
public:
	HTTP_Conn(BaseConn * http_conn);
	~HTTP_Conn();
	
	// Member access functions
	HTTP_Flow * upflow() const { return upflow_; }
	HTTP_Flow * downflow() const { return downflow_; }
	BaseConn * http_conn() const { return http_conn_; }
	
	void NewData(bool is_orig, const_byteptr begin, const_byteptr end);
	void NewGap(bool is_orig, int gap_length);
	void FlowEOF(bool is_orig);
	
protected:
	HTTP_Flow * upflow_;
	HTTP_Flow * downflow_;
	BaseConn * http_conn_;
};


class HTTP_Flow : public binpac::FlowAnalyzer
{
public:
	HTTP_Flow(HTTP_Conn * connection, bool is_orig);
	~HTTP_Flow();
	
	// Member access functions
	HTTP_Conn * connection() const { return connection_; }
	bool is_orig() const { return is_orig_; }
	
	void NewData(const_byteptr t_begin_of_data, const_byteptr t_end_of_data);
	void NewGap(int gap_length);
	void FlowEOF();
	
	// Functions
	int content_length();
	DeliveryMode delivery_mode();
	const_bytestring end_of_multipart();
	bool is_end_of_multipart(const_bytestring const & line);
	
protected:
	HTTP_PDU * dataunit_;
	ContextHTTP * context_;
	
	// Additional members

    int content_length_;
    DeliveryMode delivery_mode_;
    bytestring end_of_multipart_;

    double msg_start_time_;
    int msg_begin_seq_;
    int msg_header_end_seq_;

    bool build_headers_;
  
	FlowBuffer * flow_buffer_;
	HTTP_Conn * connection_;
	bool is_orig_;
};

bool scb_store_method_uri(const_bytestring const & method, const_bytestring const & uri);
void * scb_header_name(const_bytestring const & name);
void * scb_header_value(const_bytestring const & value);
} // namespace HTTP
}  // namespace binpac
#endif /* http_pac_h */
