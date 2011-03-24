# $Id:$

%extern{
#include <ctype.h>

typedef u_char* byte_vec;
%}

%code{

//special function needed under Windows
int
strncasecmp(const char *s1, const char *s2, size_t n)
{
    while(n > 0
          && toupper((unsigned char)*s1) == toupper((unsigned char)*s2))
    {
        if(*s1 == '\0')
            return 0;
        s1++;
        s2++;
        n--;
    }
    if(n == 0)
        return 0;
    return toupper((unsigned char)*s1) - toupper((unsigned char)*s2);
}

//special function needed under Windows
char *
strcasestr(const char *s, const char *find)
{
        char c, sc;
        size_t len;

        if ((c = *find++) != 0) {
                c = (char)tolower((unsigned char)c);
                len = strlen(find);
                do {
                        do {
                                if ((sc = *s++) == 0)
                                        return (NULL);
                        } while ((char)tolower((unsigned char)sc) != c);
                } while (strncasecmp(s, find, len) != 0);
                s--;
        }
        return ((char *)s);
}

void testoutput(SimpleFlowBuffer* buffer_t, int field_length)
{
	
	for (int i = 0; i<field_length ; i++)	{
		printf("%c", *(buffer_t->data_begin+i));
	}
	
}

int decode_hex(char ch)
{
    if ( ch >= '0' && ch <= '9' )
        return ch - '0';

    if ( ch >= 'A' && ch <= 'F' )
        return ch - 'A' + 10;

    if ( ch >= 'a' && ch <= 'f' )
        return ch - 'a' + 10;

    return -1;
}

int is_reserved_URI_char(unsigned char ch)
{ // see RFC 2396 (definition of URI)
    return strchr(";/?:@&=+$,", ch) != 0;
}

int is_unreserved_URI_char(unsigned char ch)
{ // see RFC 2396 (definition of URI)
    return isalnum(ch) || strchr("-_.!~*\'()", ch) != 0;
}

int bytestring_to_int(const_bytestring const & s, int base)
{
	return strtol((const char*) std_str(s).c_str(), 0, base);
}

int bytestring_to_int(fast_parser_t s, int base)
{
	const_bytestring temp;
	temp.init(s->startptr, s->field_length);
	return strtol((const char*) std_str(temp).c_str(), 0, base);
}

int bytestring_casecmp(const_bytestring const & s1, const_charptr const & s2)
{
	int r = strncasecmp((const char*) s1.begin(), s2, s1.length());
	if ( r == 0 )
		return s2[s1.length()] == '\0' ? 0 : -1;
	else
		return r;
}

// True if s2 is a (case-insensitive) prefix of s1.
bool bytestring_caseprefix(const_bytestring const & s1, const_charptr const & s2)
{
	return strncasecmp((const char*) s1.begin(), s2, strlen(s2)) == 0;
}

double bytestring_to_double(const_bytestring const & s)
{
	return atof((const char*) std_str(s).c_str());
}

/* //newly modified
const_bytestring record_parsed_field(fast_parser_t t_fast_parser)
{
	return const_bytestring(t_fast_parser->flowbuffer->data_begin, t_fast_parser->field_length);
}*/

normal_bytestring record_parsed_field(fast_parser_t t_fast_parser)
{
	t_fast_parser->header_name_field.init(t_fast_parser->flowbuffer->data_begin, t_fast_parser->field_length);
	return t_fast_parser->header_name_field;
}

voidptr match_http_method(fast_parser_t t_fast_parser)
{
	matchHTTPMethod(t_fast_parser, (const char*)(t_fast_parser->flowbuffer->data_begin), (const char*)(t_fast_parser->flowbuffer->data_begin)+t_fast_parser->field_length, t_fast_parser->status == FastParser::NORMAL?true:false);
	return NULL;
}

voidptr match_http_uri(fast_parser_t t_fast_parser)
{
	matchHTTPUri(t_fast_parser, (const char*)(t_fast_parser->flowbuffer->data_begin), (const char*)(t_fast_parser->flowbuffer->data_begin)+t_fast_parser->field_length, t_fast_parser->status == FastParser::NORMAL?true:false);
	return NULL;
}

voidptr match_http_header_name(fast_parser_t t_fast_parser)
{
	int actual_length = t_fast_parser->field_length;
	if (actual_length > 0 && *((const char*)(t_fast_parser->flowbuffer->data_begin)+t_fast_parser->field_length-1) == ':')	{
		actual_length--;
		}
	matchHTTPHeaderName(t_fast_parser, (const char*)(t_fast_parser->flowbuffer->data_begin), (const char*)(t_fast_parser->flowbuffer->data_begin)+actual_length, t_fast_parser->status == FastParser::NORMAL?true:false);
	return NULL;
}

voidptr match_http_header_value(fast_parser_t t_fast_parser)
{
	matchHTTPHeaderValue(t_fast_parser, (const char*)(t_fast_parser->flowbuffer->data_begin), (const char*)(t_fast_parser->flowbuffer->data_begin)+t_fast_parser->field_length, t_fast_parser->status == FastParser::NORMAL?true:false);
	return NULL;
}

bool http_header(fast_parser_t t_fast_parser)
{
		if (t_fast_parser->status != FastParser::NORMAL)	{
			return false;
		}
		
		const_bytestring name(
			t_fast_parser->header_name_field.begin(),
			t_fast_parser->header_name_field.length() > 0 ?
				t_fast_parser->header_name_field.end() - 1 :
				t_fast_parser->header_name_field.end());
				
		const_bytestring value(t_fast_parser->startptr, t_fast_parser->field_length);

		if ( bytestring_casecmp(name, "CONTENT-LENGTH") == 0 )
			{
			t_fast_parser->content_length_ = bytestring_to_int(value, 10);
			t_fast_parser->delivery_mode_ = CONTENT_LENGTH;
			}

		else if ( bytestring_casecmp(name, "TRANSFER-ENCODING") == 0 )
			{
			if ( bytestring_caseprefix(value, "CHUNKED") )
printf("CH"); fflush(stdout);
				t_fast_parser->delivery_mode_ = CHUNKED;
			}

		else if ( bytestring_casecmp(name, "CONTENT-TYPE") == 0 )
			{
			if ( bytestring_caseprefix(value, "MULTIPART") )
				{
				//t_fast_parser->end_of_multipart_.free();	//newly modified
				//t_fast_parser->end_of_multipart_ = extract_boundary(value);
				extract_boundary(t_fast_parser, value);
				if ( t_fast_parser->end_of_multipart_.length() > 0 )
					t_fast_parser->delivery_mode_ = MULTIPART;
				}
			}

		return true;
}

int match_HTTP_token(simple_flow_buffer_t flowbuffer)
{
	//RE/[^()<>@,;:\\"\/\[\]?={} \t]+/
	int length = 0;
	while (flowbuffer->data_begin+length < flowbuffer->orig_end)
	{
		char temp = *(flowbuffer->data_begin+length);
		if (temp!='(' && temp!=')' && temp!='<' && temp!='>' && temp!='@' && temp!=',' && temp!=';' && temp!=':'
				&& temp!='\\' && temp!='\"' && temp!='/' && temp!='[' && temp!=']' && temp!='?' && temp!='='
				&& temp!='{' && temp!='}' && temp!=' ' && temp!='\t')
				{
					length++;
					}
		else	{
			break;
		}
		}
		if (length == 0)	{
			return -1;
		}
		else	{
			return length;
		}		
}

int match_HTTP_HEADER_NAME(simple_flow_buffer_t flowbuffer)
{
	//RE/([^: \t]+:)/
	int length = 0;
	while (flowbuffer->data_begin + length < flowbuffer->orig_end)	{
		char temp = *(flowbuffer->data_begin + length);
		if (temp != ':' && temp != ' ' && temp != '\t')	{
			length++;
			}
		else	{
			break;
		}
	}
	if (length == 0)	{
		return -1;
	}
	
	if (flowbuffer->data_begin + length < flowbuffer->orig_end && *(flowbuffer->data_begin+length) == ':')	{
		return length+1;
	}
	else	{
		return -1;
	}
}

#define INSET(B, x) ((B)[(x) >> 3] & 1 << ((x) & 0x07))

char PROTO_CHAR[128] = 
{
// 0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0A 0x0B 0x0C 0x0D 0x0E 0x0F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1A 0x1B 0x1C 0x1D 0x1E 0x1F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// ' '  '!'  '"'  '#'  '$'  '%'  '&'  '''  '('  ')'  '*'  '+'  ','  '-'  '.'  '/'
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7'  '8'  '9'  ':'  ';'  '<'  '='  '>'  '?'
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G'  'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O'
    0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    
// 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W'  'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   0,
    
// '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g'  'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o'
    0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,

// 'p'  'q'  'r'  's'  't'  'u'  'v'  'w'  'x'  'y'  'z'  '{'  '|'  '}'  '~'  0x7F
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   0
};

char HOST_CHAR[128] = 
{
// 0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0A 0x0B 0x0C 0x0D 0x0E 0x0F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1A 0x1B 0x1C 0x1D 0x1E 0x1F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// ' '  '!'  '"'  '#'  '$'  '%'  '&'  '''  '('  ')'  '*'  '+'  ','  '-'  '.'  '/'
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   1,   1,   0,
    
// '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7'  '8'  '9'  ':'  ';'  '<'  '='  '>'  '?'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   0,
    
// '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G'  'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O'
    0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    
// 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W'  'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   0,
    
// '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g'  'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o'
    0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,

// 'p'  'q'  'r'  's'  't'  'u'  'v'  'w'  'x'  'y'  'z'  '{'  '|'  '}'  '~'  0x7F
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   0
};

char DIR_CHAR[128] = 
{
// 0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0A 0x0B 0x0C 0x0D 0x0E 0x0F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1A 0x1B 0x1C 0x1D 0x1E 0x1F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// ' '  '!'  '"'  '#'  '$'  '%'  '&'  '''  '('  ')'  '*'  '+'  ','  '-'  '.'  '/'
    0,   1,   0,   0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,
    
// '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7'  '8'  '9'  ':'  ';'  '<'  '='  '>'  '?'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   1,   0,   0,
    
// '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G'  'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    
// 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W'  'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   1,
    
// '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g'  'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o'
    0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,

// 'p'  'q'  'r'  's'  't'  'u'  'v'  'w'  'x'  'y'  'z'  '{'  '|'  '}'  '~'  0x7F
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   0
};

char VAR_CHAR[128] = 
{
// 0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0A 0x0B 0x0C 0x0D 0x0E 0x0F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1A 0x1B 0x1C 0x1D 0x1E 0x1F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// ' '  '!'  '"'  '#'  '$'  '%'  '&'  '''  '('  ')'  '*'  '+'  ','  '-'  '.'  '/'
    0,   1,   0,   0,   1,   1,   0,   1,   1,   1,   1,   1,   1,   1,   1,   0,
    
// '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7'  '8'  '9'  ':'  ';'  '<'  '='  '>'  '?'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,
    
// '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G'  'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    
// 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W'  'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   1,
    
// '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g'  'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o'
    0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,

// 'p'  'q'  'r'  's'  't'  'u'  'v'  'w'  'x'  'y'  'z'  '{'  '|'  '}'  '~'  0x7F
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   0
};

char VALUE_CHAR[128] = 
{
// 0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0A 0x0B 0x0C 0x0D 0x0E 0x0F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1A 0x1B 0x1C 0x1D 0x1E 0x1F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// ' '  '!'  '"'  '#'  '$'  '%'  '&'  '''  '('  ')'  '*'  '+'  ','  '-'  '.'  '/'
    0,   1,   0,   0,   1,   1,   0,   1,   1,   1,   1,   1,   1,   1,   1,   0,
    
// '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7'  '8'  '9'  ':'  ';'  '<'  '='  '>'  '?'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   1,   0,   0,
    
// '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G'  'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    
// 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W'  'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   1,
    
// '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g'  'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o'
    0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,

// 'p'  'q'  'r'  's'  't'  'u'  'v'  'w'  'x'  'y'  'z'  '{'  '|'  '}'  '~'  0x7F
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   1,   0,   0,   0
};

char FRAGMENT_CHAR[128] = 
{
// 0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0A 0x0B 0x0C 0x0D 0x0E 0x0F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1A 0x1B 0x1C 0x1D 0x1E 0x1F
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    
// ' '  '!'  '"'  '#'  '$'  '%'  '&'  '''  '('  ')'  '*'  '+'  ','  '-'  '.'  '/'
    0,   1,   0,   0,   1,   0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    
// '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7'  '8'  '9'  ':'  ';'  '<'  '='  '>'  '?'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   1,   0,   1,
    
// '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G'  'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
    
// 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W'  'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_'
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   1,
    
// '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g'  'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o'
    0,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,

// 'p'  'q'  'r'  's'  't'  'u'  'v'  'w'  'x'  'y'  'z'  '{'  '|'  '}'  '~'  0x7F
    1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,   0
};

uint8 PROTO[16];
uint8 HOST[16];
uint8 DIR[16];
uint8 VAR[16];
uint8 VALUE[16];
uint8 FRAGMENT[16];

void init_bitmap(uint8 * bitmap, char * char_set)
{
    for(int i = 0; i < 128; i++)
    {
        if(char_set[i] == 0)
        {
            bitmap[i >> 3] &= ~(1 << (i & 0x07));
        }
        else
        {
            bitmap[i >> 3] |= (1 << (i & 0x07));
        }
    }
}

enum URI_PARSING_STATE
{
    ABS_PATH,
    DIRECTORY,
    EXPECT_QUERY,
    EXPECT_FRAGMENT,
    VAR_NAME,
    VAR_VALUE,
    FINISH,
    BAD,
};

uint32 parse_http_uri(simple_flow_buffer_t flowbuffer)
//uint32 parse_http_uri(uint8 * p_data_begin, uint32 data_length)
{
	uint8* p_data_begin = (uint8*)flowbuffer->data_begin;
	uint32 data_length = flowbuffer->orig_end - flowbuffer->data_begin;
	uint8 * p = p_data_begin;
	
	if(*p == '/')
	{
	    p += 1;
	    goto http_uri_abs_path;
	}
	else
	{
	    while(INSET(PROTO, *p))
	    {
	        p += 1;
        }
        
        //if(strncmpi((char *)p, "://", 3) != 0)
        if(strncasecmp((char *)p, "://", 3) != 0)      
        {
           // PRINTF("HTTP URI error!\n");
        }
        p += 3;
    }
    
    while(INSET(HOST, *p))
	{
	    p += 1;
    }
    
    if(*p == '/')
	{
	    p += 1;
	    goto http_uri_abs_path;
	}

http_uri_abs_path:

    URI_PARSING_STATE state = ABS_PATH;
    
    while(state != FINISH && state != BAD)
    {
        switch(state)
        {
        case ABS_PATH:
            if(*p == '#')
            {
                p += 1;
                state = EXPECT_FRAGMENT;
            }
            else if(*p == '?')
            {
                p += 1;
                state = EXPECT_QUERY;
            }
            else if(*p == ' ' || *p == '\t')
            {
                state = FINISH;
            }
            else if(INSET(DIR, *p))
            {
                state = DIRECTORY;
            }
            else
            {
                state = BAD;
            }
            break;
        case DIRECTORY:
            if(INSET(DIR, *p))
            {
                p += 1;
                state = DIRECTORY;
            }
            else if(*p == '/')
            {
                p += 1;
                state = ABS_PATH;
            }
            else if(*p == '#')
            {
                p += 1;
                state = EXPECT_FRAGMENT;
            }
            else if(*p == '?')
            {
                p += 1;
                state = EXPECT_QUERY;
            }
            else if(*p == ' ' || *p == '\t')
            {
                state = FINISH;
            }
            else
            {
                state = BAD;
            }
            break;
        case EXPECT_QUERY:
            if(INSET(VAR, *p))
            {
                state = VAR_NAME;
            }
            else if(*p == ' ' || *p == '\t')
            {
                state = FINISH;
            }
            else
            {
                state = BAD;
            }
            break;
        case EXPECT_FRAGMENT:
            if(INSET(FRAGMENT, *p))
            {
                p += 1;
                state = EXPECT_FRAGMENT;
            }
            else if(*p == ' ' || *p == '\t')
            {
                state = FINISH;
            }
            else
            {
                state = BAD;
            }
            break;
        case VAR_NAME:
            if(INSET(VAR, *p))
            {
                p += 1;
                state = VAR_NAME;
            }
            else if(*p == '=')
            {
                p += 1;
                state = VAR_VALUE;
            }
            else
            {
                state = BAD;
            }
            break;
        case VAR_VALUE:
            if(INSET(VALUE, *p))
            {
                p += 1;
                state = VAR_VALUE;
            }
            else if(*p == '#')
            {
                p += 1;
                state = EXPECT_FRAGMENT;
            }
            else if(*p == '&')
            {
                p += 1;
                state = EXPECT_QUERY;
            }
            else if(*p == ' ' || *p == '\t')
            {
                state = FINISH;
            }
            else
            {
                state = BAD;
            }
            break;
        case FINISH:
            break;
        default:
            break;
        };
    }

    return p - p_data_begin;    
}
%}

connection HTTP_Conn() {
	upflow = HTTP_Flow(true);
	downflow = HTTP_Flow(false);
};

flow HTTP_Flow(is_orig: bool) {
	flowunit = HTTP_PDU(is_orig) withcontext (connection, this);

	# States.
	%member{
		int content_length_;
		DeliveryMode delivery_mode_;
		bytestring end_of_multipart_;
	%}

	%init{
		content_length_ = 0;
		delivery_mode_ = UNKNOWN_DELIVERY_MODE;
	%}

	%cleanup{
		end_of_multipart_.free();
	%}


	# Methods.


	function is_end_of_multipart(t_flow_buffer: SimpleFlowBuffer): bool
		%{
	const_byteptr t_end_of_data = t_flow_buffer->end();
	const_bytestring line(startptr, t_end_of_data);
	if ( line.length() < 4 + end_of_multipart_.length() )
		return false;
	int len = end_of_multipart_.length();

	// line =?= "--" end_of_multipart_ "--"
	return ( line[0] == '-' && line[1] == '-' &&
	line[len + 2] == '-' && line[len + 3] == '-' &&
	strncmp((const char*) line.begin() + 2,
	(const char*) end_of_multipart_.begin(),len) == 0 );
		%}
};

#commented by HYG
#refine typeattr HTTP_RequestLine += &let {
#	process_request: bool =
#		$context.flow.http_request(method, uri, version);
#};

# refine typeattr HTTP_ReplyLine += &let {
# 	process_reply: bool =
# 		$context.flow.http_reply(version, status.stat_num, reason);
# };

#commented by HYG
#refine typeattr HTTP_Header += &let {
#	process_header: bool =
#		$context.flow.http_header(name, value);
#};

# refine typeattr HTTP_Headers += &let {
# 	process_end_of_headers: bool =
# 		$context.flow.http_end_of_headers(this);
# };

# refine typeattr HTTP_PDU += &let {
# 	process_message: bool =
# 		$context.flow.http_message_done(this);
# };
