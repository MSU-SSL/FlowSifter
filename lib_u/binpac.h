// $Id: binpac.h,v 1.1.4.2 2006/06/02 15:13:13 rpang Exp $
// Do not edit binpac.h, edit binpac.h.in instead!

#ifndef binpac_h
#define binpac_h

#ifndef WIN32
#include <sys/param.h>
#include <alloca.h>
#include <cstdlib>
#endif


#ifdef HOST_BIGENDIAN
#  define HOST_BYTEORDER	bigendian
#else
#  define HOST_BYTEORDER	littleendian
#endif

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string>
#include <memory>

#define CR '\r'
#define LF '\n'


using namespace std;

typedef char 		int8;
typedef short 		int16;
//typedef long 		int32;
typedef unsigned char 	uint8;
typedef unsigned char byte;
typedef unsigned short 	uint16;
//typedef unsigned long 	uint32;
typedef void		*nullptr;
typedef void		*voidptr;
typedef uint8		*byteptr;

template<class T> class datastring;

template <class T>
class const_datastring
{
public:
	const_datastring()
		: begin_(0), end_(0)
		{ 
		}

	const_datastring(T const *data, int length)
		: begin_(data), end_(data + length)
		{
		}

	const_datastring(const T *begin, const T *end)
		: begin_(begin), end_(end)
		{
		}

	const_datastring(datastring<T> const &s)
		: begin_(s.begin()), end_(s.end())
		{
		}

	void init(const T *data, int length)
		{
		begin_ = data;
		end_ = data + length;
		}

	T const *begin() const	{ return begin_; }
	T const *end() const	{ return end_; }
	int length() const	{ return end_ - begin_; }

	T const &operator[](int index) const
		{
		return begin()[index];
		}

	bool operator==(const_datastring<T> const &s)
		{
		if ( length() != s.length() )
			return false;
		return memcmp((const void *) begin(), (const void *) s.begin(),
			sizeof(T) * length()) == 0;
		}

	void set_begin(T const *begin)	{ begin_ = begin; }
	void set_end(T const *end)	{ end_ = end; }

private:
	T const *begin_; 
	T const *end_;
};

typedef const_datastring<uint8>	const_bytestring;

template<class T>
class datastring
{
public:
	datastring()
		{ 
		clear();
		capacity_ = 1024;
		}

	datastring(T *data, int len)
		{
			clear();
		capacity_ = 1024;
		set_const(data, len);
		}

	datastring(T const * begin, int len)
	{
		clear();
		capacity_ = 1024;
		set_const(begin, len);
	}

	datastring(T const *begin, T const *end)
		{
			clear();
			capacity_ = 1024;
		set_const(begin, end - begin);
		}

	datastring(datastring<T> const &x)
		: data_(x.data()), length_(x.length())
		{
			capacity_ = 1024;
		}

	explicit datastring(const_datastring<T> const &x)
		{
			clear();
			capacity_ = 1024;
		set_const(x.begin(), x.length());
		}

	datastring const &operator=(datastring<T> const &x)
		{
		set(x.data(), x.length());
		return *this;
		}

	void init(T const *begin, int length)
		{
			/*if ( data_ )	//wrong, need to figure out why
			{
				delete [] data_;
				data_ = 0;
			}*/
		//assert(!data_);
			//if (!data_)	{
			//	data_ = new T[capacity_+1];
			//}
		set_const(begin, length);
		}

	void clear()
		{
		data_ = 0; length_ = 0;
		}

	void free()
		{
		if ( data_ )
			delete [] data_;
		clear();
		}

	void clone()
		{
		set_const(begin(), length());
		}

	datastring const &operator=(const_datastring<T> const &x)
		{
		assert(!data_);
		set_const(x.begin(), x.length());
		return *this;
		}

	T const &operator[](int index) const
		{
		return begin()[index];
		}

	T *data() const		{ return data_; }
	int length() const	{ return length_; }
	void set_length(int length)	{ length_ = length;}

	T const *begin() const	{ return data_; }
	T const *end() const	{ return data_ + length_; }

private:
	void set(T *data, int len)
		{
		data_ = data;
		length_ = len;
		}

	void set_const(T const *data, int len)
		{
			if (!data_)	{
				data_ = new T[capacity_+1];
			}

			size_t old_capacity = capacity_;
			while (capacity_ < len)	{
				capacity_*=2;
			}
			if (capacity_ > old_capacity)	{
				delete [] data_;
				data_ = new T[capacity_+1];
			}
		length_ = len;
		memcpy(data_, data, sizeof(T) * len);
		data_[len] = 0;
		}
	T * data_;
	int length_;
	size_t capacity_;
};

typedef datastring<uint8> bytestring;
typedef datastring<uint8> normal_bytestring;

namespace binpac {


const int bigendian = 0;
const int littleendian = 1;
const int unspecified_byteorder = -1;

#ifndef pac_type_defs
#define pac_type_defs

typedef char 		int8;
typedef short 		int16;
typedef long 		int32;
typedef unsigned char 	uint8;
typedef unsigned char byte;
typedef unsigned short 	uint16;
typedef unsigned long 	uint32;
typedef void		*nullptr;
typedef void		*voidptr;
typedef uint8		*byteptr;
typedef const uint8	*const_byteptr;
typedef const char	*const_charptr;

#endif /* pac_type_defs */

/* Handling byte order */

namespace {

inline int16 pac_swap(int16 x)
	{
	return (x >> 8) | ((x & 0xff) << 8);
	}

inline uint16 pac_swap(uint16 x)
	{
	return (x >> 8) | ((x & 0xff) << 8);
	}

inline int32 pac_swap(int32 x)
	{
	return 	(x >> 24) | 
		((x & 0xff0000) >> 8) | 
		((x & 0xff00) << 8) | 
		((x & 0xff) << 24);
	}

inline uint32 pac_swap(uint32 x)
	{
	return 	(x >> 24) | 
		((x & 0xff0000) >> 8) | 
		((x & 0xff00) << 8) | 
		((x & 0xff) << 24);
	}

#define FixByteOrder(byteorder, x)	(byteorder == HOST_BYTEORDER ? (x) : pac_swap(x))

template <class T>
inline T UnMarshall(const u_char *data, int byteorder)
	{
	T result = 0;
	for ( int i = 0; i < (int) sizeof(T); ++i )
		result = ( result << 8 ) | 
			data[byteorder == bigendian ? i : sizeof(T) - 1 - i];
	return result;
	}

inline const char* do_fmt(const char* format, va_list ap)
	{
	static char buf[1024];
	vsnprintf(buf, sizeof(buf), format, ap);
	return buf;
	}

inline string strfmt(const char* format, ...)
	{
	va_list ap;
	va_start(ap, format);
	const char* r = do_fmt(format, ap);
	va_end(ap);
	return string(r);
	}

} // anonymous namespace

#define binpac_fmt(x...) strfmt(x).c_str()

}

#include "binpac_analyzer.h"
//#include "binpac_bytestring.h"
#include "binpac_exception.h"
#include "binpac_regex.h"

#define BINPAC_ASSERT(x) if(!(x)) { throw Exception("BINPAC_ASSERT failed!"); }

namespace binpac {

class RefCount
{
public:
	RefCount() 	{ count = 1; }
	void Ref() 	{ ++count; }
	int Unref() 	{ BINPAC_ASSERT(count > 0); return --count; }

private:
	int count;
};

namespace {
	inline void Unref(RefCount *x)
		{
		if ( x && x->Unref() <= 0 )
			delete x;
		}
}  // anonymous namespace

inline const char *c_str(bytestring const &s)
	{
	return (const char *) s.begin();
	}

inline std::string std_str(const_bytestring const &s) 
	{
	return std::string((const char *) s.begin(), (const char *) s.end());
	}

inline bool operator==(bytestring const &s1, const char *s2)
	{
	return strcmp(c_str(s1), s2) == 0;
	}

inline void get_pointers(const_bytestring const &s, 
		uint8 const **pbegin, uint8 const **pend)
	{
	*pbegin = s.begin();
	*pend = s.end();
	}

inline void get_pointers(bytestring const *s, 
		uint8 const **pbegin, uint8 const **pend)
	{
	*pbegin = s->begin();
	*pend = s->end();
	}

class SimpleFlowBuffer{
public:
	SimpleFlowBuffer(size_t n = 1024);
	virtual ~SimpleFlowBuffer();
	
	void NewData(const_byteptr begin = NULL, const_byteptr end = NULL);
	
	inline bool ready() const{ return data_begin <= orig_end; }
	
	inline const_byteptr begin() const
		{
		return data_begin;
	}
	
	inline const_byteptr end() const
		{
		return current_end;
	}
	
	inline int length()const
	{
		return line_length;
	}
	
	inline bool data_available() const
	  {
		return data_begin < orig_end;
		}
		
	//void NewLine();
	inline int Oneline(){
		int count = 0;

		while (data_begin+count < orig_end && *(data_begin+count)!=LF)	{
			count++;
		}
		while (count > 0 && *(data_begin+count-1) == CR)	{
			count--;
		}
		
		current_end = data_begin+count;
		//line_mode = true;
		
		return count;
	}
	
	inline void TestOneline(){
		if (data_begin < orig_end && *data_begin != CR && *data_begin!=LF)	{
			line_length = 1;;	//only need to indicating that the line is not empty
		}
		else	{
			line_length = 0;
		}
		
		//line_mode = true;
	}
	
	inline bool CleanUpNewLine(){
		bool cleanedup = false;
		while (data_begin < orig_end && *data_begin == CR)	{
				data_begin++;
			}
			if (data_begin < orig_end && *data_begin == LF)	{
				data_begin++;
				cleanedup = true;
			}
			
		//line_mode = false;
		return cleanedup;
	}
	
	inline void EatLine(){
		while (data_begin < orig_end && *data_begin != LF)	{
			data_begin++;
		}
		if (data_begin < orig_end)	{
			data_begin++;
		}
	}

	 void push_back(const byte x)
    {
		int cursize = data_begin - (const_byteptr)_buf;
        if(_size == _capacity)
        {
            byteptr old_buf = _buf;
            
            _capacity *= 2;
            _buf = new byte[_capacity];
            
            for(size_t i = 0; i < _size; i++)
            {
                _buf[i] = old_buf[i];
            }
            
            delete [] old_buf;
        }
        
        _buf[_size] = x;
        _size++;

		data_begin = (const_byteptr)_buf + cursize;
		orig_end = (const_byteptr)(_buf + _size);
    }

	 void push_back(const_byteptr arr, size_t n)
    {
		int cursize = data_begin - (const_byteptr)_buf;
        if(_size + n > _capacity)
        {
            byteptr old_buf = _buf;
            
            while(_capacity < _size + n)
            {
                _capacity *= 2;
            }
            
            _buf = new byte[_capacity];
            
            for(size_t i = 0; i < _size; i++)
            {
                _buf[i] = old_buf[i];
            }
            
            delete [] old_buf;
        }
        
        for(size_t i = 0; i < n; i++, _size++)
        {
            _buf[_size] = arr[i];
        }

		data_begin = (const_byteptr)_buf + cursize;
		orig_end = (const_byteptr)_buf+_size;
    }

	 void pop_front(size_t n = 1)
    {
    	if (n > _size)	{
    		n = _size;
    	}
    	
    	for (int temp = 0; temp < _size-n ; temp++)	{
    		_buf[temp] = _buf[temp+n];
    	}
    	
    	_size -= n;
		data_begin -= n;
		orig_end -= n;
    }
	// A negative frame_length represents a frame till EOF
	//void NewFrame(int frame_length, bool chunked_);
	//void GrowFrame(int new_frame_length);
	
	inline bool have_pending_request() const { return true; }
	
	bool line_mode;
	unsigned char line_length;	//only indicate whether the line is empty
	const_byteptr data_begin;
	const_byteptr orig_end;
	const_byteptr current_end;
	const_byteptr expected_end;
	
	byteptr _buf;
    size_t _capacity;
    size_t _size;
};

typedef SimpleFlowBuffer *simple_flow_buffer_t;

} // namespace binpac

#endif /* binpac_h */
