#include <stdio.h>
#include <string.h>	// for memcpy

#include "binpac.h"

//#define binpac_regex_h

#pragma comment(lib, "regex.lib")



namespace binpac {


  SimpleFlowBuffer::SimpleFlowBuffer(size_t n)
  {
    _buf = new byte[n];
    _capacity = n;
    _size = 0;
    
    line_mode = false;
    data_begin = (const_byteptr)_buf;
    current_end = NULL;
    orig_end = (const_byteptr)_buf;
  }
  
  SimpleFlowBuffer::~SimpleFlowBuffer()
  {
    if (_buf)	{
      delete [] _buf;
    }
  }
  
  void SimpleFlowBuffer::NewData(const_byteptr begin_, const_byteptr end_)
  {
    push_back(begin_, end_ - begin_);
    
    this->line_length = 1;
    line_mode = false;
  }
	
#if 0
  void SimpleFlowBuffer::NewLine()
  {
    if (current_end && current_end > data_begin)	{
      data_begin = current_end;
    }
		
    if (line_mode)	{
      while (data_begin < orig_end && *data_begin == CR)	{
	data_begin++;
      }
      if (data_begin < orig_end && *data_begin == LF)	{
	data_begin++;
      }
    }
	
    current_end = data_begin;
    /*while (current_end < orig_end && *current_end!=CR && *current_end!=LF)	{
      current_end++;
      }*/
    while (current_end < orig_end && *current_end!=LF)	{
      current_end++;
    }
    while (current_end-1 >= data_begin && *(current_end-1) == CR)	{
      current_end--;
    }
		
    line_mode = true;
    //printf("current start is %u, current end is %u\n", data_begin, current_end);
  }

	
  void SimpleFlowBuffer::NewFrame(int frame_length, bool chunked_)
  {
    if (current_end && current_end > data_begin)	{
      data_begin = current_end;
    }
	
    if (line_mode)	{
      while (data_begin < orig_end && *data_begin == CR)	{
	data_begin++;
      }
      if (data_begin < orig_end && *data_begin == LF)	{
	data_begin++;
      }
    }
    if (frame_length < 0)	{
      current_end = orig_end;
    }
    else	{
      current_end = data_begin;
    }
		
    line_mode = false;
  }

  void SimpleFlowBuffer::GrowFrame(int new_frame_length)
  {
    current_end+=new_frame_length;
  }
#endif
}
