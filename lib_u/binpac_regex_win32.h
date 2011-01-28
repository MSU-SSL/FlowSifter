#ifndef binpac_regex_h
#define binpac_regex_h

#include "binpac_win32.h"
#include "regex.h"
#include "assert.h"


namespace binpac{


class RegExMatcher {
public:
	RegExMatcher(const char *pattern)
		: pattern_(pattern)
		{
		//re_matcher_ = 0;
		regexbuffer = 0;
		bitmap = NULL;
		sign = true;
		repeattime = 0;
		hasdelimeter = false;
		}

	~RegExMatcher()
		{
		delete regexbuffer;
		if (bitmap)	{
			delete [] bitmap;
		}
		}
	bool ConsiderOptimization()	{
		if (pattern_[0] == '(' && pattern_[pattern_.length()-1] == ')')	{
			pattern_ = pattern_.substr(1, pattern_.length()-2);
		}
		if (pattern_.length() < 3)	{
			return false;
		}
		if (pattern_[0] != '[')	{
			return false;
		}
		if (!(pattern_[pattern_.length()-1] == '*' || pattern_[pattern_.length()-1] == '+' || (pattern_[pattern_.length()-1] == '}' && pattern_[pattern_.length()-3] == '{' && pattern_[pattern_.length()-2]>='0' && pattern_[pattern_.length()-2]<='9')))
			{
				return false;
			}
		//for (size_t posstartlist = 1; posstartlist < pattern_[pattern_.length()] ; posstartlist++)	{
		//	if (pattern_[posstartlist] == '[' && pattern_[posstartlist-1]!='\\')	{
		//		return false;
		//	}
		//}
		size_t posendlist = pattern_.rfind("]");
		if (posendlist == std::string::npos || pattern_[posendlist-1] == '\\')	{
			return false;
		}
		for (size_t idx = 1; idx < posendlist ; idx++)	{
			if (pattern_[idx] == ']' && (pattern_[idx-1]!='[' && pattern_[idx-1]!='^'))	{
				return false;
			}
		}
		if (pattern_[posendlist+1]!='*' && pattern_[posendlist+1]!='+' && pattern_[posendlist+1]!='{')	{
			return false;
		}

		return true;
		
	}
	
	void DoOptimization()	{
		if (pattern_[1] == '^')	{
			sign = false;
			//memset(bitmap, 0xff, 32);
			memset(bitmap, 1, 256);
		}
		else	{
			sign = true;
			//memset(bitmap, 0, 32);
			memset(bitmap, 0, 256);
		}
		
		if (pattern_[pattern_.length()-1] == '*')	{
			repeattime = -2;
		}
		else if (pattern_[pattern_.length()-1] == '+')	{
			repeattime = -1;
		}
		else	{
			repeattime = pattern_[pattern_.length()-2] - '0';
		}
		
		size_t startlist;
		if (sign)	{
			startlist = 1;
		}
		else	{
			startlist = 2;
		}
		size_t endlist = pattern_.rfind("]");
		
		for (size_t curidx = startlist ; curidx < endlist ; curidx++)	{
			if (pattern_[curidx+1] == '-' && pattern_[curidx] != '\\')	{
				for (unsigned char temp = pattern_[curidx] ; temp <= pattern_[curidx+2] ; temp++)	{
					if (sign)	{
						//bitmap[temp/8]|=(0x01<<(temp%8));
						bitmap[temp] = 1;
					}
					else	{
						//bitmap[temp/8]&=(~(0x01<<(temp%8)));
						bitmap[temp] = 0;
					}
				}
				curidx+=2;
			}
			else	{
					unsigned char temp = pattern_[curidx] ;
					if (temp == '\\')	{
						curidx++;
						if (pattern_[curidx] == 't')	{
							temp = '\t';
						}
						else if (pattern_[curidx] == 'n')	{
							temp = '\n';
						}
						else if (pattern_[curidx] == 'r')	{
							temp = '\r';
						}
						else if (pattern_[curidx] == '\\')	{
							temp = '\\';
						}
						else if (pattern_[curidx] == '"')	{
							temp = '"';
						}
						else if (pattern_[curidx] == '-')	{
							temp = '-';
						}
						else {
							curidx--;
							//temp = pattern_[curidx];
						}
					}
					if (sign)	{
						//bitmap[temp/8]|=(0x01<<(temp%8));
						bitmap[temp] = 1;
					}
					else	{
						//bitmap[temp/8]&=(~(0x01<<(temp%8)));
						bitmap[temp] = 0;
					}
			}
		}
	}
	
	bool ConsiderOptimizationWithDelimeter()
	{
		if (pattern_.length() < 3)	{
			return false;
		}
		char temp = pattern_[pattern_.length()-1];
		if (pattern_[pattern_.length()-2] == '\\')	{
			if (temp == 'n')	{
				temp = '\n';
			}
			else if (temp == 'r')	{
				temp = '\r';
			}
			else if (temp == '\\')	{
				temp = '\\';
			}
			else if (temp == 't')	{
				temp = '\t';
			}
			pattern_ = pattern_.substr(0, pattern_.length()-2);
		}
		else	{
			pattern_ = pattern_.substr(0, pattern_.length()-1);
		}
		
		hasdelimeter = true;
		delimeter = temp;
		
		return ConsiderOptimization();
	}
	
	int OptimizeMatch(const_byteptr data, int len)
	{
		if (repeattime < 0)	{
			int lengthcount = 0;
			for (; lengthcount < len ; lengthcount++)	{
				//if ((bitmap[(unsigned char)(*(data+lengthcount))/8]&(0x01<<((unsigned char)(*(data+lengthcount))%8)))==0)	{
				if (bitmap[(unsigned char)(*(data+lengthcount))] == 0)	{
					break;
				}	
			}
			if (repeattime == -2 || lengthcount > 0)	{
				if (!hasdelimeter)	{
					return lengthcount;
				}
				else if (*(data+lengthcount) == delimeter)	{
					return lengthcount+1;
				}
				else	{
					return -1;
				}
			}
			else	{
				return -1;
			}
		}
		else	{
			if (len < repeattime)	{
				return -1;
			}
			int lengthcount = 0;
			for (; lengthcount < repeattime ; lengthcount++)	{
				//if ((bitmap[(unsigned char)(*(data+lengthcount))/8]&(0x01<<((unsigned char)(*(data+lengthcount))%8)))==0)	{
				if (bitmap[(unsigned char)(*(data+lengthcount))] == 0)	{
					return -1;
				}	
			}
			if (!hasdelimeter)	{
				return repeattime;
			}
			else if (*(data+lengthcount) == delimeter)	{
				return repeattime+1;
			}
			else	{
				return -1;
			}
		}
	}
	
	// Returns the length of longest match, or -1 on mismatch.
	int MatchPrefix(const_byteptr data, int len)
		{
		if ( ! regexbuffer )
			{
			regexbuffer = new regex_t();
			regexbuffer->syntax |=RE_BACKSLASH_ESCAPE_IN_LISTS;
			regexbuffer->syntax |=RE_CHAR_CLASSES;
			re_compile_pattern(pattern_.c_str(), pattern_.length(), regexbuffer);
			
			if (ConsiderOptimization() || ConsiderOptimizationWithDelimeter())	{
				if (bitmap)	{
					delete [] bitmap;
				}
				//bitmap = new unsigned char[32];
				bitmap = new unsigned char[256];
				DoOptimization();
				//printf("use optimized\n");
			}
			else	{
				//printf("not use optmized\n");
			}
			}
		if (!bitmap)	{
			return re_match(regexbuffer, (const char*)data, len, 0, NULL);
		}
		else	{
			//return re_match(regexbuffer, (const char*)data, len, 0, NULL);
			return OptimizeMatch(data, len);
		}
		}

private:
	std::string pattern_;
	//RE_Matcher *re_matcher_;
	regex_t* regexbuffer;
	unsigned char* bitmap;
	bool sign;
	int repeattime;
	bool hasdelimeter;
	char delimeter;
};
}  // namespace binpac

#endif  // binpac_regex_h
