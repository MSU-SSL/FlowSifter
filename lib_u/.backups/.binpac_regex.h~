#ifndef binpac_regex_h
#define binpac_regex_h

#include "binpac.h"
#include "RE.h"

class RE_Matcher;

namespace binpac
{

class RegExMatcher {
public:
	RegExMatcher(const char *pattern)
		: pattern_(pattern)
		{
		re_matcher_ = 0;
		bitmap = NULL;
		sign = true;
		repeattime = 0;
		hasdelimeter = false;
		}

	~RegExMatcher()
		{
		delete re_matcher_;
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
		for (size_t posstartlist = 1; posstartlist < pattern_[pattern_.length()] ; posstartlist++)	{
			if (pattern_[posstartlist] == '[' && pattern_[posstartlist-1]!='\\')	{
				return false;
			}
		}
		size_t posendlist = pattern_.rfind("]");
		if (posendlist == string::npos || pattern_[posendlist-1] == '\\')	{
			return false;
		}
		for (size_t idx = 1; idx < posendlist ; idx++)	{
			if (pattern_[idx] == ']' && pattern_[idx-1]!='\\')	{
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
		size_t endlist = pattern_.find("]", 1);
		
		for (size_t curidx = startlist ; curidx < endlist ; curidx++)	{
			if (pattern_[curidx+1] == '-')	{
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
						else {
							temp = pattern_[curidx];
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
		if ( ! re_matcher_ )
			{
			re_matcher_ = new RE_Matcher(pattern_.c_str());
			re_matcher_->Compile();
			
			if (ConsiderOptimization() || ConsiderOptimizationWithDelimeter())	{
				if (bitmap)	{
					delete [] bitmap;
				}
				//bitmap = new unsigned char[32];
				bitmap = new unsigned char[256];
				DoOptimization();
			}
			}
		if (!bitmap)	{
			return re_matcher_->MatchPrefix(data, len);
		}
		else	{
			return OptimizeMatch(data, len);
		}
		}

private:
	string pattern_;
	RE_Matcher *re_matcher_;
	unsigned char* bitmap;
	bool sign;
	int repeattime;
	bool hasdelimeter;
	char delimeter;
};

class SigRegExMatcher {
public:
	SigRegExMatcher(const char *pattern, int options)
		: pattern_(pattern), options_(options)
		{
		Sig_RE_Matcher_ = 0;
		}

	~SigRegExMatcher()
		{
		delete Sig_RE_Matcher_;
		}

	// Returns the length of longest match, or -1 on mismatch.
	int Match(const_byteptr data, int len)
		{
		if ( ! Sig_RE_Matcher_ )
			{
			Sig_RE_Matcher_ = new Sig_RE_Matcher(pattern_.c_str(), options_);
			Sig_RE_Matcher_->Compile();
			}
		return Sig_RE_Matcher_->Match(data, len);
		}

private:
	string pattern_;
	int options_;
	Sig_RE_Matcher *Sig_RE_Matcher_;
};

}  // namespace binpac

#endif  // binpac_regex_h
