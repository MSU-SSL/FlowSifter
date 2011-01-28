#ifndef bro_pcre_h
#define bro_pcre_h

#include <stdio.h>
#include <assert.h>
#include <string>

using namespace std;

#include "binpac.h"
// TODO: use configure to figure out the location of pcre.h
#include "pcre.h"

namespace binpac
{

class RE_Matcher
{
public:
   RE_Matcher(const char* pat){
       pattern_ = "^";
       pattern_ += "(";
       pattern_ += pat;
       pattern_ += ")";
       pcre_ = NULL;
       pextra_ = NULL;
   }

   ~RE_Matcher() {
       if (pcre_) {
           pcre_free(pcre_);
       }
   }

   int Compile() {
               const char *err = NULL;
               int erroffset = 0;
       pcre_ = pcre_compile(pattern_.c_str(),
                                    0,  // options,
                                    &err,
                                    &erroffset,
                                    NULL);
       if (pcre_ == NULL) {
           fprintf(stderr,
                   "Error in RE_Matcher::Compile(): %d:%s\n",
                   erroffset, err);
           return 0;
       }
       return 1;
   }

int MatchPrefix (const_byteptr s, int n)
{
       const char *err=NULL;
       assert(pcre_);
       const int MAX_NUM_OFFSETS = 30;
       int offsets[MAX_NUM_OFFSETS];
       int ret = pcre_exec(pcre_,
                                   pextra_,         // pcre_extra
                                   //NULL,          // pcre_extra
                                   (const char*)s,
                                   n,
                                   0,               // offset
                                   0,               // options
                                   offsets,
                                   MAX_NUM_OFFSETS);
       if (ret < 0) {
           return -1;
       }
       assert(offsets[0] == 0);
       return offsets[1];
}

protected:
   pcre *pcre_;
   const pcre_extra *pextra_;
   string pattern_;
};

class Sig_RE_Matcher
{
public:
   Sig_RE_Matcher(const char* pat, int options){
       pattern_ = pat;
       options_ = options;
       pcre_ = NULL;
       pextra_ = NULL;
   }

   ~Sig_RE_Matcher() {
       if (pcre_) {
           pcre_free(pcre_);
       }
   }

   int Compile() {
               const char *err = NULL;
               int erroffset = 0;
       pcre_ = pcre_compile(pattern_.c_str(),
                                    options_,  // options,
                                    &err,
                                    &erroffset,
                                    NULL);
       if (pcre_ == NULL) {
           fprintf(stderr,
                   "Error in Sig_RE_Matcher::Compile(): %d:%s\n",
                   erroffset, err);
           return 0;
       }
       return 1;
   }

int Match(const_byteptr s, int n)
{
       const char *err=NULL;
       assert(pcre_);
       const int MAX_NUM_OFFSETS = 30;
       int offsets[MAX_NUM_OFFSETS];
       int ret = pcre_exec(pcre_,
                                   pextra_,         // pcre_extra
                                   //NULL,          // pcre_extra
                                   (const char*)s,
                                   n,
                                   0,               // offset
                                   0,               // options
                                   offsets,
                                   MAX_NUM_OFFSETS);
       if (ret < 0) {
           return -1;
       }
       return offsets[1];
}

protected:
   pcre *pcre_;
   const pcre_extra *pextra_;
   string pattern_;
   int options_;
};


}  // namespace binpac

#endif
