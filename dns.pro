DNS -> ;
DNS -> RR DNS ;

RR -> NAME TYPE[cnt:=0] CLASS[cnt:=0] TTL[cnt:=0] RDLENGTH[cnt:=0] RDATA ;

NAME -> OCTET[cnt:=0] LABEL ;

LABEL [cnt==0] -> ;
LABEL [cnt>0]  -> VSTR NAME ;

VSTR  [cnt==0] -> ;
VSTR  [cnt>0]  -> VSTR[cnt:= skip(cnt)] ;

TYPE -> OCTET OCTET ;

CLASS -> OCTET OCTET ;

TTL -> OCTET OCTET OCTET OCTET ;

RDLENGTH -> OCTET OCTET ;

RDATA -> VSTR ;

OCTET -> EP[cnt:=(cnt*256) + cur_byte()] /./ ;
EP    -> ;
