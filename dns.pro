DNS -> ID FLAGS QC AC NSC ARC QS AS NSS ARS;
ID -> /../ ;
FLAGS -> /../ [qc:=0; ac:=0; nsc:=0; arc:=0];
QC -> EP [qc := (cur_byte() * 256) + cur_byte()];
AC -> EP [ac := (cur_byte() * 256) + cur_byte()];
NSC -> EP [nsc := (cur_byte() * 256) + cur_byte()];
ARC -> EP [arc := (cur_byte() * 256) + cur_byte()];

QS [qc==0] -> ;
QS [qc>0] -> NAME [cnt:=0; qc:=qc - 1] TYPE [cnt:=0] CLASS QS;

AS [ac==0] -> ;
AS [ac>0] -> RR [ac:=ac - 1] AS;

NSS [nsc==0] -> ;
NSS [nsc>0] -> RR [nsc:=nsc - 1] NSS;

ARS [arc==0] -> ;
ARS [arc>0] -> RR [arc:=arc - 1] ARS;

RR -> NAME TYPE [cnt:=0] CLASS [cnt:=0] TTL [cnt:=0] RDLENGTH RDATA AS;
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

#needs to be this form for proper inlining
OCTET -> EP[cnt:=(cnt*256) + cur_byte()] ;
EP    -> ;
