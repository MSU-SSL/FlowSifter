require(ggplot2)

d <- read.csv("~/bpac/rundata", sep="\t")
d1 <- d[d$parser != "null",]
d1$parser<-factor(d1$parser,levels=c("bpac","sift","upac"),ordered=FALSE)


###MEMORY IMPROVEMENT
rs <- recast(d1[,c("runid", "parser","mem")], runid ~ parser,mean)
rs$bmemrat <- rs$bpac / rs$sift
rs$umemrat <- rs$upac / rs$sift
y <- rs[,c("runid", "bmemrat", "umemrat")]
bs <- melt.data.frame(y)

ggplot() + theme_bw() + xlab(NULL) + ylab("Memory Improvement") +
  scale_y_log10(breaks = c(1,3,10,30,100), labels =
  c("1x","3x","10x","30x","100x")) + scale_x_discrete(breaks =
  c("bmemrat","umemrat"), labels =
  c("BinPAC/\nSifter","UltraPAC/\nSifter")) + geom_boxplot(aes(x =
  variable,y = value),data=bs)

ggsave("memimp.pdf", height=3, width=3.5)
ggsave("memimp.eps", height=3, width=3.5)
ggsave("memimp.png", height=3, width=3.5, dpi=300)

### MEMORY PER FLOW
d1$mpf <- d1$mem / d1$flows

ggplot() +
  scale_y_log10(breaks=c(100,300,1000,3000,10000,30000),labels=c("100B","300B","1KB","3KB","10KB", "30KB")) +
  scale_x_discrete(breaks=c("bpac","sift","upac"), labels=c("BinPAC","Sifter", "UltraPAC")) +
  theme_bw() + xlab(NULL) + ylab("Memory per Flow") +
  geom_jitter(aes(x = parser,y = mpf),data=d1)

ggsave("memper.pdf", height=3, width=3.5)
ggsave("memper.eps", height=3, width=3.5)
ggsave("memper.png", height=3, width=3.5, dpi=300)


### SPEED IMPROVEMENT
rs2 <- recast(d1[,c("runid","parser","gbps")], runid ~ parser, mean)
rs2$bspeedup <- rs2$sift / rs2$bpac
rs2$uspeedup <- rs2$sift / rs2$upac
y2 <- rs2[,c("runid", "bspeedup", "uspeedup")]
bs2 <- melt.data.frame(y2)

ggplot() + theme_bw() + xlab(NULL) + ylab("Speed Improvement") +
  scale_y_continuous(breaks=c(1, 5, 10, 15, 20), labels=c("1x","5x", "10x", "15x", "20x")) + 
  scale_x_discrete(breaks = c("bspeedup","uspeedup"), labels =
  c("Sifter/\nBinPAC","Sifter/\nUltraPAC")) + geom_boxplot(aes(x =
  variable,y = value),data=bs2)

ggsave("bpsimp.pdf", height=3, width=3.5)
ggsave("bpsimp.eps", height=3, width=3.5)
ggsave("bpsimp.png", height=3, width=3.5, dpi=300)

### MEMORY VS. SPEED
ggplot() + xlab("Memory Required(B)") + ylab("Speed (gbps)") +
#	scale_x_continuous(breaks=c(8192, 20480, 40960, 61440, 81920, 102400), labels=c("8K", "20K", "40K", "60K", "80K", "100K")) +
	theme_bw() +
	geom_point(aes(x = mem,y = gbps,shape = parser),data=d1)


ggsave("memspeed.pdf", height=3, width=3.5)
ggsave("memspeed.eps", height=3, width=3.5)
ggsave("memspeed.png", height=3, width=3.5, dpi=300)


### SPEED AT SOAP TRACES

d2 <- read.csv("rectest", sep="\t")
d2$n <- as.numeric(substr(as.character(d2$runid),5,7))
d2$runid<-factor(d2$runid,levels=c("Soap 0","Soap 1","Soap 2","Soap 3","Soap 4","Soap 5","Soap 6","Soap 7","Soap 8","Soap 9",
 "Soap 10","Soap 11","Soap 12","Soap 14","Soap 13","Soap 15","Soap 16"),ordered=TRUE)

d2.sub<-subset(d2,parser == "sift")

ggplot() +
 scale_y_continuous() +
 geom_errorbar(aes(y = gbps,x = n),data=d2.sub,fun.data = mean_cl_normal,conf.int = 0.95,stat = 'summary') +
 geom_bar(aes(y = gbps,x = n),data=d2.sub,alpha = 0.37,fun.data = mean_sdl,mult = 1,stat = 'summary') +
 theme_bw()

ggsave("soapbps.pdf", height=3, width=3.5)
ggsave("soapbps.eps", height=3, width=3.5)
ggsave("soapbps.png", height=3, width=3.5, dpi=300)
