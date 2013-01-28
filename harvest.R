require(reshape)
require(ggplot2)
require(grid)
require(xtable)

picheight=3.5
picwidth=4.0

#read input
d <- read.csv("harvdata", sep="\t")
#throw away nulls
d1 <- d[d$parser != "null",]
d1 <- na.omit(d1) #remove na values
d1 <- d1[is.finite(d1$gbps),]
#label parser column properly
d1$parser<-factor(d1$parser,levels=c("sift","bpac","upac","siftc"),ordered=FALSE)

### MEMORY vs. SPEED
dhull <- ddply(d1, "parser", function(df) df[chull(df$gbps, df$mem),])

ggplot() + theme_bw() + xlab("\nParsing Speed (gbps)") + ylab("Memory used(B)") +
  scale_y_log10(breaks=c(10000, 100000, 1000000, 10000000, 100000000),
                labels=c("10K", "100K", "1M", "10M", "100M")) +
  geom_point(data=d1, aes(shape=parser, x=gbps, y=mem), size=1.5) +
  theme(legend.position=c(0.8,0.7),
        plot.margin=unit(c(0,.1,0,0),"cm"),
        panel.margin=unit(c(0,0,0,0),"cm")) +
  geom_polygon(data=dhull, aes(group=parser, x=gbps, y=mem), alpha=0.2)


ggsave("harvest.pdf", height=picheight, width=picwidth); ggsave("harvest.eps", height=picheight, width=picwidth); ggsave("harvest.png", height=picheight, width=picwidth, dpi=300)
