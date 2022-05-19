
#Fig1a

rm(list=ls())

library("coin")
library("dplyr")


CRC_relative <- read.delim("r/end/tableA.txt",sep = "\t",
                           header = T,row.names = 1)


Group <- read.delim("r/end/groupA.txt",
                    sep = "\t",header = T,row.names = 1)


ss.group <- apply(CRC_relative, 1, FUN=function(x, label){
  rank.x <- rank(x)/length(x)
  ss.tot <- sum((rank.x - mean(rank.x))^2)/length(rank.x)
  ss.o.i <- sum(vapply(unique(label), function(l){
    sum((rank.x[label==l] - mean(rank.x[label==l]))^2)
  }, FUN.VALUE = double(1)))/length(rank.x)
  return(1-ss.o.i/ss.tot)
}, label=Group %>% pull(Group))

ss.study <- apply(CRC_relative, 1, FUN=function(x, label){
  rank.x <- rank(x)/length(x)
  ss.tot <- sum((rank.x - mean(rank.x))^2)/length(rank.x)
  ss.o.i <- sum(vapply(unique(label), function(l){
    sum((rank.x[label==l] - mean(rank.x[label==l]))^2)
  }, FUN.VALUE = double(1)))/length(rank.x)
  return(1-ss.o.i/ss.tot)
}, label=Group %>% pull(Study))

t.mean <- apply(CRC_relative, 1, mean, trim=0.1)

write.table(ss.group,"r/end/ss.groupA.txt",sep = "\t",row.names = T)

write.table(ss.study,"r/end/ss.StudyA.txt",sep = "\t",row.names = T)

write.table(t.mean,"r/end/t.meanA.txt",sep = "\t",row.names = T)



CRC_relative <- as.data.frame(t(CRC_relative))

Group1 = as.factor(Group$Group)

Study<-as.factor(Group$Study)

p.vals1 <- apply(CRC_relative, 2, FUN=function(x){
  c <- wilcox_test(x~Group1|Study,alternative='two.sided')
  w <- pvalue(c)}) 

p.vals.adj <- p.adjust(p.vals1, method='fdr')

write.table(p.vals.adj,"r/end/p.vals.adjA.txt",sep = "\t",row.names = T)

#plot
library("ggplot2")
library("RColorBrewer")
library("rstatix")
library("stringr")

alpha.meta <- 0.05
CRC_plot.all <- read.delim('r/end/A.txt',row.names = 1,header=TRUE,
                           stringsAsFactors = FALSE,check.names = 1)
df.plot.study <- CRC_plot.all %>%
  gather(key=type, value=meta, -species, -Group,
         -t.mean, -adj.p.val, -meta.significance) %>%
  filter(!str_detect(type, '.significance')) %>%
  filter(complete.cases(.)) %>%
  filter(type=='Study')

g2 <- df.plot.study %>%
  ggplot(aes(x=Group, y=meta)) +
  geom_point(aes(size=t.mean, fill=meta.significance), shape=21,
             col=alpha(c('black'), alpha=0.4)) +
  xlab(paste0('Variance explained by Group(LHvsHT)\n','species',' average: ',
              formatC(mean(df.plot.study$Group)*100, digits=2), '%')) +
  ylab(paste0('Variance explained by Study\n','species',' average: ',
              formatC(mean(df.plot.study$meta)*100, digits=2), '%')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(from=0, to=0.1, by=0.05)) +
  scale_y_continuous(breaks=seq(from=0, to=0.6, by=0.1)) +
  scale_fill_manual(values = alpha(c('grey', '#CC071E'),
                                   alpha=c(0.4, .8)),
                    name=paste0('Significance\n(', alpha.meta,')')) +
  scale_size_area(name='Trimmed mean abundance',
                  breaks=c(1e-05, 1e-01, 1e+00)) +
  guides( size = "legend", colour='legend')

g2

ggsave(g2, filename = 'r/end/Fig1a.pdf', 
       width = 6, height = 7)


#Fig1b 


rm(list=ls())

library("coin")
library("dplyr")


CRC_relative <- read.delim("r/end/tableB.txt",sep = "\t",
                           header = T,row.names = 1)


Group <- read.delim("r/end/groupB.txt",
                    sep = "\t",header = T,row.names = 1)


ss.group <- apply(CRC_relative, 1, FUN=function(x, label){
  rank.x <- rank(x)/length(x)
  ss.tot <- sum((rank.x - mean(rank.x))^2)/length(rank.x)
  ss.o.i <- sum(vapply(unique(label), function(l){
    sum((rank.x[label==l] - mean(rank.x[label==l]))^2)
  }, FUN.VALUE = double(1)))/length(rank.x)
  return(1-ss.o.i/ss.tot)
}, label=Group %>% pull(Group))

ss.study <- apply(CRC_relative, 1, FUN=function(x, label){
  rank.x <- rank(x)/length(x)
  ss.tot <- sum((rank.x - mean(rank.x))^2)/length(rank.x)
  ss.o.i <- sum(vapply(unique(label), function(l){
    sum((rank.x[label==l] - mean(rank.x[label==l]))^2)
  }, FUN.VALUE = double(1)))/length(rank.x)
  return(1-ss.o.i/ss.tot)
}, label=Group %>% pull(Study))

t.mean <- apply(CRC_relative, 1, mean, trim=0.1)

write.table(ss.group,"r/end/ss.groupB.txt",sep = "\t",row.names = T)

write.table(ss.study,"r/end/ss.StudyB.txt",sep = "\t",row.names = T)

write.table(t.mean,"r/end/t.meanB.txt",sep = "\t",row.names = T)



CRC_relative <- as.data.frame(t(CRC_relative))

Group1 = as.factor(Group$Group)

Study<-as.factor(Group$Study)

p.vals1 <- apply(CRC_relative, 2, FUN=function(x){
  c <- wilcox_test(x~Group1|Study,alternative='two.sided')
  w <- pvalue(c)}) 

p.vals.adj <- p.adjust(p.vals1, method='fdr')

write.table(p.vals.adj,"r/end/p.vals.adjA.txt",sep = "\t",row.names = T)

#plot
library("ggplot2")
library("RColorBrewer")
library("rstatix")
library("stringr")

alpha.meta <- 0.05
CRC_plot.all <- read.delim('r/end/B.txt',row.names = 1,header=TRUE,
                           stringsAsFactors = FALSE,check.names = 1)
df.plot.study <- CRC_plot.all %>%
  gather(key=type, value=meta, -species, -Group,
         -t.mean, -adj.p.val, -meta.significance) %>%
  filter(!str_detect(type, '.significance')) %>%
  filter(complete.cases(.)) %>%
  filter(type=='Study')

g2 <- df.plot.study %>%
  ggplot(aes(x=Group, y=meta)) +
  geom_point(aes(size=t.mean, fill=meta.significance), shape=21,
             col=alpha(c('black'), alpha=0.4)) +
  xlab(paste0('Variance explained by Group(LHvsHT)\n','species',' average: ',
              formatC(mean(df.plot.study$Group)*100, digits=2), '%')) +
  ylab(paste0('Variance explained by Study\n','species',' average: ',
              formatC(mean(df.plot.study$meta)*100, digits=2), '%')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(from=0, to=0.3, by=0.1)) +
  scale_y_continuous(breaks=seq(from=0, to=0.6, by=0.1)) +
  scale_fill_manual(values = alpha(c('grey', '#CC071E'),
                                   alpha=c(0.4, .8)),
                    name=paste0('Significance\n(', alpha.meta,')')) +
  scale_size_area(name='Trimmed mean abundance',
                  breaks=c(1e-05, 1e-01, 1e+00)) +
  guides( size = "legend", colour='legend')

g2

ggsave(g2, filename = 'r/end/Fig1b.pdf', 
       width = 6, height = 7)


