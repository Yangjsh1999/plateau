#####################################Fig 6 

rm(list=ls())

library("ggthemes")
library ("ggplot2")
library("RColorBrewer")

data <- read.delim("Figure 6.txt",row.names = 1,header = T,sep = "\t")


cols = brewer.pal(4, "Set1")

data$Group <- factor(data$Group, 
                 levels=c("LH", "HH","LT","HT"), 
                 ordered=TRUE)

###############Fig 6A 

p <- ggplot(data,aes(x=Group, y=acetylcoa,fill=Group))+scale_fill_manual(values=cols)+
  geom_boxplot()+geom_jitter(shape=15, position=position_jitter(0.35))+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=7.5,face = "bold",angle = 45))+
  stat_boxplot(geom ='errorbar', width = 0.3)

p

ggsave(p, file='Fig 6A.pdf', width=12, height=10)

###############Fig 6B

p <- ggplot(data,aes(x=Group, y=but,fill=Group))+scale_fill_manual(values=cols)+
  geom_boxplot()+geom_jitter(shape=15, position=position_jitter(0.35))+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=7.5,face = "bold",angle = 45))+
  stat_boxplot(geom ='errorbar', width = 0.3)

p

ggsave(p, file='Fig 6B.pdf', width=12, height=10)

###############Fig 6C

p <- ggplot(data,aes(x=Group, y=buk,fill=Group))+scale_fill_manual(values=cols)+
  geom_boxplot()+geom_jitter(shape=15, position=position_jitter(0.35))+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=7.5,face = "bold",angle = 45))+
  stat_boxplot(geom ='errorbar', width = 0.3)

p

ggsave(p, file='Fig 6C.pdf', width=12, height=10)

###############Fig 6D

p <- ggplot(data,aes(x=Group, y=succinate,fill=Group))+scale_fill_manual(values=cols)+
  geom_boxplot()+geom_jitter(shape=15, position=position_jitter(0.35))+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=7.5,face = "bold",angle = 45))+
  stat_boxplot(geom ='errorbar', width = 0.3)

p

ggsave(p, file='Fig 6D.pdf', width=12, height=10)

###############Fig 6E

p <- ggplot(data,aes(x=Group, y=pdiol,fill=Group))+scale_fill_manual(values=cols)+
  geom_boxplot()+geom_jitter(shape=15, position=position_jitter(0.35))+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=7.5,face = "bold",angle = 45))+
  stat_boxplot(geom ='errorbar', width = 0.3)

p

ggsave(p, file='Fig 6E.pdf', width=12, height=10)

###############Fig 6F

p <- ggplot(data,aes(x=Group, y=propionate.,fill=Group))+scale_fill_manual(values=cols)+
  geom_boxplot()+geom_jitter(shape=15, position=position_jitter(0.35))+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=7.5,face = "bold",angle = 45))+
  stat_boxplot(geom ='errorbar', width = 0.3)

p

ggsave(p, file='Fig 6F.pdf', width=12, height=10)

############################# p-value calculation

rm(list=ls())

library("stats")

########## LH vs HH

data <- read.delim("pA.txt",row.names = 1,header = T,sep = "\t")

Group <- read.delim("GrouppA.txt",row.names = 1,header = T,sep = "\t")

group <- as.factor(Group$Group)

p.vals <- apply(data, 2, FUN=function(x){
  wilcox.test(x~group, exact=FALSE)$p.value})

p.vals.adj <- p.adjust(p.vals, method='fdr')

write.table(p.vals.adj,"p.vals.adjLHvsHH.txt",sep = "\t",row.names = T)

########## LH vs HT

data <- read.delim("pB.txt",row.names = 1,header = T,sep = "\t")

Group <- read.delim("GrouppB.txt",row.names = 1,header = T,sep = "\t")

group <- as.factor(Group$Group)

p.vals <- apply(data, 2, FUN=function(x){
  wilcox.test(x~group, exact=FALSE)$p.value})

p.vals.adj <- p.adjust(p.vals, method='fdr')

write.table(p.vals.adj,"p.vals.adjLHvsHT.txt",sep = "\t",row.names = T)

########## LH vs LT

data <- read.delim("c.txt",row.names = 1,header = T,sep = "\t")

Group <- read.delim("Grouppc.txt",row.names = 1,header = T,sep = "\t")

group <- as.factor(Group$Group)

p.vals <- apply(data, 2, FUN=function(x){
  wilcox.test(x~group, exact=FALSE)$p.value})

p.vals.adj <- p.adjust(p.vals, method='fdr')

write.table(p.vals.adj,"p.vals.adjLHvsLT.txt",sep = "\t",row.names = T)


