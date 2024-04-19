
################## Figure 4C and D

##################Figure 4C

rm(list=ls())

library("ggthemes")
library ("ggplot2")
library('RColorBrewer')

ups <- read.delim("Figure 4C.txt",row.names = 1,
                  header = T,sep = "\t")

ups$group1 <- factor(ups$group1, levels=c("4", "9","18","36","imp","p","all"), 
                     ordered=TRUE)

cols = brewer.pal(4, "Set1")

p<-ggplot(data=ups,aes(x=group1,y=value,group=group2)) + 
  geom_line(aes(colour=group2)) +
  geom_point(size=15,aes(shape=group1,colour=group2)) + xlab("")+ylab("Average AUC")+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=15,face = "bold"))+
  scale_shape_manual(values = c(16,16,16,16,17,15,18))+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+ylim(0.2,1)

p

ggsave(p, file='Figure 4C.pdf', width=12, height=10)



##################Figure 4D

rm(list=ls())

library("ggthemes")
library ("ggplot2")
library('RColorBrewer')

ups <- read.delim("Figure 4D.txt",row.names = 1,
                  header = T,sep = "\t")

ups$group1 <- factor(ups$group1, levels=c("3", "7","15","31","imp","p","all"), 
                     ordered=TRUE)

cols = brewer.pal(4, "Set1")

p<-ggplot(data=ups,aes(x=group1,y=value,group=group2)) + 
  geom_line(aes(colour=group2)) +
  geom_point(size=15,aes(shape=group1,colour=group2)) + xlab("")+ylab("Average AUC")+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=15,face = "bold"))+
  scale_shape_manual(values = c(16,16,16,16,17,15,18))+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+ylim(0.85,1)

p

ggsave(p, file='Figure 4D.pdf', width=12, height=10)

