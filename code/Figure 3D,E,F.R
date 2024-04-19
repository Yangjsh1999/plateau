
###############Figure 3D,E,F

##############Figure 3D


rm(list=ls())


library("ggthemes")
library ("ggplot2")
library('RColorBrewer')

ups <- read.delim("Figure 3D.txt",row.names = 1,header = T,sep = "\t")

ups$level <- factor(ups$level, levels=c("RF", "GLM","SVM"), ordered=TRUE)

cols = brewer.pal(6, "Set1")

p<-ggplot(data=ups,aes(x=level,y=value,group=index)) + 
  geom_line(aes(colour=index)) +
  geom_point(size=5,aes(shape=level,colour=index)) + xlab("")+ylab("Odd Ratio")+
  theme_few()+ylim(0.75,1)+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=15,face = "bold"))+
  scale_shape_manual(values = c(16,16,16,16,16,16,16))+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  scale_size_manual(values = c(10,10,10))+
  scale_linetype_manual(values = c("dotdash","dotdash","dotdash"))

p

ggsave(p, file='Figure 3D.pdf', width=12, height=10)

##############Figure 3E


rm(list=ls())


library("ggthemes")
library ("ggplot2")
library('RColorBrewer')

ups <- read.delim("Figure 3E.txt",row.names = 1,header = T,sep = "\t")

ups$level <- factor(ups$level, levels=c("RF", "GLM","SVM"), ordered=TRUE)

cols = brewer.pal(6, "Set1")

p<-ggplot(data=ups,aes(x=level,y=value,group=index)) + 
  geom_line(aes(colour=index)) +
  geom_point(size=5,aes(shape=level,colour=index)) + xlab("")+ylab("Odd Ratio")+
  theme_few()+ylim(0.75,1)+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=15,face = "bold"))+
  scale_shape_manual(values = c(16,16,16,16,16,16,16))+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  scale_size_manual(values = c(10,10,10))+
  scale_linetype_manual(values = c("dotdash","dotdash","dotdash"))

p

ggsave(p, file='Figure 3E.pdf', width=12, height=10)


##############Figure 3F


rm(list=ls())


library("ggthemes")
library ("ggplot2")
library('RColorBrewer')

ups <- read.delim("Figure 3F.txt",row.names = 1,header = T,sep = "\t")

ups$level <- factor(ups$level, levels=c("RF", "GLM","SVM"), ordered=TRUE)

cols = brewer.pal(6, "Set1")

p<-ggplot(data=ups,aes(x=level,y=value,group=index)) + 
  geom_line(aes(colour=index)) +
  geom_point(size=5,aes(shape=level,colour=index)) + xlab("")+ylab("Odd Ratio")+
  theme_few()+ylim(0.75,1)+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=15,face = "bold"))+
  scale_shape_manual(values = c(16,16,16,16,16,16,16))+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  scale_size_manual(values = c(10,10,10))+
  scale_linetype_manual(values = c("dotdash","dotdash","dotdash"))

p

ggsave(p, file='Figure 3F.pdf', width=12, height=10)
