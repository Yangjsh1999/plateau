

##########################Fig2A Richness index and evenness index scatter plot

#################Figure 2A obs and evenness scatter plots

rm(list=ls())

library(vegan)
library(ape)
library (ggplot2)  
library(RColorBrewer)

Alpha <- read.delim("Figure 2A.txt",sep = '\t',header = T,row.names = 1)

centroid <- aggregate(cbind(observed,evenness) ~ Group, data = Alpha, FUN = mean)

Alpha2 <- dplyr::left_join(Alpha, centroid, by = "Group", suffix = c("",".cen"))

centroid$Group <- factor(centroid$Group, 
                         levels=c("LH", "HH","LT","HT"), 
                         ordered=TRUE)

show_col(brewer.pal(4, "Set1"))
cols = brewer.pal(4, "Set1")
cols

Alpha2$Group <- factor(Alpha2$Group, 
                       levels=c("LH", "HH","LT","HT"), 
                       ordered=TRUE)

Alpha$Group <- factor(Alpha$Group, 
                      levels=c("LH", "HH","LT","HT"), 
                      ordered=TRUE)


ggtheme = theme(legend.title = element_blank(),
                axis.text.y = element_text(colour="black", size=14), 
                axis.text.x = element_text(colour="black", size=14), 
                axis.title = element_text(colour="black", size=14), 
                legend.position = "right",legend.text = element_text(size=15), 
                legend.key = element_rect(fill = NA), 
                legend.key.size = unit(0.7, "cm"), 
                legend.box.spacing = unit(0, "cm"),
                panel.border = element_rect(colour="black", fill=NA, size=1), 
                panel.background = element_blank())# title centeredplot.title = element_text(hjust=0.5, size=25) )

p <-  ggplot(Alpha2,aes(x = observed,y = evenness))+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  geom_point(aes(color=Group))+
  geom_segment(aes(xend=observed.cen,yend=evenness.cen,color=Group),
               show.legend = F,alpha = 0.5)+
  ggtheme+
  scale_colour_manual(values = cols)+
  geom_label(data = centroid, 
             aes(label = Group, fill = Group), size = 5, 
             show.legend = FALSE,
             color="white",alpha = 0.8)+
  scale_fill_manual(values = cols)+
  theme(legend.position = "top")+xlim(0,650)+ylim(0.25,1)
p

ggsave(p, file='Figure 2A-1.pdf', width=8, height=6)


########### evenness box plots


rm(list=ls())

library("ggthemes")
library ("ggplot2")
library("RColorBrewer")


Alpha <- read.delim("Figure 2A.txt",row.names = 1,header = T,sep = "\t")

Alpha$Group <- factor(Alpha$Group, 
                      levels=c("LH", "HH","LT","HT"), 
                      ordered=TRUE)

show_col(brewer.pal(4, "Set1"))
cols = brewer.pal(4, "Set1")
cols



p <- ggplot(Alpha,aes(x=Group, y=evenness,fill=Group))+scale_fill_manual(values=cols)+
  geom_boxplot()+geom_jitter(shape=15, position=position_jitter(0.35),size = 1)+
  theme_few()+ylim(0.25,1)+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=7.5,face = "bold",angle = 45))+ylim(0.25,1)


p

ggsave(p, file='Figure 2A-2.pdf', width=6, height=8)

################### observed box plots

rm(list=ls())

library("ggthemes")
library ("ggplot2")
library("RColorBrewer")


Alpha <- read.delim("Figure 2A.txt",row.names = 1,header = T,sep = "\t")

Alpha$Group <- factor(Alpha$Group, 
                      levels=c("LH", "HH","LT","HT"), 
                      ordered=TRUE)

show_col(brewer.pal(4, "Set1"))
cols = brewer.pal(4, "Set1")
cols



p <- ggplot(Alpha,aes(x=Group, y=observed,fill=Group))+scale_fill_manual(values=cols)+
  geom_boxplot()+geom_jitter(shape=15, position=position_jitter(0.35),size = 1)+
  theme_few()+theme(
    axis.text.y=element_text(vjust=1,size=15,face = "bold"))+theme(
      axis.text.x=element_text(vjust=1,size=7.5,face = "bold",angle = 45))+ylim(0,650)


p

ggsave(p, file='Figure 2A-3.pdf', width=6, height=8)



##############Alpha wilcox LH vs HH

rm(list=ls())

library("coin")

Alpha <- read.delim("AlphaA.txt",sep = '\t',header = T,row.names = 1)

Group <- read.delim("GroupA.txt",sep = '\t',header = T,row.names = 1)

Group <- as.factor(Group$Group)

p.vals <- apply(Alpha, 2, FUN=function(x){
  c <- wilcox_test(x~Group,alternative='two.sided')
  w <- pvalue(c)}) 

write.table(p.vals,"p.valA.txt",sep = "\t")


#############Alpha wilcox block LH vs HH

rm(list=ls())

library("coin")

Alpha <- read.delim("AlphaA.txt",sep = '\t',header = T,row.names = 1)

Group1 <- read.delim("GroupA.txt",sep = '\t',header = T,row.names = 1)

Group <- as.factor(Group1$Group)

Study <- as.factor(Group1$Study)

p.vals <- apply(Alpha, 2, FUN=function(x){
  c <- wilcox_test(x~Group|Study,alternative='two.sided')
  w <- pvalue(c)}) 

write.table(p.vals,"block_p.valA.txt",sep = "\t")

#############Alpha wilcox LH vs HT

rm(list=ls())

library("coin")

Alpha <- read.delim("AlphaB.txt",sep = '\t',header = T,row.names = 1)

Group <- read.delim("GroupB.txt",sep = '\t',header = T,row.names = 1)

Group <- as.factor(Group$Group)

p.vals <- apply(Alpha, 2, FUN=function(x){
  c <- wilcox_test(x~Group,alternative='two.sided')
  w <- pvalue(c)}) 

write.table(p.vals,"p.valB.txt",sep = "\t")

##################Alpha wilcox block LH vs HT

rm(list=ls())

library("coin")

Alpha <- read.delim("AlphaB.txt",sep = '\t',header = T,row.names = 1)

Group1 <- read.delim("GroupB.txt",sep = '\t',header = T,row.names = 1)

Group <- as.factor(Group1$Group)

Study <- as.factor(Group1$Study)

p.vals <- apply(Alpha, 2, FUN=function(x){
  c <- wilcox_test(x~Group|Study,alternative='two.sided')
  w <- pvalue(c)}) 

write.table(p.vals,"block_p.valB.txt",sep = "\t")


#######################Alpha wilcox LH vs LT

rm(list=ls())

library("coin")

Alpha <- read.delim("AlphaC.txt",sep = '\t',header = T,row.names = 1)

Group <- read.delim("GroupC.txt",sep = '\t',header = T,row.names = 1)

Group <- as.factor(Group$Group)

p.vals <- apply(Alpha, 2, FUN=function(x){
  c <- wilcox_test(x~Group,alternative='two.sided')
  w <- pvalue(c)}) 

write.table(p.vals,"p.valC.txt",sep = "\t")

#####################Alpha wilcox block LH vs LT

rm(list=ls())

library("coin")

Alpha <- read.delim("AlphaC.txt",sep = '\t',header = T,row.names = 1)

Group1 <- read.delim("GroupC.txt",sep = '\t',header = T,row.names = 1)

Group <- as.factor(Group1$Group)

Study <- as.factor(Group1$Study)

p.vals <- apply(Alpha, 2, FUN=function(x){
  c <- wilcox_test(x~Group|Study,alternative='two.sided')
  w <- pvalue(c)}) 

write.table(p.vals,"block_p.valC.txt",sep = "\t")


