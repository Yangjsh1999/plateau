

###########Figure 2B beta diversity PCoA 

rm(list=ls())

library("labdsv")
library("coin")
library("vegan")
library("yaml")
library("ggpubr")
library("cowplot")
library("tidyverse")
library("RColorBrewer")
library ("ggplot2")
library('vegan')
library('ape')

##########import data
df.plot <- read.delim(file = 'PCoAB.txt',stringsAsFactors = FALSE, header = TRUE, row.names = 1,
                      check.name = FALSE)

group<-read.delim(file="Group.txt",header=T,sep="\t",row.names=1)

df.plot$Group <- factor(df.plot$Group , 
                        levels=c("LA", "HH",
                                 "LT","HT"), 
                        ordered=TRUE)


############main plot
g.main <- df.plot %>% 
  ggplot(aes(x=Axis1, y=Axis2, shape = Group ,col= Study)) +
  geom_point(size = 3)  +
  
  scale_colour_manual(values=c('Study1' = '#085F63', 'Study2' = '#49BEB7',
                               'Study3' = '#FF5959', 'Study4' = '#FACF5A', 'Study5' =
                                 '#c71585', 'Study6'='#01aaed'),guide = FALSE) +
  scale_shape_manual(values=c(15, 16, 17, 18),guide=FALSE) +
  scale_x_continuous(position='top') +
  theme(panel.background = element_rect(fill='white', color = 'black'),
        axis.ticks=element_blank(), axis.text = element_blank(),axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid = element_blank())

study <- as.factor(group$Study)

###############study boxplot axis 1

df.plot$Study <- factor(df.plot$Study , 
                        levels=c("Study1", "Study2",
                                 "Study3","Study4","Study5","Study6"), 
                        ordered=TRUE)



g.s.1 <- df.plot %>% 
  mutate(Study=factor(Study, levels=names(c('Study1' = '#085F63', 'Study2' = '#49BEB7',
                                            'Study3' = '#FF5959', 'Study4' = '#FACF5A', 'Study5' =
                                              '#c71585', 'Study6'='#01aaed')))) %>% 
  ggplot(aes(y=Axis1, x=Study, fill=Study)) +
  xlab(paste0('Study\n','')) +
  geom_boxplot() +
  scale_fill_manual(values=c('Study1' = '#085F63', 'Study2' = '#49BEB7',
                             'Study3' = '#FF5959', 'Study4' = '#FACF5A', 'Study5' =
                               '#c71585', 'Study6'='#01aaed'), guide=FALSE) +
  theme(axis.ticks = element_blank(),
        panel.background = element_rect(fill='white', color = 'black'),
        axis.text = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y  = element_text(size = 5),
        panel.grid = element_blank()) + 
  coord_flip()


################study boxplot axis 2



g.s.2 <- df.plot %>% 
  mutate(Study=factor(Study, levels=names(c('Study1' = '#085F63', 'Study2' = '#49BEB7',
                                            'Study3' = '#FF5959', 'Study4' = '#FACF5A', 'Study5' =
                                              '#c71585', 'Study6'='#01aaed')))) %>% 
  ggplot(aes(y=Axis2, x=Study, fill=Study)) + 
  xlab(paste0('Group\n','')) +
  geom_boxplot() + 
  scale_fill_manual(values=c('Study1' = '#085F63', 'Study2' = '#49BEB7',
                             'Study3' = '#FF5959', 'Study4' = '#FACF5A', 'Study5' =
                               '#c71585', 'Study6'='#01aaed'), guide = FALSE) +
  
  scale_x_discrete(position='top') +
  theme(axis.ticks=element_blank(), 
        panel.background = element_rect(fill='white', color = 'black'),
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x  = element_text(size = 5),
        panel.grid = element_blank())

###############group plot axis1


df.plot$Group <- factor(df.plot$Group , 
                        levels=c("LA", "HH",
                                 "LT","HT"), 
                        ordered=TRUE)



g.g.1 <- df.plot %>% 
  ggplot(aes(x=Group, y=Axis1, fill=Group)) +
  xlab(paste0('Group\n','')) +
  geom_boxplot() +
  scale_fill_manual(values= c('LA' = '#C2C2C2', 'HH' = '#226597', 'LT' = '#d9544d','HT' = '#A65628'),guide= FALSE) + 
  
  
  theme(axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size = 5),
        axis.title.x  = element_blank(),
        legend.title=element_text(size =15),legend.text=element_text(size = 15),
        panel.background = element_rect(fill='white', color='black'),
        panel.grid = element_blank()) + 
  coord_flip()


#################group plot axis2
g.g.2 <- df.plot %>% 
  ggplot(aes(x=Group, y=Axis2, fill=Group)) +
  xlab(paste0('Group\n','')) +
  geom_boxplot() +
  scale_fill_manual(values=c('LA' = '#C2C2C2', 'HH' = '#226597', 'LT' = '#d9544d','HT' = '#A65628'), guide=FALSE) + 
  scale_x_discrete(position='top') + 
  scale_y_continuous(position = 'right') +
  
  
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=5),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill='white', color='black'),
        panel.grid = element_blank())

pdf('PCoA.pdf', useDingbats = FALSE)
b <- plot_grid(g.main, g.s.2, g.g.2, g.s.1, NULL,NULL,g.g.1, NULL, NULL,
               nrow=3,
               rel_widths = c(0.8, 0.2, 0.2), rel_heights = c(0.8, 0.2, 0.2))
plot(b)
dev.off()


#############PERMANOVA

rm(list=ls())

library("vegan")

df <- read.delim("table.txt", sep = '\t', 
                 row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)

Group <- read.delim("PCoAB.txt", sep = '\t', 
                    row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)

df2<-t(df)

bray_dist<-vegdist(df2,method = "bray")

adonis_result_Group <- adonis(bray_dist~Group, Group, permutations = 999)

adonis_result_Study <- adonis(bray_dist~Study, Group, permutations = 999)


otuput <- data.frame(adonis_result_Study$aov.tab, 
                     check.names = FALSE, stringsAsFactors = FALSE)

otuput <- cbind(rownames(otuput), otuput)

names(otuput) <- c('', 'Df', 'Sums of squares', 
                   'Mean squares', 'F.Model', 'Variation (R2)', 'Pr (>F)')

write.table(otuput, file = 'adonis_result_Study.txt', 
            row.names = FALSE, sep = '\t', quote = FALSE, na = '')

otuput <- data.frame(adonis_result_Group$aov.tab, 
                     check.names = FALSE, stringsAsFactors = FALSE)

otuput <- cbind(rownames(otuput), otuput)

names(otuput) <- c('', 'Df', 'Sums of squares', 
                   'Mean squares', 'F.Model', 'Variation (R2)', 'Pr (>F)')

write.table(otuput, file = 'adonis_result_Group.txt', 
            row.names = FALSE, sep = '\t', quote = FALSE, na = '')


##############Kruskal test

rm(list=ls())

library('coin')

data <- data.frame(read.delim("PCoAB.txt",sep = '\t',header = T))

data$Group<-as.factor(data$Group)

data$Study<-as.factor(data$Study)

res1 <- kruskal_test(Axis1~Group,data=data,alternative='two.sided')

res2<- kruskal_test(Axis2~Group,data=data,alternative='two.sided')

res3<- kruskal_test(Axis1~Study,data=data,alternative='two.sided')

res4<- kruskal_test(Axis2~Study,data=data,alternative='two.sided')

res1

res2

res3

res4

