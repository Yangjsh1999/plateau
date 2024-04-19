
###############Figure 4E

############## LH vs HH

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
library("rstatix")
library('RColorBrewer')


Alpha <- read.delim("Figure 4E.txt",sep = '\t',header = T)

Alpha$LT2 <- as.factor(Alpha$LT2)

cols = brewer.pal(6, "Set1")


p <- ggbarplot(Alpha, x = 'ID', y = 'fold_change1', fill = 'LT', x.text.angle = 90,y.text.angle = 90,
               palette = cols,
               ggtheme = theme_classic(), position = position_dodge(0.8),
               add = c("mean_se")) +
  scale_fill_manual(values = cols) +
  labs(x = 'OTU', y = 'Log2(Fold_Change[HH])', fill = 'Supplement type')


p


ggsave(p, file='Figure 4E-HH.pdf', width=20, height=15)


############## LH vs HT


p <- ggbarplot(Alpha, x = 'ID', y = 'fold_change2', fill = 'LT', x.text.angle = 90,y.text.angle = 90,
               palette = cols,
               ggtheme = theme_classic(), position = position_dodge(0.8),
               add = c("mean_se")) +
  scale_fill_manual(values = cols) +
  labs(x = 'OTU', y = 'Log2(Fold_Change[HT]))', fill = 'Supplement type')


p

ggsave(p, file='Figure 4E-HT.pdf', width=20, height=15)

##############   The images Figure 4E-HH and Figure 4E-HT are flipped together in Adobe Illustrator CS5  adding family and genus level information.


