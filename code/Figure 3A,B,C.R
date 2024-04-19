################## Figure 3A OTU Number LHvsHH

rm(list=ls())

library(ggplot2)


allerr <- read.delim("Figure 3A.txt", row.names = 1, header = T, sep="\t")

optimal = 72
main_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=0.5, colour="black"),
                   axis.line.y=element_line(size=0.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=12),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=12),
                   text=element_text(family="sans", size=12))
max(allerr$num)


y <- ggplot() +
  geom_line(aes(x = allerr$num, y = allerr$err.1), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.2), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.3), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.4), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.5), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.6), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.7), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.8), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.9), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.10), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.mean), colour = 'black',size=0.5) +
  geom_vline(xintercept = optimal, colour='black', lwd=0.36, linetype=2) +
  coord_trans(x = "log2") +
  scale_x_continuous(breaks = c(1, 5, 10, 20, 30, 50, 100, 300, 723)) + # , max(allerr$num)
  labs( x='Number of OTUs ', y='Cross-validation error rate') +
  annotate("text", x = optimal, y = max(allerr$err.mean), label=paste("Optimal = ", optimal, sep=""))+
  main_theme
y

ggsave(y, file='Figure 3A.pdf', width=10, height=8)




################## Figure 3B OTU Number LHvsHT

rm(list=ls())

library(ggplot2)


allerr <- read.delim("Figure 3B.txt", row.names = 1, header = T, sep="\t")

optimal = 62
main_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=0.5, colour="black"),
                   axis.line.y=element_line(size=0.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=12),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=12),
                   text=element_text(family="sans", size=12))
max(allerr$num)


y <- ggplot() +
  geom_line(aes(x = allerr$num, y = allerr$err.1), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.2), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.3), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.4), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.5), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.6), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.7), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.8), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.9), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.10), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.mean), colour = 'black',size=0.5) +
  geom_vline(xintercept = optimal, colour='black', lwd=0.36, linetype=2) +
  coord_trans(x = "log2") +
  scale_x_continuous(breaks = c(1, 5, 10, 20, 30, 50, 100, 300, 723)) + # , max(allerr$num)
  labs( x='Number of OTUs ', y='Cross-validation error rate') +
  annotate("text", x = optimal, y = max(allerr$err.mean), label=paste("Optimal = ", optimal, sep=""))+
  main_theme
y

ggsave(y, file='Figure 3B.pdf', width=10, height=8)



################### Figure 3c OTU Number LHvsLT

rm(list=ls())

library(ggplot2)


allerr <- read.delim("Figure 3C.txt", row.names = 1, header = T, sep="\t")

optimal = 12
main_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=0.5, colour="black"),
                   axis.line.y=element_line(size=0.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=12),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=12),
                   text=element_text(family="sans", size=12))
max(allerr$num)


y <- ggplot() +
  geom_line(aes(x = allerr$num, y = allerr$err.1), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.2), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.3), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.4), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.5), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.6), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.7), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.8), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.9), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.10), colour = 'grey',size=0.5) +
  geom_line(aes(x = allerr$num, y = allerr$err.mean), colour = 'black',size=0.5) +
  geom_vline(xintercept = optimal, colour='black', lwd=0.36, linetype=2) +
  coord_trans(x = "log2") +
  scale_x_continuous(breaks = c(1, 5, 10, 20, 30, 50, 100, 300, 723)) + # , max(allerr$num)
  labs( x='Number of OTUs ', y='Cross-validation error rate') +
  annotate("text", x = optimal, y = max(allerr$err.mean), label=paste("Optimal = ", optimal, sep=""))+
  main_theme
y

ggsave(y, file='Figure 3C.pdf', width=10, height=8)
