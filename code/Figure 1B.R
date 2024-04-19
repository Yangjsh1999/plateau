
###################Figure 1B Sample classification statistics

######### Sequencing area

rm(list=ls())

info <- c(603,65)

names <- c("v3-v4","v4")

cols = brewer.pal(2, "Set1")

piepercent = paste(round(100*
                           info/sum(info)), "%")

pie(info,labels=piepercent, col=cols)

legend("topright", names, cex=0.8, fill=cols)

#########Study

library('RColorBrewer')

info <- c(273,40,65,89,104,133)

names <- c("Study1","Study2","Study3","Study4","Study5","Study6")

cols = brewer.pal(6, "Set1")

piepercent = paste(round(100*
                           info/sum(info)), "%")

pie(info,labels=piepercent, col=cols)

legend("topright", names, cex=0.8, fill=cols)

###########region

rm(list=ls())

info <- c(276,96,69,103,43,30,10,41)

names <- c("Tibet","Xinjiang","Qinghai","Shanxi","Hubei","Jiangsu","Guangdong",
           "Other")


cols = brewer.pal(8, "Set1")

piepercent = paste(round(100*
                           info/sum(info)), "%")

pie(info,labels=piepercent, col=cols)

legend("topright", names, cex=0.8, fill=cols)

###############group

info <- c(263,105,60,240)

names <- c("LH","HH","LT","HT")


cols = brewer.pal(4, "Set1")

piepercent = paste(round(100*
                           info/sum(info)), "%")

pie(info,labels=piepercent, col=cols)

legend("topright", names, cex=0.8, fill=cols)
