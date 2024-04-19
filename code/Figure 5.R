######################## Figure 5

rm(list=ls())

library(stringr)
library(grid)
library(forestploter)

bc <- read.delim("Figure 5.txt",sep = '\t',header = T)


bc$se<- (log(as.numeric(bc$hi)) - log(as.numeric(bc$OR)))/1.96

bc$hi<-as.numeric(bc$hi)
bc$low<-as.numeric(bc$low)

bc$`OR(95% CI)` <- ifelse(is.na(bc$se), "",
                          sprintf("%.2f(%.2f to %.2f)", bc$OR,bc$low, bc$hi))


bc$` `<- paste(rep(" ", 20), collapse = " ")


tm <- forest_theme(base_size = 10, 
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,   
                   ci_col = "#762a83",    
                   ci_fill = "blue",    
                   ci_alpha = 0.8,   
                   ci_lty = 1,            
                   ci_lwd = 5,          
                   ci_Theight = 0.2, 
                   refline_lwd = 1,      
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   vertline_lwd = 1,              
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   summary_fill = "black",      
                   summary_col = "#4575b4", 
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "red")


p <- forest(bc[,c(2:3,10,11,8)],
            est = bc$OR,       
            lower = bc$low,     
            upper = bc$hi,     
            sizes = bc$se,
            ci_column = 4,   
            ref_line = 1,
            arrow_lab = c("No Schizophrenia", "Schizophrenia"),
            xlim = c(0, 2),
            ticks_at = c(0.5, 1, 1.5, 2),
            footnote = "",
            theme = tm)

p