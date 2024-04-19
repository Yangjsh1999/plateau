############# p value 

rm(list=ls())

library("stats")

data <- read.delim("table1.txt",sep = '\t',header = T,row.names = 1)

Group <- read.delim("Group.txt",sep = '\t',header = T,row.names = 1)

Group <- as.factor(Group$class)

p.vals <- apply(data, 2, FUN=function(x){
  wilcox.test(x~Group, exact=FALSE)$p.value})

p.vals.adj <- p.adjust(p.vals, method='fdr')

write.table(p.vals.adj,"p.vals.adj.txt",sep = "\t")  ##########According to p.vals.adj<0.05, the screening feature is table2.



################## caret

rm(list=ls())

library(randomForest)
library(caret)
library(YSX)
library(Boruta)
library(dplyr)
library(Boruta)


expr_mat <- read.delim("table2.txt", row.names = 1, header = T, sep="\t")

metadata <- read.delim("Group.txt", row.names=1, header=T, sep="\t")

dim(expr_mat)

expr_mat[1:4,1:5]

head(metadata)

expr_mat <- t(expr_mat)

expr_mat_sampleL <- rownames(expr_mat)

metadata_sampleL <- rownames(metadata)

common_sampleL <- intersect(expr_mat_sampleL, metadata_sampleL)

expr_mat <- expr_mat[common_sampleL,,drop=F]
metadata <- metadata[common_sampleL,,drop=F]

group = "class"

if(numCheck(metadata[[group]])){
  if (!is.numeric(metadata[[group]])) {
    metadata[[group]] <- mixedToFloat(metadata[[group]])
  }
} else{
  metadata[[group]] <- as.factor(metadata[[group]])
}

set.seed(304)

rf <- randomForest(expr_mat, metadata$class)
rf

train_index <- createDataPartition(metadata[[group]], p=0.8, list=F)
train_data <- expr_mat[train_index,]
train_data_group <- metadata[[group]][train_index]

test_data <- expr_mat[-train_index,]
test_data_group <- metadata[[group]][-train_index]

boruta <- Boruta(x=train_data, y=train_data_group, pValue=0.01, mcAdj=T, 
                 maxRuns=300)

boruta

Boruta::plotImpHistory(boruta)

table(boruta$finalDecision)

library(dplyr)
boruta.imp <- function(x){
  imp <- reshape2::melt(x$ImpHistory, na.rm=T)[,-1]
  colnames(imp) <- c("Variable","Importance")
  imp <- imp[is.finite(imp$Importance),]
  
  variableGrp <- data.frame(Variable=names(x$finalDecision), 
                            finalDecision=x$finalDecision)
  
  showGrp <- data.frame(Variable=c("shadowMax", "shadowMean", "shadowMin"),
                        finalDecision=c("shadowMax", "shadowMean", "shadowMin"))
  
  variableGrp <- rbind(variableGrp, showGrp)
  
  boruta.variable.imp <- merge(imp, variableGrp, all.x=T)
  
  sortedVariable <- boruta.variable.imp %>% group_by(Variable) %>% 
    summarise(median=median(Importance)) %>% arrange(median)
  sortedVariable <- as.vector(sortedVariable$Variable)
  
  
  boruta.variable.imp$Variable <- factor(boruta.variable.imp$Variable, levels=sortedVariable)
  
  invisible(boruta.variable.imp)
}

boruta.variable.imp <- boruta.imp(boruta)

head(boruta.variable.imp)

write.table(boruta.variable.imp,"r6/RF/three/impA.txt",sep = "\t",row.names = F)

sp_boxplot(boruta.variable.imp, melted=T, xvariable = "Variable", yvariable = "Importance",
           legend_variable = "finalDecision", legend_variable_order = c("shadowMax", "shadowMean", "shadowMin", "Confirmed"),
           xtics_angle = 90)

boruta.finalVarsWithTentative <- data.frame(Item=
                                              getSelectedAttributes(boruta, withTentative = F), 
                                            Type="Boruta_with_tentative")

write.table(boruta.finalVarsWithTentative,"r6/RF/three/imp-finalA.txt",sep
            = "\t",row.names = F)  ##########According to imp-finalA.txt, the screening feature is table3

############################## RF biomarkers
rm(list=ls())

library(randomForest)
library(caret)
library(YSX)
library(Boruta)
library(dplyr)
library(Boruta)


expr_mat <- read.delim("table3.txt", row.names = 1, header = T, sep="\t")

metadata <- read.delim("Group.txt", row.names=1, header=T, sep="\t")

dim(expr_mat)

expr_mat[1:4,1:5]

head(metadata)

expr_mat <- t(expr_mat)

expr_mat_sampleL <- rownames(expr_mat)

metadata_sampleL <- rownames(metadata)

common_sampleL <- intersect(expr_mat_sampleL, metadata_sampleL)

expr_mat <- expr_mat[common_sampleL,,drop=F]
metadata <- metadata[common_sampleL,,drop=F]

group = "class"

if(numCheck(metadata[[group]])){
  if (!is.numeric(metadata[[group]])) {
    metadata[[group]] <- mixedToFloat(metadata[[group]])
  }
} else{
  metadata[[group]] <- as.factor(metadata[[group]])
}

set.seed(304)

expr_mat <- as.data.frame(expr_mat)

rf <- randomForest(expr_mat, metadata$class,importance=TRUE)
rf

train_index <- createDataPartition(metadata[[group]], p=0.8, list=F)
train_data <- expr_mat[train_index,]
train_data_group <- metadata[[group]][train_index]

test_data <- expr_mat[-train_index,]
test_data_group <- metadata[[group]][-train_index]


result<-rfcv(train_data, train_data_group, cv.fold=10, scale = "log", step = 0.9)
result1<-result
result1$n.var # 723


error.cv <- data.frame(num = result$n.var, error.1 =  result$error.cv)

for (i in 305:313){
  print(i)
  set.seed(i)
  result= rfcv(train_data, train_data_group, cv.fold=10, scale = "log", step = 0.9)
  error.cv = cbind(error.cv, result$error.cv)
}

n.var <- error.cv$num
error.cv <- error.cv[,-1]
colnames(error.cv)<- paste('err',1:10,sep='.')
err.mean <-apply(error.cv,1,mean)
allerr<-data.frame(num=n.var,err.mean=err.mean,error.cv)
head(allerr[,1:6])

write.table(allerr, 'allerr.txt', 
            sep = '\t', row.names = FALSE, quote = FALSE)

optimal = x ###### The x are determined according to allerr.txt
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

ggsave(y, Number.pdf', width=10, height=8)


imp_otu <- as_tibble(round(importance(rf), 2),rownames = "OTUid") %>%
  arrange(desc(MeanDecreaseAccuracy))
imp_otu

write.table(imp_otu[1:y,], 'biomarkers.txt', 
            sep = '\t', row.names = FALSE, quote = FALSE)################ The x are determined according to optimal.

##############According to biomarkers.txt, the screening feature is biomarkers.


#################### RF verification

rm(list=ls())

library(randomForest)
library(caret)
library(YSX)
library(Boruta)
library(dplyr)
library(Boruta)
library(pROC)
library(ggrepel)

expr_mat <- read.delim("table1.txt", row.names = 1, header = T, sep="\t")

expr_mat <- t(expr_mat)

metadata <- read.delim("Group.txt", row.names=1, header=T, sep="\t")

boruta.finalVarsWithTentative <- data.frame(
  read.delim("biomarkers.txt", row.names = 1, header = T, sep="\t"))

dim(expr_mat)

expr_mat[1:4,1:5]

head(metadata)

expr_mat <- t(expr_mat)

expr_mat_sampleL <- rownames(expr_mat)

metadata_sampleL <- rownames(metadata)

common_sampleL <- intersect(expr_mat_sampleL, metadata_sampleL)

expr_mat <- expr_mat[common_sampleL,,drop=F]
metadata <- metadata[common_sampleL,,drop=F]

group = "class"

if(numCheck(metadata[[group]])){
  if (!is.numeric(metadata[[group]])) {
    metadata[[group]] <- mixedToFloat(metadata[[group]])
  }
} else{
  metadata[[group]] <- as.factor(metadata[[group]])
}

set.seed(304)


folds <- createDataPartition(metadata[[group]],
                             times =10,p=0.8)



set.seed(304)



library(dplyr)
boruta.imp <- function(x){
  imp <- reshape2::melt(x$ImpHistory, na.rm=T)[,-1]
  colnames(imp) <- c("Variable","Importance")
  imp <- imp[is.finite(imp$Importance),]
  
  variableGrp <- data.frame(Variable=names(x$finalDecision), 
                            finalDecision=x$finalDecision)
  
  showGrp <- data.frame(Variable=c("shadowMax", "shadowMean", "shadowMin"),
                        finalDecision=c("shadowMax", "shadowMean", "shadowMin"))
  
  variableGrp <- rbind(variableGrp, showGrp)
  
  boruta.variable.imp <- merge(imp, variableGrp, all.x=T)
  
  sortedVariable <- boruta.variable.imp %>% group_by(Variable) %>% 
    summarise(median=median(Importance)) %>% arrange(median)
  sortedVariable <- as.vector(sortedVariable$Variable)
  
  
  boruta.variable.imp$Variable <- factor(boruta.variable.imp$Variable, levels=sortedVariable)
  
  invisible(boruta.variable.imp)
}


generateTestVariableSet <- function(num_toal_variable){
  max_power <- ceiling(log10(num_toal_variable))
  tmp_subset <- c(unlist(sapply(1:max_power, function(x) (1:10)^x, simplify = F)), ceiling(max_power/3))
  base::unique(sort(tmp_subset[tmp_subset<num_toal_variable]))
}


customRF <- list(type = "Classification", library = "randomForest", loop = NULL,
                 parameters = data.frame(parameter = c("mtry", "ntree"), 
                                         class = rep("numeric", 2), 
                                         label = c("mtry", "ntree")),
                 
                 grid = function(x, y, len = NULL, search = "grid") {
                   if(search == "grid") {
                     out <- expand.grid(mtry = caret::var_seq(p = ncol(x),
                                                              classification = is.factor(y),
                                                              len = len),
                                        ntree = c(500,700,900,1000,1500))
                   } else {
                     out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)),
                                       ntree = unique(sample(c(500,700,900,1000,1500), 
                                                             size = len, replace = TRUE)))
                   }
                 },
                 fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
                   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
                 },
                 predict = function(modelFit, newdata, preProc = NULL, submodels = NULL)
                   predict(modelFit, newdata),
                 prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
                   predict(modelFit, newdata, type = "prob"),
                 sort = function(x) x[order(x[,1]),],
                 levels = function(x) x$classes
)



ROC <- c(1)

AUC <- c(1)

accuracy <- c(1)

sp <- c(1)

set.seed(304)

for(i in 1:10 ){
  
  
  
  train_data <- expr_mat[folds[[i]],]
  train_data_group1 <- metadata[[group]][folds[[i]]]
  
  train_data1 <- train_data[, boruta.finalVarsWithTentative$Item]
  
  test_data <- expr_mat[-folds[[i]],]
  test_data_group <- metadata[[group]][-folds[[i]]]
  
  
  boruta_mtry <- generateTestVariableSet(ncol(train_data1))
  
  trControl <- trainControl(method="repeatedcv", number=10, repeats=3)
  tuneGrid <- expand.grid(mtry=boruta_mtry,
                          ntree=c(500,700, 800, 1000, 1500, 2000))
  
  borutaConfirmed_rf_default <- train(x=train_data1, y=train_data_group1, method=customRF, 
                                      tuneGrid = tuneGrid, 
                                      metric="Accuracy", 
                                      trControl=trControl)
  
  
  
  borutaConfirmed_rf_default_finalmodel <- borutaConfirmed_rf_default$finalModel 
  
  
  prediction_prob <- predict(borutaConfirmed_rf_default_finalmodel, 
                             newdata=test_data, type="prob")
  
  roc_curve <- roc(test_data_group,prediction_prob[,1])
  
  R <- as.factor(roc_curve$auc)
  
  
  AUC <- data.frame(AUC,R)
  
  predictions <- predict(borutaConfirmed_rf_default_finalmodel, newdata=test_data)
  confusionMatrix(predictions, test_data_group)
  
  best_thresh <- data.frame(coords(roc=roc_curve, x = "best", input="threshold", 
                                   transpose = F, best.method = "youden"))
  
  best_thresh$best <- apply(best_thresh, 1, function (x) 
    paste0('threshold: ', x[1], ' (', round(1-x[2],3), ", ", round(x[3],3), ")"))
  
  best_thresh
  
  
  ROC_data <- data.frame(FPR = 1- roc_curve$specificities, TPR=roc_curve$sensitivities)
  ROC_data <- ROC_data[with(ROC_data, order(FPR,TPR)),]
  
  m <-  ROC_data
  
  ROC <- rbind(ROC,m)
  
  
  predict_result <- data.frame(Predict_status=c(T,F), Predict_class=colnames(prediction_prob))
  
  head(predict_result)
  
  predictions2 <- plyr::join(data.frame(Predict_status=prediction_prob[,1] > best_thresh[1,1]), predict_result)
  
  predictions2 <- as.factor(predictions2$Predict_class)
  
  N <- confusionMatrix(predictions2, test_data_group)
  
  A <- N$overall
  
  B <- N$byClass
  
  accuracy <- cbind(accuracy,A)
  
  sp <- cbind(sp,B)
  
  
  
}


write.table(AUC,"aucRF.txt",sep = "\t",row.names = F)


write.table(accuracy,"accuracyRF.txt",sep = "\t",
            row.names = F)

write.table(sp,"spRF.txt",sep = "\t",row.names = F)

write.table(ROC,"ROCRF.txt",sep = "\t",
            row.names = F)


.

