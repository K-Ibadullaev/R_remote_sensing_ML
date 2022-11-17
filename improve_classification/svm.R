#rm(list=ls)
library(sp)
library(rgdal)
library(raster)
library(reshape)
library(grid)
library(gridExtra)
library(RStoolbox)
library(caret)
library(rasterVis)
library(corrplot)
library(doParallel)
library(NeuralNetTools)
library(rstudioapi)
library(ggplot2)
library(e1071)

# load data
setwd(dirname(getActiveDocumentContext()$path))
getwd()

raslist = list.files(getwd(),pattern="img$", full.names = T)
rasVars = stack(raslist)

rasVars

#visualize bands
plot(rasVars)


# training data
ta_data = readOGR(getwd(), "TA_1984")

summary(ta_data)

# Use training points to extract reflectance values
# Next, assign raster values to the training data set.
ta = as.data.frame(extract(rasVars, ta_data))
ta_data@data=data.frame(ta_data@data,ta[match(rownames(ta_data@data),
                                              rownames(ta)),])

#check str
str(ta_data@data)


#check NAs
complete.cases(ta_data@data)
length(ta_data@data[is.na(ta_data@data)])
dim(ta_data@data)

ta_data@data = na.omit(ta_data@data)
length(ta_data@data[is.na(ta_data@data)])
complete.cases(ta_data@data)
dim(ta_data@data)

# Prepare training and test data sets-------
hre_seed = 27
set.seed(hre_seed)

# split data sets
inTraining =createDataPartition(ta_data@data$Cl1984,
                                p=.80, list=F)
training = ta_data@data[inTraining,]
testing = ta_data@data[-inTraining,]

#check dimensions
dim(training)
dim(testing)


#correlation matrix
bandCorrelations = cor(training[,2:43])
dim(bandCorrelations)

# correlation 
corrplot(bandCorrelations, method = "color",
         order = 'hclust')

# cutoff with 0.8
highCorrelated = findCorrelation(bandCorrelations,
                                 cutoff = 0.8)

length(highCorrelated)
# names of high correlated variables
names(training)[highCorrelated]


# set-up model tuning parameters
hre_seed=27
set.seed(hre_seed)
fitControl = trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5)
# SVM---------------

cl = makePSOCKcluster(5)
registerDoParallel(cl)


timeStart = proc.time()

# svm_model = caret::train(factor(Cl1984)~.,data = training[-highCorrelated],
#                          method="svmRadial",
#                          trControl = fitControl,
#                          preProc = c("center","scale"),
#                          tuneLength=3)

#remove high correlated data
svm_model = caret::train(factor(Cl1984)~.,data = training[-highCorrelated],
                 method="svmRadial",
                 trControl = fitControl,
                 preProc = c("center","scale"),
                 tuneLength=3)
proc.time() - timeStart
stopCluster(cl)


print(svm_model)
plot(svm_model)

#check the best model
svm_model$finalModel

# The output shows that only 4,626 support vectors were selected
# from a total of 6,195 training samples

# check importance of variables
# svm_varImp =varImp(svm_model, competes = FALSE)
# plot(svm_varImp)

pred_svm = predict(svm_model, newdata=testing)

svm_confmatr = confusionMatrix(data=pred_svm, factor(testing$Cl1984))
svm_confmatr


#KNN -------------
cl = makePSOCKcluster(5)
registerDoParallel(cl)
set.seed(hre_seed) 
training$Cl1984=factor(training$Cl1984)
knn_model =train(Cl1984~., data=training,
                     method="kknn",
                     preProcess=c("center","scale"),
                     trControl=fitControl)
proc.time() - timeStart
stopCluster(cl)

# attributes
print(knn_model)
plot(knn_model)

knn_model$finalModel

# display variable importance using the varImp() function
knn_varImp = caret::varImp(knn_model, competes=F)
plot(knn_varImp)


# now let's predict
predKnn = predict(knn_model,newdata=testing)
knn_matr=confusionMatrix(data=predKnn, factor(testing$Cl1984) )
knn_matr

#PREDICT
timeStart =proc.time()
cl = makePSOCKcluster(5)
registerDoParallel(cl)
LC_knn <-predict(rasVars,knn_model)
LC_svm <-predict(rasVars,svm_model)
proc.time() - timeStart
stopCluster(cl)

LC_svm_plot <- gplot(LC_svm) + geom_raster(aes(fill= factor(value, labels=c("Agriculture", "Bareland","Green Spaces", "Urban", "Water")))) +
  scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"),na.translate = F,name= "Land Cover") + 
  ggtitle("Support Vector Machine Classification") +
  theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()
LC_svm_plot
ggsave(plot=LC_svm_plot, "LC_svm_plot.tiff", device = "tiff")

LC_knn_plot <- gplot(LC_knn) + geom_raster(aes(fill = factor(value, labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"),na.translate = F, name= "Land Cover") + ggtitle("K Nearest Neighbour Classification") +
  theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()
LC_knn_plot 
ggsave(plot=LC_knn_plot, "LC_knn_plot.tiff", device = "tiff")
grid.arrange(LC_knn_plot, LC_svm_plot, ncol=2)

# LC_84_Multdate <- gplot(LC_rf_84_multidate ) + geom_raster(aes(fill = factor(value, 
#                                                                              labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + 
#   scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"),na.translate = F, name= "Land Cover") + ggtitle("Random Forest Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()
# LC_84_Multdate
# 
# grid.arrange(LC_knn_plot, LC_svm_plot,LC_rf_84_MultiVariables2, ncol=2)

