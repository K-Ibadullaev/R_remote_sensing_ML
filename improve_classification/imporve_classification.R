# Satellite image derivatives such as vegetation and texture indices as
# well as multidate images, multisensor images, and digital elevation
# model (DEM) have been reported to improve image classification.
# However, the increase in additional predictor variables results in high
# data dimensionality and redundancy. Feature selection and extraction
# can be used to reduce high data dimensionality and minimize
# redundancy (Dash and Liu 1997). A number of techniques such as
# principle component analysis have been used to reduce high data
# dimensionality and redundancy.
# In this chapter, we are going to perform image classification using
# two different approaches. First, we are going to perform image
# classification using the RF classifier and multiple data sets, which
# comprises multidate Landsat 5 TM imagery, and vegetation and
# texture indices. Second, we are going to perform image classification
# using the RF classifier with feature selection and multiple data sets.
# Our main aim is to check whether the use of multiple data sets as well
# as feature selection improves image classification.


# Importance of Feature Selection------
# Machine learning classifiers are affected by high data dimensionality
# and noise, particularly if many predictor variables are used during
# classification (Yu and Liu 2004). Therefore, it is important to select
# optimal predictor variables because they: (i) enable the machine
# learning algorithm to train faster; and (ii) reduce model complexity.
# Filter and wrapper embedded methods are some of the most commonly used feature selection techniques (Kuhn and Johnson 2016).
# Filter methods measure the relevance of features based on correlation with the response variable, while wrapper methods measure the
# usefulness of a subset of feature by actually training a model on it
# (Kuhn and Johnson 2016). Generally, filter methods evaluate each
# predictor in isolation before training the model or classifier (Kuhn and
# Johnson 2016). However, the wrapper methods evaluate multiple
# models in order to add or remove features from the subset, which
# eventually lead to the optimal model. Some common examples of
# wrapper methods are forward feature selection, backward feature
# elimination, recursive feature elimination (RFE). While filter methods
# are much faster compared to wrapper methods (since they do not
# involve training the models), they often fail to find the best subset of
# features, especially if there many highly correlated variables.

# Recursive Feature Elimination (RFE)----------
# In this chapter, we are going to use RFE in order to select optimal
# predictor variables. RFE is a greedy optimization algorithm that aims
# to find the best performing predictor variables. This technique
# repeatedly creates models and keeps aside the best or the worst performing feature at each iteration, and then constructs the next model
# with the remaining features until all the features are exhausted. RFE
# then ranks the features based on the order of their elimination.



#RF with no improvements ----
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

# Note that we removed a small proportion of the training data set.
# Therefore, the remaining data set is still good enough to serve our
# purpose.


# Please note that, RF classifier does not handle missing values in
# predictors. Therefore, training data with missing values can be
# either removed or imputed (fill in missing values) with the
# median (for numerical values) or mode (for categorical values).
# There are many techniques for imputing missing data. While it is
# not recommended to remove training points in case you do not
# have enough training, imputing data is computer intensive. For
# the purpose of this tutorial, we are going to remove the NAs
# since we have a small proportion of missing values.

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
set.seed(hre_seed)
fitControl = trainControl(method = "repeatedcv",
                         number = 5,
                         repeats = 5)
# train RF

cl = makePSOCKcluster(5)
registerDoParallel(cl)
set.seed(hre_seed)

timeStart = proc.time()
rf_model = train(factor(Cl1984)~., data = training,
                 method="rf",
                 trControl=fitControl,
                 prox=T,
                 fitBest = F,
                 returnData=T)
proc.time() - timeStart
stopCluster(cl)


# check model
print(rf_model)
plot(rf_model)

# 
# The results show that 6,187 training samples were used for training. Five land use/cover classes represent the response variable, and
# there are 42 predictor variables. The best model had an mtry value of
# 22 with an overall accuracy of 84%, which is relatively high

# check best models
rf_model$finalModel

 
# The output shows a confusion matrix for the best model (after
# cross-validation). A total of 500 decision trees were used in the RF
# model. The out-of-bag (OOB) estimate of error rate is 14.85%. 

#check variable importance
rf_varImp = varImp(rf_model, competes = F)
plot(rf_varImp, top=20)

# Plot shows the relative importance of the contribution of the
# top 20 predictor variables. The top 5 predictors are from the dry
# season Landsat 5 TM imagery (acquired in June), followed by
# Landsat 5 TM band 1 from the wet season (acquired in December). In
# particular, NDVI and SAVI, bands 4 and 5 variance, and Landsat 5
# TM band 5 (all dry season) are the most informative predictor variables. As observed in Fig. 4.1, band 5 is better at separating land use/
#   cover in the test site.


# Accuracy assessment as we done in the previous
# steps. First, we use the RF model results for prediction, and then build
# a confusion matrix 

pred_rf = predict(rf_model$finalModel,newdata=testing)
confusionMatrix(data = pred_rf, factor(testing$Cl1984))

#There is a slight increase in overall classification accuracy compared to the
# previous multidate RF model

# perform classification using RF
timeStart =proc.time()
LC_rf_84 = predict(rasVars, rf_model)
proc.time()- timeStart



# DISPLAY RESULTS
LC_rf_84_MultiVariables1<- gplot(LC_rf_84) + geom_raster(aes(fill = factor(value, 
labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + 
  scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"), name= "Land Cover") + ggtitle("Random For
est Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()
LC_rf_84_MultiVariables1

ggsave(plot=LC_rf_84_MultiVariables1, "RF84_MultiVariables1.tif", device = "tiff")
#for QGIS doesn't work
# writeRaster(LC_rf_84_MultiVariables1,
#             "RF84_MultiVariables1.tif",
#             type="raw",
#             datatype='INT1U',
#             index=1,
#             na.rm=TRUE,
#             progress="window",
#             overwrite=TRUE)


# we observed that the multiple data sets (which comprises
# multidate Landsat 5 TM imagery, vegetation and texture indices)
# slightly improved land use/cover classification. However, we also
# observed high between predictor correlations. Next, let’s try to optimize the RF model using feature selection, and see if we can improve
# classification




# WITH FEATURE SELECTION---------

# load data
raslist = list.files(getwd(), pattern = "img$",
                     full.names = T)
rasVars = stack(raslist)

# training data
ta_data = readOGR(getwd(), "TA_1984")

# Use training points to extract reflectance values
ta = as.data.frame(extract(rasVars, ta_data))
ta_data@data = data.frame(ta_data@data, ta[match(rownames(ta_data@data),
                                                 rownames(ta)),])

str(ta_data@data)


ta_data@data <- na.omit(ta_data@data)
complete.cases(ta_data@data)


#prepare training and test data sets
hre_seed= 27
set.seed(hre_seed)


# split data sets
inTraining =  createDataPartition(ta_data@data$Cl1984,
                                  p=.80, list=F)
training = ta_data@data[inTraining,]
testing = ta_data@data[-inTraining,]

dim(training)
dim(testing)

# TRAIN THE RFE-RF METHOD
control = rfeControl(functions=rfFuncs,
                     method = "repeatedcv",
                     number = 5)

# Next, run the RFE algorithm. Notice that the model specification is
# different here. Furthermore, this model takes some time to run.

cl = makePSOCKcluster(5)
registerDoParallel(cl)


timeStart = proc.time()
rfe_model = rfe(training[,2:43],
                factor(training[,1]),
                sizes=c(2:43),
                method="rf",
                rfeControl=control,
                prox=TRUE,
                fitBest = FALSE,
                returnData = TRUE)
proc.time() - timeStart
stopCluster(cl)


#check the attributes
print(rfe_model)

# The RFE algorithm selected “variance_22_6_84_b5”, “variance_22_6_84_b4”, “mean_22_6_84_b4”, “mean_22_6_84_b5”, and
# “variance_31_12_84_b4” as the top five variables.
# Next, list the selected features. In total 26 predictor variables were
# selected from 42. Next, check the selected predictor variables

predictors(rfe_model)

plot(rfe_model, type=c("g", "o"))


#TRAIN RFE model
# specify the 28 top predictor variables from the RFE model
(f <- as.formula(paste("Cl1984", paste(rfe_model$optVariables, collapse=" + "), sep=" ~ ")))


#fit control
fitControl= trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)
set.seed(hre_seed)

# After that, we can set up the RF model and execute the commands
# below.
cl = makePSOCKcluster(5)
registerDoParallel(cl)
timeStart = proc.time()
opt_rfmodel = train(f,data=training,
                method="rf",
                trControl=fitControl,
                prox=TRUE,
                fitBest = FALSE,
                returnData = TRUE)
proc.time() - timeStart
stopCluster(cl)


# It took approximately 14 min to run the RF model with the optimal
# predictor variables.

print(opt_rfmodel)
plot(opt_rfmodel)

# The output shows that 6,187 training samples were used for
# training. Here, there are five land use/cover classes that represent the
# response variable, and 26 optimal predictor variables. The best model
# had an mtry value of 2 with an overall accuracy of 84%

opt_rfmodel$finalModel

# The output shows a confusion matrix for the best model (after
# cross-validation). A total of 500 decision trees were used in the RF
# model. From 23 predictor variables, only two predictor variables were
# selected at each split. The out-of-bag (OOB) estimate of error rate is
# 14.64%, which is slightly better than the previous model in tutorial 1


# Perform classification accuracy assessment
pred_rf <- predict(opt_rfmodel$finalModel,
                   newdata = testing)
confusionMatrix(data = pred_rf, factor(testing$Cl1984))

# The overall classification accuracy for the RF-RFE (with feature
# selection) model is 86.3%, while the overall accuracy for the simple
# RF model (without feature selection) is 84.8%. This shows that feature selection based on the RFE model did not improve land use/cover
# classification. However, the RFE managed to reduce the predictor
# variables and thus reduced computation time for the final RF model

timeStart<- proc.time() # measure computation time
LC_rf_84 <-predict(rasVars,opt_rfmodel)
proc.time() - timeStart # user time and system time.

LC_rf_84_MultiVariables2<- gplot(LC_rf_84) + geom_raster(aes(fill = factor(value, 
                                                                           labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + 
  scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"),na.translate = F, name= "Land Cover") + ggtitle("Random Forest Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()
LC_rf_84_MultiVariables2

ggsave(plot=LC_rf_84_MultiVariables2, "RF84_MultiVariables1.tif", device = "tiff")


# We observed that the use of multiple data sets (which comprises
#                                                 multidate Landsat 5 TM imagery, vegetation and texture indices)
# slightly improved land use/cover classification. Although the OBB
# error rate for the RF-RFE model (with feature selection) was better
# than the simple RF model (without feature selection), the overall
# accuracy of the latter was slightly higher. The better performance
# achieved by the simple RF model is partly attributed to the fact that
# the simple RF model successfully selected the most informative
# predictor variables during model training. Therefore, performing RFE
# feature selection did not provide additional information that improves
# image classification. While the RFE-RF model did not improve land
# use/cover classification accuracy, it reduced the number of predictor
# variables. This is important since it can be used to reduce computation
# time.




