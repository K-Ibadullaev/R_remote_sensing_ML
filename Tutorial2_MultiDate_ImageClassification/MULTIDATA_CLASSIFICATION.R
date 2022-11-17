#MULTIDATA CLASSIFICATION
rm(list=ls())
# SET-UP-----
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

## load data
setwd(dirname(getActiveDocumentContext()$path))
getwd()

# create list of rasters
raslist = list.files(getwd(), pattern = "img$",
                     full.names = T)
# stack layers
rasvars = stack(raslist)

# check attributes
rasvars

#plot imagery as single bands
TM5_Multidate_84 = plot(rasvars)

# load training data
ta_data = readOGR(getwd(), "TA_1984")

# structure 
str(ta_data)

# summary
summary(ta_data)

# Create a data frame with labeled training points
# containing all reflectance values

ta = as.data.frame(extract(rasvars,ta_data))

ta_data@data=data.frame(ta_data@data, ta[match(rownames(ta_data@data),
                                               rownames(ta)),])
# structure
str(ta_data@data)

# summary statistics
summary(ta_data@data)

# The statistics show that there are NAs. Therefore, we should
# remove the NAs before we proceed.
ta_data@data = na.omit(ta_data@data)

complete.cases(ta_data@data)


# prepare training and test data sets
hre_seed = 27
set.seed(hre_seed)

# split dataset
inTraining = createDataPartition(ta_data@data$Cl1984,
                                 p=.80, list = F)
training = ta_data@data[inTraining,]
testing = ta_data@data[-inTraining,]

#check dimensions
dim(training)
dim(testing)


# Check the summary statistics
summary(training)
summary(testing)


# Visualization of training data set

# feature plots
featurePlot(x = training[, 2:13],
            y = as.factor(training$Cl1984),
            plot = 'density',
            labels=c("Reflectance", "Density distribution"),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            layout = c(2,6),
            auto.key = list(columns = 3))

#box plots

featurePlot(x = training[, 2:13],
            y = as.factor(training$Cl1984),
            plot = 'box',
            labels=c("Reflectance", "Density distribution"),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            layout = c(2, 6),
            auto.key = list(columns = 3))

# correlations
bandCorrelations = cor(training[,2:13])
dim(bandCorrelations)
bandCorrelations

# correlation matrix
corrplot.mixed(bandCorrelations, lower.col = "black",
               upper = "color", number.cex=0.7)

# check the bands which are highly correlated with cutoff point =0.8
highCorrelated <- findCorrelation(bandCorrelations,
                                  cutoff = .8)
# In addition, we can run commands to check how many predictors
# are highly correlated. More importantly, we can get specific names of
# the highly correlated predictor variables.

length(highCorrelated)
names(training)[highCorrelated]



# REDUCE HIGHLY CORRELATED PREDICTORS-----
# Here,we are not removing any highly correlated predictors (bands).
# This is because the RF model uses a subset of predictors to construct
# each decision tree. Therefore, correlation between the single decision
# trees is reduced, thereby improving model performance and accuracy.
# However, there is an option to remove highly correlated predictors as
# shown in the command below. This is useful when you are dealing
# with machine learning methods such as ANN, which are sensitive to
# high predictor collinearity.

# reduced_training <- training[, -highCorrelated]


# Set-up the model tuning parameters ----
fitControl = trainControl(method = "repeatedcv",
                          number =5,
                          repeats = 5)

# Train RF----

# Before we train the RF model, we are going to use a function to
# “register” multiple cores (this is a parallel processing technique that
# specifies the number of cores to be used) in order to increase computational efficiency. In this example, we are going to use the
# doParallel package with five cores on the same machine. The
# package is loaded and then registered as shown in the commands
# below.

cl = makePSOCKcluster(5)
registerDoParallel(cl)

#train RF
set.seed(hre_seed)
rf_model = train(factor(Cl1984)~., data = training,
                 method="rf",
                 trControl=fitControl,
                 prox=T,
                 fitBest = F,
                 returnData=T)
# After finishing training, we can use the stopCluster() function to
# stop the parallel processing.
stopCluster(cl)

#check model performance
print(rf_model)
plot (rf_model)

# The best model had an mtry value of 7 with an overall accuracy of 0.804, which is
# relatively high 

#variable importance
rf_varImp = varImp(rf_model)
ggplot(rf_varImp, top=10)

# Figure  shows the relative importance of the contribution of
# the 12 Landsat 5 TM bands. Band 5 (dry season) is the most informative predictor variable, followed by band 5 (wet season), and band
# 7 (dry season). As noted in Fig. 4.1, band 5 was better at separating
# the land use/cover classes. 

# check the parameters of the best model.
 rf_model$finalModel
 
 
#  The output shows a confusion matrix for the best model (after
# cross-validation). A total of 500 decision trees were used in the RF
#  model. From 12 predictor variables (12 Landsat 5 TM bands), seven
#  predictor variables (bands) were selected at each split. The out-of-bag
#  (OOB) estimate of error rate is 18.4%, which is better than the OOB
#  error estimate in the previous single date RF model.
 
# PREDICT---- 
 # Next, let’s perform accuracy assessment as we done in the previous
 # steps. First, we use the RF model results for prediction, and then build
 # a confusion matrix  
 
 pred_rf = predict(rf_model$finalModel, newdata=testing)
 rf_confMatr = confusionMatrix(data = pred_rf, factor(testing$Cl1984)) 
 rf_confMatr

# The single date RF overall classification accuracy was 0.77, while
# the multidate RF overall classification accuracy was 0.83. This is
# quite a big improvement in accuracy. The RF classifier also performs
# better in terms of individual class performance. The producer’s
# accuracy (sensitivity and specificity) and the user’s accuracy (positive
# predicted values and negative predicted values) are much better for all
# land use/cover classes. While accuracy assessment results are relatively good, 
# misclassification problems are still apparent. For example, 81 bareland pixels were misclassified as agriculture.


# PERFORM CLASSIFICATION WITH RF CLASSIFIER -----
timeStart = proc.time()
LC_rf_84_multidate = predict(rasvars, rf_model)
proc.time() -timeStart

# DISPLAY RESULTS

LC_84_Multdate <- gplot(LC_rf_84_multidate ) + geom_raster(aes(fill = factor(value, 
labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + 
  scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"),na.translate = F, name= "Land Cover") + ggtitle("Random Forest Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()
LC_84_Multdate


# # save raster
# #doesn't work
# writeRaster(LC_84_Multdate,
#             "HreRF_Multidate_84.tif",
#             type="raw",
#             datatype='INT2U',
#             index=1,
#             na.rm=TRUE,
#             progress="window",
#             overwrite=TRUE)

ggsave(plot=LC_84_Multdate, "HreRF_Multidate_84.tiff", device = "tiff")
