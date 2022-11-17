# install.packages("caret") caret refers to Classification And REgression Training
# install.packages("e1071") support vector machines, bagged
#                           clustering, naive Bayes classifier, fuzzy clustering
# install.packages("kernlab")  functions for SVM
# install.packages("rasterVis")
# install.packages("maptree") example data for graphing, pruning,
#                             and mapping models from hierarchical clustering, and classification and
#                               regression trees
# install.packages("corplot")  graphical display of a correlation matrix
# install.packages("doParallel") parallel execution of R code on
#                               machines with multiple cores or processors or multiple computers
# install.packages("NeuralNetTools")

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
 
 
 #load data------
 setwd(dirname(getActiveDocumentContext()$path))
 getwd()
 
 # import rasters
 raslist = list.files(getwd(), pattern = "img$",
                      full.names = T)
 # stack raster layers
 rasVars = stack(raslist)
 
 #check attributes
 rasVars
# visualize
TM5_22_06_84 = plotRGB(rasVars, r=5,g=4,b=3,
          stretch="lin")



#load training points
ta_data= readOGR(getwd(), "TA_1984")

#check structure
ta_data


# create a spectral file------

# Understanding spectral reflectance curves for different land use/cover
# classes at different wavelengths is important. Let’s now create a
# spectral profile of the target land use/cover classes from the Landsat 5
# TM imagery. 
# First, we need to extract the reflectance values using the Landsat 5
# TM bands (rasVars) and training points (ta_data). This will create a
# data frame called “ta” (training area) that contains reflectance values
# from the Landsat 5 TM bands. 

ta = as.data.frame(extract(rasVars, ta_data))
# check the reflectance values in the first lines of the “ta”
# data frame.
head(ta)


#mean reflectence values for each land use/cover class
# using aggregate() to aggregate the training areas (ta) and the land
# use/cover class names contained in “ta_data” object.

mr = aggregate(ta, list(ta_data$Cl1984), mean, na.rm=T)
head(mr)

# Next, let’s remove the first column so that we can use the actual
# land use/cover names.
rownames(mr) =mr[,1]
mr=mr[,-1]
mr


# color vector for legend
mycolor = c('yellow','grey','green3','red','blue3')
# Next, let’s transform the data frame to a matrix.
mr =as.matrix(mr)
# create a spectre profile graph
#firstly,empty plot 
plot(0, ylim=c(0, 1.0), xlim=c(1,7),
     type='n', xlab="Bands", ylab="Reflectence")

# add different land use classes via loop
for (i in 1:nrow(mr)) {
        lines(mr[i,], type="l",lwd=3,
              lty=1,col=mycolor[i])
        
}
title(main="Spectral Profile from Landsat 5 TM", font.main=2)

#legend
legend("topleft", rownames(mr),cex = 0.8, col=mycolor,lty=1, lwd=3, bty="n")
# Spectral profile for Landsat 5 TM bands. 
# Note that 6 above refer to Landsat 5 TM band 7

#create df with labeled training points----
# containing all reflectence values
# In step 4, we used the “ta” data frame to compute mean spectral
# reflectance and then created a spectral profile. We now need to create
# a new data frame that contains labeled training points with all
# reflectance values for each land use/cover class. Use the data.frame()
# function to create the “ta_data@data”. Note that the “ta_data@data”
# object has the slots for the land use/cover class names and spectral
# reflectance values. The @ is a special operator for accessing the
# objects stored in another object.

ta_data@data=data.frame(ta_data@data,ta[match(rownames
                                              (ta_data@data), rownames(ta)),])
# structure
str(ta_data@data)

# The response variable consists of five target classes (agriculture,
#                                                        bareland, green spaces, settlement and water), while the predictor
# variables consist of six Landsat 5 TM bands (1, 2, 3, 4, 5 and 7).
# Next, check the summary statistics to see if there are NAs
summary(ta_data@data)


# The statistics show that the predictor variables (Landsat 5 TM
#                                                   bands) have missing values (NAs). It is important to treat missing
# values prior to classification because some machine learning classifiers cannot handle missing values. Next, let’s remove the NAs before
# we proceed

ta_data@data = na.omit(ta_data@data)

is.na(ta_data@data )
# Use the complete.cases() function to check if the NAs have been
# removed. Alternatively, you can use anyNA() function in order to
# check whether the data contains missing values or not.

complete.cases(ta_data@data)


# prepare testing and training data sets----

# Generally, it is recommended to randomly partition the training data
# set into training, validation and testing sets.
# Setting the seed to the same number will ensure that
# we get the same result. 

hre_seed =27
set.seed(hre_seed)


# Next, split the training data set into training and test sets using the
# createDataPartition()

# In particular, the training set will
# be used to find the optimal model parameters as well as to check
# initial model performance based on repeated cross-validation. The test
# set will be used for final accuracy assessment. 


# Note that the response (target) variable is ta_data@data
# $Cl1984, which contains the different land use/cover classes.
# The “p” parameter shows the percentage of the split, that is we
# are using p = 0.8. This simply means that data split has 80%
# training set and 20% test set. The “list” parameter indicates
# whether to return a list or matrix. In this case, we pass FALSE
# because we are not returning a list

inTraining <- createDataPartition(ta_data@data$Cl1984,
                                  p = .80, list = FALSE)
training = ta_data@data[inTraining,]
testing = ta_data@data[-inTraining,]

#summary stats

summary(training)
summary(testing)


# While the output shows useful information such as the highly
# imbalanced nature of land use/cover classes, it is difficult to
# understand nature of distribution (normal or skewed), or to detect
# other problems such as outliers and collinearity

#visualization----

# Next, we further explore training data using graphical visualizations.
# Graphs provide a useful way of understanding the training data set. In
# addition, graphs can help the remote sensing analysts to select the
# most appropriate methods for pre-processing, transformation and
# classification. Here, we are going to use the featurePlot() function
# (which is a wrapper for different lattice plots) and the ggplot2()
# function in order to visualize the training data set. In addition, we are
# going to use the corrplot() function to create band correlation
# matrices.
# Let’s start by creating density estimation plots (density plots) for
# each attribute by class value using the featurePlot() function. The
# density plots summarize the distribution of the data. This highlights
# useful structures like linear separability of attribute values into classes


featurePlot(x = training[, 2:7],
            y = as.factor(training$Cl1984),
            plot = 'density',
            labels=c("Reflectance", "Density distribution"),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            layout = c(3, 2),
            auto.key = list(columns = 3))


# Clearly, the density plots show that bands 1 and 2 (that is
#                                                     “CostzPr_22_6_84.1” and “CostzPr_22_6_84.2”) display significant
# skewness. We can also calculate skewness of the bands using the
# skewness() function available in the e1071 package
library(e1071)

skewnessValues <- apply(training[, 2:7], 2, skewness)
skewnessValues


# Generally, the distribution is symmetric when the skewness value
# is zero or closer to zero. The output shows that the skewness values
# for bands 1 and 2 are 1.8 and 1.4, respectively. This indicates that the
# distributions are indeed skewed.some land use/covers classes such as
# water in bands 5 and 6 are bimodal. In addition, the density plots
# suggest presence of outliers. Therefore, we need to check for outliers

# This summarizes the
# spread of attributes. Note that box plots summarize the distribution of
# a given attribute by showing a box for the 25th and 75th percentile, a
# line in the box for the 50th percentile (median) and a dot for the mean.
# The whiskers show 1.5 times the height of the box (i.e., Inter Quartile
#                                                    Range), which indicates the expected range of the data. Data beyond
# those whiskers is assumed to be an outlier and marked with a dot

featurePlot(x=training[,2:7],y=as.factor(training$Cl1984),
            plot="box",
            scales=list(y=list(relation="free"),
                        x=list(rot=90)),
            layout=c(2,3),
            auto.key=list(colmns=2))

# Plot shows the presence of outliers (that is extreme reflectance values), especially for the bareland and agriculture classes. This
# is problematic for some machine learning classifiers. One solution
# would be to remove the outliers from the training data sets. However,
# this could result in throwing away training data, which normally is
# expensive and time consuming to generate. Therefore, it is better to
# understand the reason for having outliers in the first place. While the
# presence of some outliers can be attributed to human error during
# compilation of reference data, in this case the presence of outliers in
# the agriculture and bareland is partly attributed to soil variability in
# the test site.Plot of spectral reflectance indicated almost similar spectral reflectance
# between the agriculture and bareland because both classes are 
# dominated by bare soils during the dry season. This example, illustrate the importance of understanding the geographical characteristics
# of the test site

#check relationships between 2 bands-----
Band1_2<- ggplot(data = ta_data@data, aes(Layer_1, Layer_2)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 2")
Band1_3<-ggplot(data = ta_data@data, aes(Layer_1, Layer_3)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 3")
Band1_4<-ggplot(data = ta_data@data, aes(Layer_1, Layer_4)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 4")
Band1_5<- ggplot(data = ta_data@data, aes(Layer_1, Layer_5)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 5")
Band1_7<-ggplot(data = ta_data@data, aes(Layer_1, Layer_6)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 7")

grid.arrange(Band1_2, Band1_3,Band1_4,Band1_5,Band1_7)

# bands 1 and 2 as well as bands 1 and 3
# have difficulty to separate land use/cover classes. In addition, Fig. 4.4
# suggests that there is high between band (predictor) correlations,
# especially for bands 1 and 2, and bands 1 and 3. Therefore, it is better
# to check between band correlations. So let’s check correlation for the
# six Landsat 5 TM bands using the cor() and corrplot() functions.

bandCorrelations =cor(training[,2:7])
dim(bandCorrelations)
library(magrittr)
bandCorrelations 

# plot correlations------
corrplot(bandCorrelations, method = "number",
         type = "upper")
corrplot(bandCorrelations, method = "color",
         order = "hclust",type = "lower")
corrplot.mixed(bandCorrelations,lower.col = "black",
               number.cex=.7, upper = "color")

# Note that it is recommended to
# remove the highly correlated bands or perform a principal component
# analysis in order to reduce redundancy and improve computation
# efficiency. However, in this tutorial exercise we are going to use all
# the bands since we have only six bands.

# In summary, the preliminary statistical and graphical analysis
# shows the training data has skewed and bimodal distributions as well
# as many outliers. In addition, we observed relatively high class
# imbalance, and some of the predictors such as bands 1 and 2 are
# highly correlated. Therefore, it would be inappropriate to use conventional classifiers such as maximum likelihood that depend on the
# assumption of normal of distribution. Although machine learning
# classifiers are not affected by the normal distribution assumption,
# problems such as class imbalance, outliers and between predictor
# collinearity have negative impact on performance and accuracy.
# Therefore, it is important to evaluate many machine learning classifiers in order to find the most appropriate one, which improves performance and accuracy.



# set-up model tunning parameters------

# Setting up model tuning parameters is important because most classifiers have at least one key parameter, which control the complexity of
# the model. Failure to select appropriate model parameters may result in
# problems such as overfitting.

fitControl = trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5)
# We specify the “method”, “number” and “repeats” parameters of
# the trainControl() function. The “method” parameter contains
# resampling methods such as “repeatedcv”, “boot”, “cv”, “LOOCV”
# etc. In this workbook, we are going to use repeatedcv that is the
# repeated cross-validation. The “number” parameter refers to the
# number of folds or number of resampling iterations. The “repeats”
# parameter provides sets of folds in order to compute repeated
# cross-validation. In this case, we are running a 5-fold cross validation
# and repeating it five times (that is, number = 5 and repeats = 5)


# Cross-validation is a procedure whereby the training data
# set is subdivided into a number of mutually exclusive groups.
# For the k-fold cross-validation, the initial training data set is
# randomly partitioned into k mutually exclusive subsets or folds,
# which are approximately of the same size. Training and testing is
# then performed k times. That is, the k-fold cross-validation
# repeats the model construction using different subsets of the
# available training data, and then evaluates the model only on
# unseen data during model building. Note that I have specified k
# value of 5 in this workbook because it is computationally efficient. However, the k value is recommended to be 5 or 10 in
# order to minimize bias


#TRAIN KNN clasifier----

# The train() function from the caret package is the workhorse for
# tuning machine leaning classifiers in this workbook. The function sets
# up a grid of tuning parameters, fits each model, and calculates a
# resampling based performance measure. For each training data set,
# the performance of held-out samples is calculated and summary
# statistics are provided. The final model with the optimal resampling
# statistic is selected
# For training the KNN classifier, we specify the train() function as
# follows. First, we define “cl1984*.,” which denotes a formula for
# using all attributes in our classifier, and “cl1984” is the response
# (target) variable. The “data” contains the predictor variables (in this
#                                     “kknn”, while the tune control (trControl) is specified as “fitControl”
# (that we have previously defined in step 9). All results will be saved
# in the object “knnFit”. Generally, it is recommended to center and
# scale all predictor variables prior to executing the KNN model in
# order to reduce bias among the predictor variables during distance
# computation (Kuhn and Johnson 2016). We will start to train the
# KNN classifier as shown below   case, Landsat 5 TM reflectance values). 
# The “method” is defined as“kknn”, while the tune control (trControl) is specified as “fitControl”
# (that we have previously defined in step 9). All results will be saved
# in the object “knnFit”. Generally, it is recommended to center and
# scale all predictor variables prior to executing the KNN model in
# order to reduce bias among the predictor variables during distance
# computation (Kuhn and Johnson 2016). We will start to train the
# KNN classifier as shown below

set.seed(hre_seed) 
training$Cl1984=factor(training$Cl1984)
knnFit =caret::train(Cl1984~., data=training,
              method="kknn",
              preProcess=c("center","scale"),
              trControl=fitControl)


#check model results
print(knnFit)

#training perfomance
plot(knnFit) 
# The optimal number of
# neighbors is 5. Notice that there is a slight decrease from “kmax = 5”
# to “kmax = 7”.

#check parameters of the best model
knnFit$finalModel

# display variable importance using the varImp() function
knn_varImp = caret::varImp(knnFit, competes=F)
plot(knn_varImp)


# now let's predict
predKnn = predict(knnFit,newdata=testing)



# Before performing accuracy assessment, we need to be familiar
# with key terms. The confusion matrix (error matrix), which is a
# cross-tabulation of the reference (observed data) and predicted classes
# (classified) is the most common method used for land use/cover
# accuracy assessment.
# The diagonal areas represent land
# use/cover classes that were correctly predicted or classified, while the
# off-diagonals represent classification errors (that is, errors of commission and omission). The columns represent reference land use/
#         cover (derived from ground checks, high resolution aerial photographs or satellite imagery), while the rows represents the predicted
# or classified land use/cover class.

# Let’s consider a case where we have binary land use/cover classification problem (Table 4.2). In this case, we are performing accuracy assessment for agriculture (Agr) and non-agriculture
# (Non-agr) classes . True positive (TP) quantifies the
# number of correctly allocated positives, that is the correct agriculture
# class. The true negative (TN) quantifies the number of correctly
# allocated non-agriculture class. The false positive (FP) quantifies the
# number of non-agriculture pixels classified as agriculture (error of commission), while the false negative (FN) quantifies the number of
# agriculture pixels classified as non-agriculture (error of ommission).


# You will notice that the confusion matrix produces several accuracy
# assessment metrics. In this workbook, we will focus on accuracy
# assessment metrics commonly used in remote sensing (Foody 2010)
# such as the overall accuracy, producer’s accuracy (sensitivity and
#                                                    specificity) and user’s accuracy (positive prediction value and negative
#                                                                                      prediction value). The overall accuracy is the proportion of correctly
# classified land use/cover samples. The producer’s accuracy is the land
# use/cover map accuracy from the point of view of the map maker (the
#                                                                 producer). In other words, the producer’s accuracy determines how well
# the classification model detects a given class. The user’s accuracy is the
# accuracy from the point of view of a map user. That is, the user’s
# accuracy informs us how often the class on the land use/cover map will
# actually be present on the ground. Note that, we will ignore the kappa
# coefficient kappa because it is highly correlated with overall accuracy
# and therefore, redundant 

# CONFUSION MATRIX--------------
# Next, let’s check the accuracy metrics for the KNN model using the
# confusionMatrix() function from the caret package in R.
confusionMatrix(data=predKnn, factor(testing$Cl1984) )


# Let’s examine the agriculture class as an example. The confusion
# matrix results show that 359 pixels were correctly classified as agriculture. However, 107 bareland (Br), 17 green space (GS) and 9
# settlement (Set) pixels were wrongly classified as agriculture. These
# 131 misclassified pixels represent the false positives or errors of
# commission. In contrast, 112 bareland (Br), 18 green space (GS), 19settlement (Set) and 1 water (Wt) pixels were incorrectly classified as
# non-agriculture. These 150 misclassified pixels represent the false
# negatives or errors of omission.
# Next, let’s take a look at the overall statistic results. The overall
# accuracy is 72.3%, while the “No Information Rate” is about 41%.
# The “No Information Rate” is the best guess given no information
# beyond the overall distribution of the classes that are being classified.
# Therefore, the KNN classifier is better than the best guess model.
# Next, let’s examine classification accuracy performance of each
# land use/cover class. First, we examine the producer’s accuracy in
# terms of sensitivity and specificity. The sensitivity for the agriculture
# (agr) class is 73%, while the specificity is 86%. This means that the
# KNN classifier correctly identified the agriculture (Agr) class 73% of
# the time. However, KNN classifier correctly identified non-agriculture
# (non-agr) classes (that is bareland, green spaces, settlement and water)
# 86% of the time. This pattern is also observed for the bareland, green
# spaces, settlement and water classes. Therefore, the KNN classifier is
# better at detecting the true negative rate rather than the true positive
# rate.
# Second, we examine the user’s accuracy in terms of the positive
# predicted values (Pos Pred Value) and negative predicted values (Neg
#                                                                  Pred Value). The Pos Pred Value for the agriculture (agri) class is
# 71%, while the Neg Pred Value is 87%. This means that from the map
# user’s perspective, the KNN classifier correctly identified agriculture
# (Agr) 71% of the time. However, the KNN classifier correctly identified non-agriculture (non-agr) classes (that is bareland, green spaces,
#                                                                                                            settlement and water) 87% of the time. Again, the KNN classifier is
# also better at classifying the non-agriculture classes.
# The accuracy assessment results highlight important insights on
# land use/cover classification. For example, the confusion matrix
# reveals significant spectral confusion between agriculture and bareland classes. This is not surprising given the high quantity of outliers
# observed in Fig. 4.3. Furthermore, the KNN is biased towards high
# specificity and negative predicted values. As a result, the KNN model
# is poor at detecting the true positive classes. This information can be
# used to improve land use/cover classification.


# ANN CLASSIFIER----------


# Next, let’s train the ANN classifier. In R, I use the nnet (neural
# network) package since it is easy to adjust the weight decay.
# However, the nnet package is generally restricted to one hidden layer,
# which is a limitation. Here, we use “size” and “decay” as the primary
# model tuning parameters. The parameter “size” refers to the number
# of nodes in the hidden layer, while “decay” refers to the size of the
# weight decay. Remember, the ANN model has a tendency to overfit.
# As a result, a weight decay—which is a penalization method for
# regularizing or controlling the model—is used to counter overfitting.
# For training the ANN model, we specify the train() method as we
# have done in step 10. We define the response (target) variable as
# “cl1984*.,” the “data” as training, and tune control (“trControl”) as
# “fitControl”. Here, we define the “method” as “nnet” since we are
# running a neural network classifier. All results will be saved in the
# object “annFit”. We will start to train the ANN classifier as shown
# below.


set.seed(hre_seed)
training$Cl1984=factor(training$Cl1984)
annFit = train(Cl1984 ~., data = training,
               method="nnet",
               preProcess = c("center","scale"),
               trControl =fitControl)


# As I mentioned before, the ANN classifier is a connection of input/
# output nodes whereby each connection has a weight associated with
# it. Therefore, the network learns by adjusting the weights in order to
# predict or identify the correct land use/cover class during training.
# The following is part of the program output, which shows progress
# messages that are generated automatically while the ANN is being
# trained.

print(annFit)

# The ANN classifier results show that 6,195 training samples were
# used for training. Six are predictors (that is, six Landsat 5 TM bands),
# and five land use/cover classes represent the response (target) variable. In addition, pre-processing was done. The cross-validation
# results also provide the sample sizes and the final values that were
# selected for the best model. The “size” parameter specifies the number
# of hidden processing nodes used in the neural network.
# Next, let’s display the model training performance based on overall
# accuracy

plot(annFit)

# shows the weight decay values that were evaluated with
# a single hidden layer with sizes ranging between 1 and 5 hidden
# nodes. The optimal ANN model has 5 hidden nodes and a weight
# decay of 0.1.

# parameters of the best model
annFit$finalModel


# We trained a network with 6 variables as input data (that is, the
# Landsat bands), 5 nodes in the hidden layer and five classes in the
# output layer. In total, 65 weights which are constants that define the
# neural network are shown in the output. That is, if a neural network
# has n inputs, h hidden nodes and o outputs, it will have (n * h) +
# h + (h * o) + o weights. In our case, our final neural network has six
# inputs, five hidden nodes, and five outputs. 
# Therefore, (6 *5) + 5 + (5 * 5) + 5 will give 65 weights.

#we can plot the net
plotnet(annFit$finalModel)


# importance of variables
olden(annFit)
# The relative importance of the predictor variables for the ANN
# model is computed as the sum of the product of raw input-hidden,
# hidden-output connection weights (Olden et al. 2004). Figure 4.10
# shows that band 5, 4 and 7 are the most important predictor variables.
# For the ANN model, the analysis of the variable importance provides
# important insights. First, band 5 has the greatest contribution because
# it is better at separating land use/cover classes in the test site (see
# Fig. 4.1). Second, we can observe that the negative contribution of
# bands 1, 2 and 3 is related to high between predictor correlations.
# Remember, the correlation matrix indicated that bands 1 and 2, and
# bands 1 and 3 are highly correlated (see Fig. 4.5a–c).
# Next, let’s perform accuracy assessment. First, we will use the
# ANN model for prediction and then after build a confusion matrix as
# shown in the commands below. 


predAnn = predict(annFit, newdata=testing)
confusionMatrix(predAnn, factor(testing$Cl1984))

# In general, the confusion matrix shows high misclassification
# errors. The overall accuracy is relatively poor. Generally, the KNN
# classifier has better accuracy than the ANN classifier.
# In terms of the producer’s accuracy, sensitivity for the agriculture
# (agr) class is 50%, while the specificity is 82%. This means that the
# ANN classifier correctly identified agriculture (Agr) 50% of the time,
# while 82% of the time, the ANN classifier correctly identified
# non-agriculture (non-agr) classes (that is bareland, green spaces,
#                                    settlement and water). However, the sensitivity and specificity for the
# bareland is much closer. It also interesting to note that sensitivity is
# lower than specificity for the green space class. The positive predicted
# values (Pos Pred Value) and negative predicted values (Neg Pred
#                                                        Value) shows a similar pattern for all land use/cover classes except
# water class. Generally, the positive predicted values (Pos Pred Value)
# for all land use/cover classes except the water class are lower than the
# negative predicted values (Neg Pred Value). This means that from the
# map user’s perspective, the ANN classifier is poor at identifying the
# positive land use/cover classes.
# The ANN classifier is also better at detecting the true negative
# classes. A closer look at the confusion matrix shows high misclassification errors, which suggests significant spectral confusion
# between the agriculture and bareland classes. As a result, the ANN
# classifier has difficulty to separate agriculture and bareland areas.
# Here, we see that poor performance of the ANN model is due to high
# between predictor correlations. Be aware that the ANN model is
# sensitive to high predictor collinearity


#SINGLE DECISION TREE----------
# Next, we are going to train a single DT classifier using the CART
# algorithm available in rpart package. The single DT is trained as
# follows. First a single variable that best splits the training data into
# two groups is selected. After that, the process is applied separately to
# each sub-group recursively until the sub-groups reach a minimum
# size. The second stage of the procedure consists of using
# cross-validation to prune back the full tree. Here, a “complexity
# parameter”, is defined as the primary tuning parameters. The complexity parameter (cp) is used to control the size of the decision tree
# and to select the optimal tree size.
# For training a single DT classifier, we specify the train() method as
# we have done before. We define the response (target) variable as
# “cl1984*.,” the “data” as training, and tune control (“trControl”) as
# “fitControl”. Here, we specify the “method” as “rpart” since we are
# running a single decision tree classifier. All results will be saved in
# the object “cart_model”


set.seed(hre_seed)
cart_model = train(factor(Cl1984)~., data=training,
                   method="rpart",
                   trControl=fitControl)
print(cart_model)
plot(cart_model)


# The results show that 6,195 training samples were used for training. We also have six Landsat 5 TM bands and five land use/cover
# classes. Note that pre-processing was not done since the single DT
# does not require it. The cross-validation results also give us the
# sample sizes and the final values that were selected for the best model.
# The optimal model has cp value of about 0.026 and an accuracy of
# 52%, which is relatively low 

# check parameters
cart_model$finalModel


# From the output, “n = 6195” represents the total training dataset,
# while the “node, split, n, loss, yval, (yprob)” shows the decision tree
# legend. The “node” represents the node number, while “split” indicates the feature which was split on. The “n” shows the number of
# observations, while “loss” shows misclassifications. Finally, “yval”
# shows the class predictions when processing terminates, while
# “yprob” shows the class probabilities. As indicated in the results
# output above “*” denotes terminal node or leaf. The results show that
# the single DT has four terminal leaves that correspond to agriculture,
# green spaces, bareland and settlement.
# Next, let’s visualize the tree using rpart.plot() function from the
# rpart.plot package
#install.packages("rpart.plot")

library(rpart.plot)
rpart.plot(cart_model$finalModel)

# The tree is a set
# of binary decisions and terminal nodes connected by branches. The
# root (first decision) node evaluates the rule as follows. If band 1
# (CostzPr_22_6_84.1) is less than the reflectance value of 0.074 and
# band 7 (CostzPr_22_6_84.6) is greater than or equal to reflectance
# value 0.2, then a terminal node is reached. The nodes are assigned to
# the agriculture (agr) and green space (GS) classes. If band 1
# (CostzPr_22_6_84.1) is greater than the reflectance value of 0.074,
# then another binary decision node is evaluated and the tree continues
# to grow until all branches end with terminal nodes. Note that the DT
# classifier has completely left out the water class, which indicates
# classification problems. This is attributed to a number of problems.
# First, the water class has a significantly small size of only 64 samples
# from a training sample size of 6,195 (see summary statistics in step 7).
# However, single DT requires large training samples for accurate tree
# classification. Second, the stability of trees is affected by outliers. As
# observed before (Fig. 4.3), the training dataset has many outliers,
# which greatly affects the performance of the singe DT classifier

# importance of variables
cart_varImp = varImp(competes=FALSE, cart_model)
ggplot(cart_varImp)

# Landsat 5 TM bands 5, 1 and 7 are the most
# informative predictor variables. As shown in Fig. 4.1, band 5 is better
# at separating land use/cover classes in the test site. However, the
# importance of band 1 is questionable here. This implies instability of
# the single DT model.
# Next, let’s perform accuracy assessment. First, we use the model
# for prediction, and then after build a confusion matrix as shown in the
# commands below. Note the predict() function will not work here if
# there are missing values. Therefore use “na.action = na.pass”.

pred_cart = predict(na.action=na.pass, newdata=testing, cart_model)
confusionMatrix(data=pred_cart, factor(testing$Cl1984))

# particular, there is significant confusion between the agriculture and
# bareland classes. Generally, the single DT accuracy is very poor. This
# is not surprising given the significant class imbalance. For example,
# green space and water classes have low proportions compared to
# agriculture, bareland and settlement classes. In addition, the single DT
# shows significant instability given the presence of many outliers in
# the training data (Fig. 4.3). Therefore, the single DT is not effective
# for image classification in this case.


#SVM classifier---------------


# Many studies have demonstrated the effectiveness of SVMs for remote sensing image classification (Pal and Mather 2005; Nemmour
#                                                                                                   and Chibani 2006). However, SVMs as other machine learning
# classifiers require proper tuning in order to reduce or eliminate
# problems such as overfitting. This requires the selection of an
# appropriate kernel function and cost parameter. In this workbook, we
# are going to use the radial basis function, which has been successfully
# used for remote sensing image classification. Automatic tuning will
# be done to find the optimal cost parameter. Note that all predictors
# (that is, the Landsat bands) should be centered and scaled before in
# order to reduce the impact of predictor variables with high attribute
# values (Kuhn and Johnson 2016).
# First, let’s train the radial basis SVM classifier.

set.seed(hre_seed)
svm_model=train(factor(Cl1984)~.,data = training,
                method="svmRadial",
                trControl = fitControl,
                preProc = c("center","scale"),
                tuneLength=3)

#check model attributes
print(svm_model)
plot(svm_model)

#check the best model
svm_model$finalModel

# The output shows that only 4,626 support vectors were selected
# from a total of 6,195 training samples

# check importance of variables
svm_varImp =varImp(svm_model, competes = FALSE)
plot(svm_varImp)

# Figure 4.15 shows that all Landsat 5 TM bands expect band 4
# contribute significantly to the settlement class. This is the same pattern we observed with the KNN model variable importance. As we
# observed before, band 4 exhibits significant spectral confusion
# between settlement class, and the agriculture and bareland classes
# (see Fig. 4.1). Nonetheless, Fig. 4.1 also shows that band 7
# (CostzPr_22_6_84.6) exhibit significant spectral confusion between
# settlement class, and the agriculture and bareland classes. Therefore,
# it is difficult to understand how the radial basis SVM classifier judged
# that band 7 has more contribution than band 4. Again, the analysis
# results are a bit misleading taking into consideration the geographical
# characteristics of the test site.
# Next, let’s perform accuracy assessment as we have done in the
# previous steps. First, we use the SVM model results for prediction,
# and then build a confusion matrix as shown in the commands below.

pred_svm = predict(svm_model, newdata=testing)
confusionMatrix(data=pred_svm, factor(testing$Cl1984))


# The overall accuracy is about 65%. The SVM classifier has better
# classification accuracy than the ANN and DT classifiers. However, the
# SVM classifier has lower classification accuracy than the KNN classifier.
# In terms of the producer’s accuracy, sensitivity is lower than
# specificity for all land use/cover classes except the bareland class.
# This suggests that the SVM classifier is better at detecting negative
# classes (non-bareland) than the positive classes (e.g., barelend).
# Generally, the positive predicted values (Pos Pred Value) for all land
# use/cover classes except for the water class are lower than the negative predicted values (Neg Pred Value). This means that from the
# map user’s perspective, the SVM classifier is better at identifying the
# negative rates. In other words, the SVM classifier is better at detecting
# non-agriculture or non-bareland classes.
# The confusion matrix shows significant confusion between the
# agriculture and bareland classes. This indicates that the radial basis
# SVM classifier has difficulty separating the agriculture and bareland
# classes. However, the radial basis SVM classifier is much better at
# handling outliers compared to the single DT and the ANN classifiers.


# RANDOM FOREST CLASSIFIER--------
# Random forests have also been used successfully for remote sensing
# image classification (Lu and Weng 2007; Mellor et al. 2013). For RF
# models, the most important tuning parameter is mtry, which represents the number of randomly selected predictors (k) to choose from
# each split.
# Let’s start to train the RF model.

#install package randomForest
# package_url = "https://cran.r-project.org"
#  require(devtools)
#  install_version("randomForest", version = "4.6.12", repos = package_url)

library(randomForest)

set.seed(hre_seed)
rf_model = train(factor(Cl1984)~.,
                 data=training,
                 method="rf",
                 trControl = fitControl,
                 prox=TRUE,
                 fitBest=FALSE,
                 returnData=TRUE)
# check attributes
print(rf_model)
plot(rf_model)

# The results show that 6,195 training samples were used for training. The six predictor variables are the Landsat 5 TM bands, while the
# five land use/cover classes represent the response (target) variable.
# Note that pre-processing was not done since the RF classifier requires
# minimum pre-processing. The RF model was tuned over the values of
# the mtry parameter. The best model had an mtry value of 2 with an
# overall accuracy of 74%, which is relatively good 


# check the best model
rf_model$finalModel

# 
# The output shows a confusion matrix for the best model (after
#cross-validation). A total of 500 decision trees were used in the RF
# model. From the six predictor variables (six Landsat 5 TM bands),
# only two predictor variables (bands) were selected at each split. The
# out-of-bag (OOB) estimate of error rate is 24.6%

# variable importance
rf_varImp = varImp(rf_model,competes=F)
plot(rf_varImp)


# the relative importance of the contribution of
# the six Landsat 5 TM bands. Band 5 has great contributions followed
# by bands 4 and 7. The results provide valuable insight of the RF
# model performance. For example, Fig. 4.1 shows that band 5 was
# better at separating land use/cover classes. In this case, the RF correctly managed to identify band 5 as the most informative predictor
# variable.


#accuracy assesment
pred_rf = predict(rf_model$finalModel,
                  newdata=testing)
confusionMatrix(data = pred_rf, factor(testing$Cl1984))

# The overall accuracy of the RF classifier is 76%, which is better
# than the KNN, ANN, DT and SVM classifiers. The RF classifier also
# performs better in terms of individual class performance. The producer’s accuracy (sensitivity and specificity) and the user’s accuracy
# (positive predicted values and negative predicted values) are relatively better for all land use/cover classes. In addition, the RF classifier is better at handling outliers than the other machine learning
# classifiers used in this workbook. While accuracy assessment results
# are relatively good, misclassification problems are still apparent. For
# example, 103 bareland pixels were misclassified as agriculture. This
# means there is still need to improve land use/cover classification
# accuracy further



# COMPARE ALL RESULTS OF ML CLASSIFIERES-----

# Let’s proceed to compare all machine learning classifiers based on the
# cross-validation statistics. Here, we are going to use the resamples()
# function because the machine learning classifiers share a common set
# of resampled data sets. First, we run the resamples(), and then check
# the resamps object as shown in the command below

resamps =resamples(list(knn=knnFit,
                        nnet=annFit,
                        rpart=cart_model,
                        kernlab=svm_model,
                        e1071 = rf_model))
resamps

#summary stats
summary(resamps)
bwplot(resamps, layout=c(3,1))


# The summary statistics and Fig. 4.18 show that the performance of
# the machine learning classifiers varies greatly. Clearly, the RF classifier has the highest performance, while the single DT tree (rpart)
# classifier has the lowest performance. However, it should be noted
# that it took more time to tune the RF classifier. Interestingly, the
# simple KNN classifier performed better than the advanced radial basis
# SVM classifier. This shows that it is better to start training simple
# machine learning classifiers, which are less computationally
# intensive.

#PERFORM CLASSIFICATION -----

timeStart =proc.time()
LC_knnFit_84 <-predict(rasVars,knnFit)
LC_ann_84 <-predict(rasVars,annFit)
LC_cart_84 <-predict(rasVars,cart_model)
LC_svm_84 <-predict(rasVars,svm_model)
LC_rf_84 <-predict(rasVars,rf_model)
proc.time() - timeStart

# PLOT----
LC_ann_84a <- gplot(LC_ann_84) + geom_raster(aes(fill= factor(value, labels=c("Agriculture", "Bareland", "
Green Spaces", "Urban", "Water")))) + 
        scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"),name= "Land Cover") +
        ggtitle("Artificial Neural Network Classification") + 
        theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()

LC_ann_84a

LC_cart_84a <- gplot(LC_cart_84) + geom_raster(aes(fill = factor(value, labels=c("Agriculture", "Bareland",
"Green Spaces", "Urban")))) + scale_fill_manual(values= c("yellow", "grey", "green3", "red"), name= "Land Cover") + 
        ggtitle("Decision Trees Classification") +theme(plot.title = element_text(lineheight=.4, face="bold"))
+ coord_equal()
LC_cart_84a

LC_svm_84a <- gplot(LC_svm_84) + geom_raster(aes(fill= factor(value, labels=c("Agriculture", "Bareland","Green Spaces", "Urban", "Water")))) +scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"),name= "Land Cover") + 
        ggtitle("Support Vector Machine Classification") +
        theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()
LC_svm_84a

LC_rf_84a <- gplot(LC_rf_84) + geom_raster(aes(fill =factor(value, labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"),
name= "Land Cover") +
        ggtitle("Random Forest Classification") +theme(plot.title = element_text(lineheight=.4,face="bold")) + coord_equal()
LC_rf_84a


LC_knnFit_84a <- gplot(LC_knnFit_84) + geom_raster(aes(fill = factor(value, labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"), name= "Land Cover") + ggtitle("K Nearest Neighbour Classification") +
        theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()
LC_knnFit_84a 
grid.arrange(LC_knnFit_84a, LC_ann_84a, LC_cart_84a,
             LC_svm_84a, LC_rf_84a, ncol=2)


# The RF classifier outperformed the KNN, ANN, DT and radial basis
# SVM classifiers. In addition, the RF classifier was not strongly
# affected by outliers and class imbalances problems. Therefore, the RF
# classifier will be used for image classification in tutorial 2, and
# Chap. 5.

# Single DT can't recognize Water class!





