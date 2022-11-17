# install.packages("rgdal")
# install.packages("raster")
# install.packages("Rstoolbox")
# install.packages("gridExtra")
library("sp")
library("raster")
library("rgdal")
library("ggplot2")
library("gridExtra")
library(rstudioapi)

# set working dir
setwd(dirname(getActiveDocumentContext()$path))

#load data set
metaData<-readMeta("LT51700721984174XXX08_MTL.txt")