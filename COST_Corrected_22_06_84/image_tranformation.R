library("sp")
library("raster")
library("rgdal")
library("ggplot2")
library("gridExtra")
library("RStoolbox")
library(RColorBrewer)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
getwd()

# create a list of import raster layers or bands
rlist = list.files(getwd(), pattern="img$", full.names = TRUE)
# combine and stack layers
costz22_6_84 = stack(rlist)

# check attributes
costz22_6_84

# dipslay img
plotRGB(costz22_6_84, r =5, g=4,b=3, stretch="lin")

# Compute selected vegetation indices
# NDVI SADVI MSAVI

VI_06_84 <- spectralIndices(costz22_6_84,
                            red = "Layer_3",
                            nir = "Layer_4",
                            indices = c("NDVI", "SAVI", "MSAVI"))
VI_06_84
