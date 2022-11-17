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
#set layers
VI_06_84 <- spectralIndices(costz22_6_84,
                            red = "Layer_3",
                            nir = "Layer_4",
                            indices = c("NDVI", "SAVI", "MSAVI"))
VI_06_84


# The output shows that all three vegetation indices have different
# value ranges. Next, let’s plot the vegetation indices. First, select a
# color brewer of your choice and then create a color palette. After that,
# display the vegetation indices. Following are the commands.


display.brewer.all()
VI_palette = brewer.pal(n=10,name = "PiYG")

spplot(VI_06_84, col.regions = VI_palette, cuts = 6,
       col="transparent")
# Figure 3.1 shows differences in the vegetation indices. In this
# workbook, we are going to use vegetation indices computed from
# Landsat 5 TM imagery acquired in the dry and wet seasons. The idea
# is to check if the vegetation phenology can help to improve land use/
#   cover classification in the test site.
# Finally, save the individual vegetation indices as shown in the
# command below. We are going to use these vegetation indices for
# land use/cover classification

#save
writeRaster(VI_06_84$NDVI, "NDVI_06_84.img", datatype='FLT4S',
            overwrite = TRUE)
writeRaster(VI_06_84$MSAVI, "MSAVI_06_84.img", datatype='FLT4S',
            overwrite = TRUE)
writeRaster(VI_06_84$SAVI, "SAVI_06_84.img", datatype='FLT4S',
            overwrite = TRUE)



# TEXTURE ANALYSIS
# Compute texture indices from the Landsat 5 TM imagery acquired
# on 22 June 1984
# The glcm package computes image textures derived from
# grey-level co-occurrence matrices (GLCMs) in R.

#install.packages("glcm")
library(glcm)
# list of raster layers
rlist2 = list.files(getwd(), pattern = "img$",
                    full.names = TRUE)
# stack layers
costz22_6_84_2=stack(rlist2)

costz22_6_84_2

# display

plotRGB(costz22_6_84_2, r=5, g=4, b=3,stretch="lin")


#Compute the second order GLCM indices (mean, variance, homogeneity and entropy)
# for Landsat 5 TM band 4 based on a 3 x 3
# window size.



glcm_22_6_84.4 = glcm(raster(costz22_6_84_2, layer=4),
                      window = c(3,3),
                      statistics = c("mean","variance",
                                     "homogeneity",
                                     "entropy"))

#display
plot(glcm_22_6_84.4)

# 
# It is worth noting that texture can be computed using different
# approaches depending on the spatial resolution of the
# remotely-sensed imagery. While texture has been reported to
# improve land use/cover accuracy (Tso and Mather 2001), it is
# very difficult to interpret. This is because texture analysis
# depends on the spatial resolution of the satellite sensor and the
# size of the homogenous area being classified (Tso and Mather
#                                               2001). As a result, we do not intend to analyse the texture
# images. Rather, the intention is to use texture images in order to
# test if they can improve land use/cover classification (in the later
#                                                         chapters). While some terms will be unfamiliar to the reader, I
# encourage the reader to consult the relevant literature listed in
# the reference for more in depth discussion

# Save the glcm texture images for band 4

writeRaster(glcm_22_6_84.4$glcm_mean, "glcm_mean_22_6_84_b4.img", datatype='FLT4S',
            overwrite = TRUE)
writeRaster(glcm_22_6_84.4$glcm_variance, "glcm_variance_22_6_84_b4.img", datatype='FLT4S',
            overwrite = TRUE)
writeRaster(glcm_22_6_84.4$glcm_entropy, "glcm_entropy_22_6_84_b4.img", datatype='FLT4S',
            overwrite = TRUE)

writeRaster(glcm_22_6_84.4$glcm_homogeneity, "glcm_homogeneity_22_6_84_b4.img", datatype='FLT4S',
            overwrite = TRUE)


#Compute the second order GLCM indices (mean, variance, homogeneity and entropy)
# for Landsat 5 TM band 5 based on a 3 x 3
# window size.



glcm_22_6_84.5 = glcm(raster(costz22_6_84_2, layer=5),
                      window = c(3,3),
                      statistics = c("mean","variance",
                                     "homogeneity",
                                     "entropy"))

#display
plot(glcm_22_6_84.5)


# Save the glcm texture images for band 5

writeRaster(glcm_22_6_84.5$glcm_mean, "glcm_mean_22_6_84_b5.img", datatype='FLT4S',
            overwrite = TRUE)
writeRaster(glcm_22_6_84.5$glcm_variance, "glcm_variance_22_6_84_b5.img", datatype='FLT4S',
            overwrite = TRUE)
writeRaster(glcm_22_6_84.5$glcm_entropy, "glcm_entropy_22_6_84_b5.img", datatype='FLT4S',
            overwrite = TRUE)

writeRaster(glcm_22_6_84.5$glcm_homogeneity, "glcm_homogeneity_22_6_84_b5.img", datatype='FLT4S',
            overwrite = TRUE)


#Compute the second order GLCM indices (mean, variance, homogeneity and entropy)
# for Landsat 5 TM band 7 based on a 3 x 3
# window size.



glcm_22_6_84.7 = glcm(raster(costz22_6_84_2, layer=7),
                      window = c(3,3),
                      statistics = c("mean","variance",
                                     "homogeneity",
                                     "entropy"))

#display
plot(glcm_22_6_84.7)


# Save the glcm texture images for band 7

writeRaster(glcm_22_6_84.7$glcm_mean, "glcm_mean_22_6_84_b7.img", datatype='FLT4S',
            overwrite = TRUE)
writeRaster(glcm_22_6_84.7$glcm_variance, "glcm_variance_22_6_84_b7.img", datatype='FLT4S',
            overwrite = TRUE)
writeRaster(glcm_22_6_84.7$glcm_entropy, "glcm_entropy_22_6_84_b7.img", datatype='FLT4S',
            overwrite = TRUE)

writeRaster(glcm_22_6_84.7$glcm_homogeneity, "glcm_homogeneity_22_6_84_b7.img", datatype='FLT4S',
            overwrite = TRUE)



