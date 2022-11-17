# install.packages("rgdal")
 # install.packages("RStoolbox")
# install.packages("gridExtra")

library("sp")
library("raster")
library("rgdal")
library("ggplot2")
library("gridExtra")
library("RStoolbox")
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
getwd()

#load meta data
metaData = readMeta("LT51700721984174XXX08_MTL.txt") #readMeta(file; raw = TRUE)

# read attributes from meta data of Landsat 5
summary(metaData)

# Create an object“TM522_6_84” and stack the data. Loads Geotiffs into a
# single rasterStack
TM522_6_84 = stackMeta(metaData)

#inspect the object
class(TM522_6_84)
TM522_6_84
dataType(TM522_6_84)[[1]]
# The output shows that data type is an integer. Keep this in mind
# because we will need to compare the original Landsat 5 TM imagery
# with the radiometric-corrected Landsat 5 TM imagery later.

# Display the imagery
#firstly, we remove thermal band 6
TM522_6_84 = dropLayer(TM522_6_84, c(6))
TM522_6_84

# plot with spplot each band
spplot(TM522_6_84,col.regions = rainbow(99, start=.1), main="Bands")

# multispectral imagery with plotRGB()
# The plotRGB() function can be used to create
# “true” or “false” color images from Landsat imagery and other
# satellite imagery which is based on 3 layers(RasterBrick or RasterStack)
plotRGB(TM522_6_84, r=5,g=4,b=3,stretch="lin")


# histograms and density plot
timeStart = proc.time()
B1_dn <- ggplot(TM522_6_84, aes(B1_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B2_dn <- ggplot(TM522_6_84, aes(B2_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B3_dn <- ggplot(TM522_6_84, aes(B3_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B4_dn <- ggplot(TM522_6_84, aes(B4_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B5_dn <- ggplot(TM522_6_84, aes(B5_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B7_dn <- ggplot(TM522_6_84, aes(B7_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
proc.time() - timeStart # user time and system time.
grid.arrange(B1_dn, B2_dn, B3_dn, B4_dn, B5_dn, B7_dn,
             ncol = 1)


# The histogram and density plot are important graphic representations, which are useful for exploring the distribution of digital
# number values (e.g., normal distribution, negative or positively
#                skewed distribution, multimodal or uniform distribution), contrast
# (high or low) as well as identifying bad data values in the remotely
# sensed imagery. Therefore, it is important to check the histogram
# and density plot of the remotely-sensed data given that most statistical tests used for image analysis assume that the digital values
# recorded in the scene are normally distributed, which is not always
# true (Jensen 2005). 



# Radiometric Correction and Reprojection

# check the conversion parameters (gain and offset)
TM5_radParameters = metaData$CALRAD
TM5_radParameters

# convert landsat 5 tm dn to top-of-the-atmosphere radiance
# imagery DN to top-of-the -atmosphere radiance -> radiometric calibration and
# correction
rad_22_6_84 = radCor(TM522_6_84, metaData = metaData, method = "rad")

# check attributes of the top-of-the-atmosphere radiance imagery
rad_22_6_84

dataType(rad_22_6_84)[[1]]

# step 4 visualization
timeStart = proc.time()
B1_rad <- ggplot(rad_22_6_84, aes(B1_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B2_rad <- ggplot(rad_22_6_84, aes(B2_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B3_rad<- ggplot(rad_22_6_84, aes(B3_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B4_rad<- ggplot(rad_22_6_84, aes(B4_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B5_rad<- ggplot(rad_22_6_84, aes(B5_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B7_rad<- ggplot(rad_22_6_84, aes(B7_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
proc.time() - timeStart # user time and system time.
grid.arrange(B1_rad, B2_rad, B3_rad, B4_rad, B5_rad, B7_rad,
             ncol = 1)
# convert the top-of-the-atmosphere radiance to 
# the top-of-the-atmosphere reflectence = apparent reflectence,
# which takes into account temporal changes in solar illumination 
# due to Earth-Sun geometry
# Reflectance is a unitless or
# dimensionless ratio of the radiance emittance of an object and irradiance
# Radiance emittance refers to the
# energy that flows away from the surface (e.g., thermal energy emitted
# by the Earth), while irradiance is the radiant energy that falls on a
# surface (Mather and Koch 2011). Following are steps to convert
# Landsat 5 TM imagery DNs to top-of-the atmosphere reflectance.

apref_22_6_84 = radCor(TM522_6_84, metaData = metaData, method = "apref")
# check attributes reflectence values
apref_22_6_84

# visualization
plotRGB(apref_22_6_84, r=5, g=4, b=3, stretch="lin")



# histograms and density plots
B1_apref = ggplot(apref_22_6_84, aes(B1_tre))+
  geom_histogram(aes(y=..density..))+geom_density()
B2_apref = ggplot(apref_22_6_84, aes(B2_tre))+
  geom_histogram(aes(y=..density..))+geom_density()
B3_apref = ggplot(apref_22_6_84, aes(B3_tre))+
  geom_histogram(aes(y=..density..))+geom_density()
B4_apref = ggplot(apref_22_6_84, aes(B4_tre))+
  geom_histogram(aes(y=..density..))+geom_density()
grid.arrange(B1_apref, B2_apref, B3_apref,B4_apref)


# Simple dark object subtraction (SDOS)
# Generally, it is assumed that any radiation originating from dark
# pixels (that is, reflectance close to zero) in the image is due to
# atmospheric haze and not the reflectance of the surface. Therefore,
# haze values are estimates of path radiance.
# Note that SDOS requires the estimation of haze values in the
# visible and NIR bands. This is because atmospheric haze affects
# mostly the visible wavelength range (blue, green and red bands). 

# haze correction
# First, perform automatic haze estimation using the estimateHaze()
# function, which estimates the DN pixel value of dark objects for the
# visible and NIR bands.

hazeDN = estimateHaze(TM522_6_84, hazeBands = 1:4,
                      darkProp = 0.01, plot = T)


# radiometric correction using the SDOS method.
# Note that the “starting haze value” (SHV) 

sdos_22_6_84 = radCor(TM522_6_84, metaData = metaData,
                      method = "sdos", hazeValues = hazeDN,
                      hazeBands = 1:4)
# check
sdos_22_6_84

# visualization
plotRGB(sdos_22_6_84, r=5, g=4, b=3, stretch="lin")


# histograms
B1_sdos = ggplot(apref_22_6_84, aes(B1_tre))+
  geom_histogram(aes(y=..density..))+geom_density()
B2_sdos = ggplot(apref_22_6_84, aes(B2_tre))+
  geom_histogram(aes(y=..density..))+geom_density()
B3_sdos = ggplot(apref_22_6_84, aes(B3_tre))+
  geom_histogram(aes(y=..density..))+geom_density()
B4_sdos = ggplot(apref_22_6_84, aes(B4_tre))+
  geom_histogram(aes(y=..density..))+geom_density()
grid.arrange(B1_sdos, B2_sdos, B3_sdos,B4_sdos)

# DARK OBJECT SUBTRACTION (DOS)
# perform atmospheric correction using
# the DOS model (Chavez decay model)

dos_22_6_84 = radCor(TM522_6_84, metaData = metaData,
                     method = "dos")

#check the attributes of the DOS corrected LANDSAT imagery
dos_22_6_84


# visualization
plotRGB(dos_22_6_84, r=5, g=4, b=3, stretch="lin")

#histograms
B1_dos = ggplot(dos_22_6_84, aes(B1_sre))+
  geom_histogram(aes(y=..density..))+geom_density()
B2_dos = ggplot(dos_22_6_84, aes(B2_sre))+
  geom_histogram(aes(y=..density..))+geom_density()
B3_dos = ggplot(dos_22_6_84, aes(B3_sre))+
  geom_histogram(aes(y=..density..))+geom_density()
B4_dos = ggplot(dos_22_6_84, aes(B4_sre))+
  geom_histogram(aes(y=..density..))+geom_density()
grid.arrange(B1_dos, B2_dos, B3_dos,B4_dos)


# COSINE ESTIMATION OF ATMOSPHERIC TRANSMITTANCE (COST)

# PERFORM COST RADIOMETRIC CORRECTION
hazeDN= estimateHaze(TM522_6_84, hazeBands = 1:4,
                     darkProp = 0.01, plot = T)

# radiometric correction
costz_22_6_84 = radCor(TM522_6_84, metaData = metaData,
                       method = "costz",hazeValues = hazeDN,
                       hazeBands = 1:4)
costz_22_6_84

# visualization
plotRGB(costz_22_6_84, r=5,g=4, b=3,
        stretch="lin")
#histograms
B1_costz = ggplot(costz_22_6_84, aes(B1_sre))+
  geom_histogram(aes(y=..density..))+geom_density()
B2_costz = ggplot(costz_22_6_84, aes(B2_sre))+
  geom_histogram(aes(y=..density..))+geom_density()
B3_costz = ggplot(costz_22_6_84, aes(B3_sre))+
  geom_histogram(aes(y=..density..))+geom_density()
B4_costz = ggplot(costz_22_6_84, aes(B4_sre))+
  geom_histogram(aes(y=..density..))+geom_density()
grid.arrange(B1_costz, B2_costz, B3_costz,B4_costz)


# Summary of radiometric and atmospheric correction methods
grid.arrange(B1_apref, B1_sdos, B1_dos, B1_costz)
grid.arrange(B2_apref, B2_sdos, B2_dos, B2_costz)
grid.arrange(B3_apref, B3_sdos, B3_dos, B3_costz)
grid.arrange(B4_apref, B4_sdos, B4_dos, B4_costz)

# Note that most of the
# scattering is corrected in blue and green bands, and slightly in the red
# band. The COST-corrected Landsat image will be used for image
# processing and classification since the COST model accounts for
# multiplicative atmospheric scattering and absorption effects.



# REPROJECTION OF THE COST-CORRECTED LANDSAT IMAGERY
# FROM WGS84 UTM ZONE36 N TO WGS84 UTM ZONE 36 S

# define projection parameters
newproj= "+proj=utm +zone=36 +south +datum=WGS84
+units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


# perform reprojection
costz_22_6_84pr = projectRaster(costz_22_6_84, crs=newproj,
                                res=30)
#check properties
costz_22_6_84pr

# dipsplay "+proj=utm +zone=36 +south +datum=WGS84
# +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

plotRGB(TM522_6_84, r=5, g=4, b=3, stretch="lin")



# Next, create a subset of the reprojected COST-corrected Landsat 5
# TM imagery that corresponds to the test site. First, import the
# boundary shapefile.
#clip
clip_boundary =readOGR(getwd(), "Hre_Boundary")

#create a clip function
clip=function(raster, shape){
  
  a1_crop = crop(raster,shape)
  step1 = rasterize(shape, a1_crop)
  a1_crop *step1
}

# clip the obj

costz22_6_84 =clip(costz_22_6_84pr, clip_boundary)

#check attributes
costz22_6_84

#display the subset
plotRGB(costz22_6_84, r =5, g=4, b=3, stretch="lin")

#save cost corrected imagery
writeRaster(costz22_6_84,"clippedcostproj.img",datatype='FLT4S', overwrite = TRUE)
