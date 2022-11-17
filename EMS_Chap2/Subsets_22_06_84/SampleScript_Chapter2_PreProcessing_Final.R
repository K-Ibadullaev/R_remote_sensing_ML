# Sample secript for chapter 2: Pre-processing

# Tutorial 1: Display Landsat 5 TM Image
# Objectives: 
#   Import and display Landsat 5 TM image acquired on 22 June 1984; and 
#   Explore Landsat 5 TM image statistics

# Intsall the required packages by typing the following commands
install.packages("sp")
install.packages("rgdal")
install.packages("raster")
install.packages("RStoolbox")
install.packages("ggplot2")
install.packages("gridExtra")

# Load the packages 
library(sp)
library(rgdal)
library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)

# Setup your working directory. My working directory is specified as follows:
setwd("C:\\Users\\039011\\Desktop\\Practical Machine Learning\\Manuscript\\2019\\Data_SampleScripts\\Chapter2\\Tutorials_1_2\\Subsets_22_06_84")

# Import and load the Landsat 5 TM scene
metaData<- readMeta("LT51700721984174XXX08_MTL.txt")

# Check the attributes of the Landsat image files
summary(metaData)

# Use the stackMeta() function reads Landsat MTL or XML metadata files and combines single Landsat images into a raster stack.
TM522_6_84 <- stackMeta(metaData)

# Check the attributes of the spatial object.
TM522_6_84

# Check the dat type of the spatial object.
dataType(TM522_6_84)[[1]]

# Remove band 6 (the thermal band) using the dropLayer() function
TM5_22_6_84 <- dropLayer(TM522_6_84, c(6))

# Check the attributes of the orginal Landsat 5 TM raster stack.
TM5_22_6_84

# Display the six original Landsat 5 TM imagery bands using the spplot function from the sp package.
spplot(TM5_22_6_84, col.regions = rainbow(99, start=.1))

# Display multispectral Landsat 5 TM image using the plotRGB() function.
plotRGB(TM5_22_6_84, r=5, g=4, b=3, stretch="lin")

# Plot the histogram and density plots of the six original Landsat 5 TM imagery bands
timeStart<- proc.time() # allows to measure measure computation time
B1_dn <- ggplot(TM5_22_6_84, aes(B1_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B2_dn <- ggplot(TM5_22_6_84, aes(B2_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B3_dn <- ggplot(TM5_22_6_84, aes(B3_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B4_dn <- ggplot(TM5_22_6_84, aes(B4_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B5_dn <- ggplot(TM5_22_6_84, aes(B5_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
B7_dn <- ggplot(TM5_22_6_84, aes(B7_dn)) +geom_histogram(aes(y = ..density..)) + geom_density()
proc.time() - timeStart # user time and system time.

grid.arrange(B1_dn, B2_dn, B3_dn, B4_dn, B5_dn, B7_dn, ncol = 1)

##########################################################################################################################
# Tutorial 2: Radiometric correction and Reprojection
# Objectives:
#  Perform radiometric correction for the Landsat 5 TM imagery acquired on 22 June 1984
#  Explore statistics for the radiometric and atmospheric corrected Landsat 5 TM imagery
#  Reproject the COST-corrected  Landsat 5 TM imagery

## Note that we are using the spatial obejct "TM5_22_6_84", which we created in tutorial!!! 

# Tutorial 2A. Radiometric correction
# Before we perform radiometric correction , we need to check the conversion parameters (gain and offset).
# These are available in the metadata.
TM5_radParameters <- metaData$CALRAD
TM5_radParameters

# Convert Landsat 5 TM image digital numbers to top-of-the-atmosphere radiance
rad_22_6_84 <- radCor(TM5_22_6_84, metaData = metaData, method = "rad")

# Check the attributes of the top-of-the-atmosphere radiance corrected Landsat 5 TM imagery. 
rad_22_6_84

# Check the data type of the top-of-the-atmosphere radiance corrected Landsat 5 TM imagery. 
dataType(rad_22_6_84)[[1]]

# Plot the histogram and density plots of the six radiometric corrected Landsat 5 TM imagery
timeStart<- proc.time() # allows to measure measure computation time
B1_rad <- ggplot(rad_22_6_84, aes(B1_tra)) +geom_histogram(aes(y = ..density..)) + geom_density() 
B2_rad <- ggplot(rad_22_6_84, aes(B2_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B3_rad <- ggplot(rad_22_6_84, aes(B3_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B4_rad <- ggplot(rad_22_6_84, aes(B4_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B5_rad <- ggplot(rad_22_6_84, aes(B5_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()
B7_rad <- ggplot(rad_22_6_84, aes(B7_tra)) +geom_histogram(aes(y = ..density..)) + geom_density()

proc.time() - timeStart # user time and system time.

# Arrange and diplay the histogram and density plots for bands 1, 2, 3, 4, 5 and 7.
grid.arrange(B1_rad, B2_rad, B3_rad, B4_rad, B5_rad, B7_rad, ncol = 1)

#########################################################################################################################
# Convert Landsat 5 TM image digital numbers to top-of-the-atmosphere radiance.
apref_22_6_84 <- radCor(TM5_22_6_84, metaData = metaData, method = "apref")

# Check the atttributes of the apparent reflectance-corrected Landsat 5 TM imagery. 
apref_22_6_84 

# Display multispectral the image using the plotRGB() function.
plotRGB(apref_22_6_84, r=5, g=4, b=3, stretch="lin")

# Plot the histogram and density plots of the top-of-the-atmosphere reflectance/ apparent reflectance corrected Landsat 5 TM bands.
B1_apref <- ggplot(apref_22_6_84, aes(B1_tre)) +geom_histogram(aes(y = ..density..)) + geom_density() 
B2_apref <- ggplot(apref_22_6_84, aes(B2_tre)) +geom_histogram(aes(y = ..density..)) + geom_density()
B3_apref <- ggplot(apref_22_6_84, aes(B3_tre)) +geom_histogram(aes(y = ..density..)) + geom_density()
B4_apref <- ggplot(apref_22_6_84, aes(B4_tre)) +geom_histogram(aes(y = ..density..)) + geom_density()

# Arrange and diplay the histogram and density plots for bands 1, 2, 3, and 4.
grid.arrange(B1_apref, B2_apref, B3_apref, B4_apref, ncol = 1)

#########################################################################################################################
# Perform radiometric correction using the simple dark object subtraction (sdos, Chavez decay model).

# First, perform aut omatic haze estimation using the estimateHaze function.
hazeDN <- estimateHaze(TM5_22_6_84, hazeBands = 1:4, darkProp = 0.01, plot = TRUE)

# Then perform radiometric correction using the SDOS method.
sdos_22_6_84 <- radCor(TM5_22_6_84, metaData = metaData, method = "sdos", 
                       hazeValues = hazeDN, hazeBands = 1:4)

# Check the attributes of the SDOS-corrected Landsat 5 TM imagery.
sdos_22_6_84

# Display multispectral the image using the plotRGB() function.
plotRGB(sdos_22_6_84, r=5, g=4, b=3, stretch="lin")

# Plot the histogram and density plots of the sdos-corrected Landsat 5 TM imagery.
B1_sdos <- ggplot(sdos_22_6_84, aes(B1_sre)) +geom_histogram(aes(y = ..density..)) + geom_density() 
B2_sdos <- ggplot(sdos_22_6_84, aes(B2_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()
B3_sdos <- ggplot(sdos_22_6_84, aes(B3_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()
B4_sdos <- ggplot(sdos_22_6_84, aes(B4_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()

# Arrange and diplay the histogram and density plots for bands 1, 2, 3, and 4.
grid.arrange(B1_sdos, B2_sdos, B3_sdos, B4_sdos, ncol = 1)

########################################################################################################################
# Perform atmospheric correction using the dark object subtraction (dos, Chavez decay model). 
dos_22_6_84 <- radCor(TM5_22_6_84, metaData = metaData, method = "dos")

# Check the attributes of the DOS-corrected Landsat 5 TM imagery.
dos_22_6_84

# Display multispectral the image using the plotRGB() function.
plotRGB(dos_22_6_84, r=5, g=4, b=3, stretch="lin")

# Plot the histogram and density plots of the sdos-corrected Landsat 5 TM imagery.
B1_dos <- ggplot(dos_22_6_84, aes(B1_sre)) +geom_histogram(aes(y = ..density..)) + geom_density() 
B2_dos <- ggplot(dos_22_6_84, aes(B2_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()
B3_dos <- ggplot(dos_22_6_84, aes(B3_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()
B4_dos <- ggplot(dos_22_6_84, aes(B4_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()

# Arrange the histogram and density plots for bands 1, 2, 3, and 4.  
grid.arrange(B1_dos, B2_dos, B3_dos, B4_dos, ncol = 1)

########################################################################################################################
# Atmospheric correction using the using costz (Chavez decay model).

# First, perform automatic haze estimation using the estimateHaze function.
hazeDN <- estimateHaze(TM5_22_6_84, hazeBands = 1:4, darkProp = 0.01, plot = TRUE)

# Use the costz method.
costz_22_6_84 <- radCor(TM5_22_6_84, metaData = metaData, method = "costz", 
                        hazeValues = hazeDN, hazeBands = 1:4)

# Check the attributes of the costz-corrected Landsat 5 TM imagery
costz_22_6_84

# Display multispectral the image using the plotRGB() function.
plotRGB(costz_22_6_84, r=5, g=4, b=3, stretch="lin")

# plot the histogram and density plots of the COSTZ-corrected Landsat 5 TM imagery. 
B1_costz <- ggplot(costz_22_6_84, aes(B1_sre)) +geom_histogram(aes(y = ..density..)) + geom_density() 
B2_costz <- ggplot(costz_22_6_84, aes(B2_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()
B3_costz <- ggplot(costz_22_6_84, aes(B3_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()
B4_costz <- ggplot(costz_22_6_84, aes(B4_sre)) +geom_histogram(aes(y = ..density..)) + geom_density()

# Arrange the histogram and density plots for bands 1, 2, 3, and 4. 
grid.arrange(B1_costz, B2_costz, B3_costz, B4_costz, ncol = 1)

########################################################################################################################
# Compare the radiometric and atmospheric correction results for each band using the histogram and density plots.

# Display histogram and density plots for bands 1 to 4.
grid.arrange(B1_apref, B1_sdos, B1_dos, B1_costz)
grid.arrange(B2_apref, B2_sdos, B2_dos, B2_costz)
grid.arrange(B3_apref, B3_sdos, B3_dos, B3_costz)
grid.arrange(B4_apref, B4_sdos, B4_dos, B4_costz)

##########################################################################################################################
# Tutorial 2B. Reprojection

# Define the new projection parameters (that is, WGS84 UTM Zone 36 South) as shown below.
newproj <- "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Reproject COST-corrected Landsat5 TM imagery to WGS84 UTM Zone 36 South.
costz_22_6_84pr <- projectRaster(costz_22_6_84, crs=newproj, res=30)

# Check the properties of the reprojected COST-corrected Landsat 5 TM imagery.
costz_22_6_84pr

# Display the reprojected COST-corrected Landsat 5 TM composite.
plotRGB(costz_22_6_84pr, r=5, g=4, b=3, stretch="lin")

# Create a subset of the reprojected COST-corrected Landsat 5 TM composite.
clip_boundary <- readOGR(getwd(), "Hre_Boundary")

# Create a clip function. 
clip<-function(raster,shape) {
  a1_crop<-crop(raster,shape)
  step1<-rasterize(shape,a1_crop)
  a1_crop*step1}

# Clip the reprojected COST-corrected Landsat 5 TM composite using the clip function and the boundary shapefile.
costz22_6_84 = clip(costz_22_6_84pr, clip_boundary) # Subset the image to the Harare boundary

# Check the attributes and dimensions of the clipped and reprojected COST-corrected Landsat 5 TM subset.
costz22_6_84

# Display the single the reprojected COST-corrected Landsat 5 TM subset.
plot(costz22_6_84)

# Display the multispectral composite Landsat 5 TM subset. 
plotRGB(costz22_6_84, r=5, g=4, b=3, stretch="lin")

# Save the reprojected COST-corrected Landsat 5 TM subset.
writeRaster(costz22_6_84, "C:\\Users\\039011\\Desktop\\Practical Machine Learning\\Manuscript\\2019\\Data_SampleScripts\\Chapter3\\COST_Corrected_22_06_84\\CostzPr_22_6_84.img", datatype='FLT4S', overwrite = TRUE)

