site     = 'DEJU'
year     = '2021'
domain   = 'D19'
visit    = '4'
wd       = "F:/NEON/Tutorials/R/VegStrCHMR"

## NEON_chm_analysis <- function(site, year, domain, visit, wd = '.')  {

## ----Install_packages, eval=FALSE------------------------------------------------------------------------
##
# install.packages("neonUtilities")
# install.packages("sp")
# install.packages("raster")
# install.packages("devtools")
# devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
# install.packages("gdalUtilities")
# install.packages("dplyr")

## ----Load-packages, results="hide"-----------------------------------------------------------------------

library(sp)
library(raster)
library(neonUtilities)
library(geoNEON)
library(gdalUtilities)
library(rgdal)
## library(dplyr)

##----Define parameters--------------------------------------------------------
options(stringsAsFactors=F)

# set working directory
# adapt directory path for your system
wd <- wd
setwd(wd)

# Define NEON data file  parameters
data_root_path = "F:/NEON/data"

veg_dpID = "DP1.10098.001"
chm_dpID = "DP3.30015.001"
RGB_dpID = "DP3.30010.001"

fullSite  = paste0(year, '_', site, '_', visit)
startdate = paste0(year, '-01')
enddate   = paste0(year, '-12')
resultsDir = './results/'
nCores = 6

## Create the fully-qualified file names for the CHM and the RGB mosaic files
CHM_path_root = paste(sep = '/',
                      create_NEON_datafile_path(chm_dpID, site = site, year = year, domain = domain, visit = visit, root_path = data_root_path))
CHM_file   = paste0(CHM_path_root, "/DiscreteLidar/CanopyHeightModelGtifMosaic/", fullSite, "_CHM_mosaic.tif")
CHM10_file = paste0(CHM_path_root, "/DiscreteLidar/CanopyHeightModelGtifMosaic/", fullSite, "_CHM_mosaic_10.tif")

RGB_path_root = paste(sep = '/',
                      create_NEON_datafile_path(RGB_dpID, site = site, year = year, domain = domain, visit = visit, root_path = data_root_path))
RGB_file = paste0(RGB_path_root, "/Camera/Mosaic/", fullSite, "_RGB_mosaic.tif")

## ABoVE is dominated by spruce trees (see book ("Tree Line") (not used)
whiteSpruce = "Picea glauca (Moench) Voss"
blackSpruce = "Picea mariana (Mill.) Britton, Sterns & Poggenb."
spruceTrees = factor(c(whiteSpruce, blackSpruce))

## list of scientific names found in HEAL
species = factor(c('species',
                   "Alnus viridis (Chaix) DC",
                   "Betula glandulosa Michx.",
                   "Betula occidentalis Hook.",
                   "Ledum palustre L.",
                   "Picea glauca (Moench) Voss", #white spruce
                   "Picea mariana (Mill.) Britton, Sterns & Poggenb.", # black spruce
                   "Salix bebbiana Sarg.",
                   "Salix glauca L.",
                   "Salix myrtillifolia Andersson",
                   "Salix pulchra Cham.",
                   "Vaccinium uliginosum L."))

## x and y plot labels
xLab = "Ground Measured Canopy Height (m)"
yLab = "Lidar Canopy Height Model (m)"

## Get the veglist
if(!exists("veglist")) {
  veglist <- loadByProduct(dpID=veg_dpID,
                           site=site,
                           startdate=startdate,
                           enddate=enddate,
                           package="basic",
                           nCores = nCores,
                           check.size = FALSE)
}
## View the variables files to figure out which data table the spatial data are contained in
## View(veglist$variables_10098)

## Spatial data (decimalLatitude and decimalLongitude, etc) are in the vst_perplotperyear table
## View(veglist$vst_perplotperyear)

## Data fields for stemDistance and stemAzimuth contain the distance and azimuth from a pointID to a specific stem
## View(veglist$vst_mappingandtagging)

## ----vegmap------------------------------------------------------------------------------
## Get the locations of the individual sampled plants
if(exists("vegmap") == FALSE)
{
  ## Use getLocTOS to calculate precise protocol-specific locations for each sampling effort
  vegmap <- getLocTOS(veglist$vst_mappingandtagging,
                      "vst_mappingandtagging")
}

## ----veg_merge-------------------------------------------------------------------------------------------
# Merge the mapped locations of individuals (the vst_mappingandtagging table) with the annual measurements of height,
## diameter, etc. (the vst_apparentindividual table)
if(exists("veg") == FALSE)
{
  veg <- merge(veglist$vst_apparentindividual, vegmap,
               by=c("individualID","namedLocation",
                    "domainID","siteID","plotID"))
}

## filter by desired variables. variables obtained from colnames(veg)
## This cuts down on the size of the tables
## uncomment to add a variable
desiredVariables = c(
  "individualID",
  "namedLocation",
  # "domainID",
  # "siteID",
  "plotID",
  # "uid.x",
  "date.x",
  # "eventID.x",
  # "tempStemID",
  # "tagStatus",
  "growthForm",
  "plantStatus",
  "stemDiameter",
  "measurementHeight",
  "changedMeasurementLocation",
  "height",
  # "baseCrownHeight",
  # "breakHeight",
  # "breakDiameter",
  "maxCrownDiameter",
  "ninetyCrownDiameter",
  "canopyPosition",
  # "shape",
  "basalStemDiameter",
  # "basalStemDiameterMsrmntHeight" ,
  # "maxBaseCrownDiameter",
  # "ninetyBaseCrownDiameter",
  # "dendrometerInstallationDate",
  # "initialGapMeasurementDate",
  # "initialBandStemDiameter",
  # "initialDendrometerGap",
  # "dendrometerHeight",
  # "dendrometerGap",
  # "dendrometerCondition",
  # "bandStemDiameter",
  # "remarks.x",
  # "recordedBy.x",
  # "measuredBy.x",
  # "dataQF.x",
  # "publicationDate.x",
  # "release.x",
  # "uid.y",
  # "date.y",
  # "eventID.y",
  "subplotID",
  # "nestedSubplotID",
  # "pointID",
  # "stemDistance",
  # "stemAzimuth",
  # "recordType",
  # "supportingStemIndividualID",
  # "previouslyTaggedAs",
  # "samplingProtocolVersion",
  "taxonID",
  "scientificName",
  # "taxonRank",
  # "identificationReferences",
  # "morphospeciesID",
  # "morphospeciesIDRemarks",
  # "identificationQualifier",
  # "remarks.y",
  # "measuredBy.y",
  # "recordedBy.y",
  # "dataQF.y",
  # "publicationDate.y",
  # "release.y",
  "utmZone",
  "adjNorthing",
  "adjEasting",
  "adjCoordinateUncertainty",
  "adjDecimalLatitude",
  "adjDecimalLongitude",
  "adjElevation",
  "adjElevationUncertainty")

veg = veg[desiredVariables]

## get a list of the tree names at this site
# treeNames = sort(unique(veglist$vst_mappingandtagging$scientificName))

## ----get an array of the plotID's, sorted
sitePlots = sort(unique(veg$plotID))
nSP = length(sitePlots)

## ----get-chm, results="hide"-----------------------------------------------------------------------------
chm <- raster(CHM_file)
png(filename = paste0(resultsDir, fullSite, "_CHM.png"), width = 800, height = 800)

plot(chm,
     col = topo.colors(5),
     xlab = "Easting (m)", ylab = "Northing (m)",
     main = c(fullSite, 'Canopy Height'))

dev.off()

## ----vegTrees----------------------------------------------------------------------------------------------
##

## View the extent of chm and subset the vegetation structure table to
##  1. only those individuals that fall within the extent of the CHM mosaic,
##  2. include only trees, not shrubs, and
##  3. trees are "Live", and
##  4. height is not NA and is >= 2.0 m
vegTrees <- veg[which(veg$adjEasting >= extent(chm)[1] &
                        veg$adjEasting <= extent(chm)[2] &
                        veg$adjNorthing >= extent(chm)[3] &
                        veg$adjNorthing <= extent(chm)[4] &
                        (veg$growthForm == "single bole tree" |
                           veg$growthForm == "multi-bole tree" |
                           veg$growthForm == "small tree") &
                        veg$plantStatus == "Live" &
                        !is.na(veg$height) &
                        veg$height >= 2.0), ]

## Write vegTrees to a csv file for outside processing
write.csv(vegTrees, paste0(resultsDir, paste0(fullSite, '_vegTrees.csv')))

## ----buffer-chm------------------------------------------------------------------------------------------
## Extract the CHM value that matches the coordinates of each mapped tree Include a buffer equal to the uncertainty
## in the plant's location, and extract the highest CHM value within the buffer.
CHMtrees <- extract(chm,
                     cbind(vegTrees$adjEasting,
                           vegTrees$adjNorthing),
                     buffer=vegTrees$adjCoordinateUncertainty,
                     fun=max)

## ----Scatterplot-tree heights------------------------------------------------------------------------------
## Limit the plot to x/y extent of the [trees, CHM]
pLim = c(0.0, max(max(vegTrees$height, na.rm = TRUE), max(CHMtrees, na.rm = TRUE)))

png(filename = paste0(resultsDir, fullSite,"_alltrees.png"), width = 800, height = 600)
plot(vegTrees$height~CHMtrees, pch=20,
     xlim = pLim, ylim = pLim,
     xlab = xLab, ylab = yLab,
     main = c(fullSite, 'All Trees'))

regress = lm(CHMtrees ~ 0 + vegTrees$height)
lines(pLim, pLim*regress$coefficients, col="black", lwd = 3)
dev.off()

## ----Correlation:ground vs lidar heights-----------------------------------------------------------------------------------------
## Strength of correlation between the ground and lidar measurements (low correlation)
print(paste0("Correlation, all trees: ", cor(CHMtrees, vegTrees$height, use="complete")))

## ----Round-x-y-------------------------------------------------------------------------------------------
## Filtering understory from dataset (map-centric vs. tree-centric)
## Approach #1: Use a map-centric approach to filter out understory: select a pixel size (e.g., 10m) and aggregate both
## the vegetation structure data and the CHM  to find the tallest point in each pixel

# ## Use floor() instead of round() so each tree ends up in the pixel with the same numbering as the raster pixels
# easting10 <- 10*floor(vegTrees$adjEasting/10)
# northing10 <- 10*floor(vegTrees$adjNorthing/10)
# vegTrees <- cbind(vegTrees, easting10, northing10)
# 
# ## ----vegTreesmax----------------------------------------------------------------------------------------------
# ## Use the stats package version of the aggregate() function to get the tallest tree in each 10m bin
# vegTreesmax <- stats::aggregate(vegTrees, by=list(vegTrees$easting10, vegTrees$northing10), FUN=max)
# 
# ## ----CHM-10----------------------------------------------------------------------------------------------
# 
# ## Use the raster package version of the aggregate() function to create a 10m resolution version of the CHM to match
# if(file.exists(CHM10_file) == FALSE) {
#   print("Creating 10 m CHM file, may take some time")
#   CHM10 <- raster::aggregate(chm, fact=10, fun=max)
#   writeRaster(CHM10, CHM10_file)
# }
# CHM10 <- raster(CHM10_file)

# ## ----adj-tree-coord--------------------------------------------------------------------------------------
# 
# ## Use the extract() function again to get the values from each pixel (uncertainty buffers no longer needed);
# ## add 5 to each tree coordinate to make sure it's in the correct pixel since grids are numbered by the corners
# 
# vegTreesmax$easting10 <- vegTreesmax$easting10+5
# vegTreesmax$northing10 <- vegTreesmax$northing10+5
# binCHM <- raster::extract(CHM10, cbind(vegTreesmax$easting10,
#                                        vegTreesmax$northing10))
# 
# png(paste0(resultsDir, fullSite, "_low_res_chm.png"),  width = 800, height = 600)
# pLim = c(min(min(vegTreesmax$height, na.rm = TRUE), min(binCHM, na.rm = TRUE)),
#          max(max(vegTreesmax$height, na.rm = TRUE), max(binCHM, na.rm = TRUE)))
# 
# plot(binCHM~vegTreesmax$height, pch=20,
#      xlim = pLim, ylim = pLim,
#      xlab = xLab, ylab = yLab,
#      main = paste0(c(fullSite, "Low Res CHM")))
# 
# lines(c(0,50), c(0,50),col="black", lwd = 3)
# dev.off()
# 
# ## ----cor-2-----------------------------------------------------------------------------------------------
# ## Improved correlation between field measurements and CHM, but a lot of data has been lost  going to a lower resolution
# 
# print(paste0("After low res:", cor(binCHM, vegTreesmax$height, use="complete")))

## ----vegTrees-2--------------------------------------------------------------------------------------------
## Approach #2: Use trees as the starting point instead of map area. Start by sorting the veg structure data by height

vegTrees <- vegTrees[order(vegTrees$height, decreasing=T),]

## ----vegTreesfil----------------------------------------------------------------------------------------------
# For each tree, estimate which nearby trees might be beneath its canopy and discard those points:
## 1. Calculate the distance of each tree from the target tree
## 2. Pick a reasonable estimate for canopy size, and discard shorter trees within that radius (e.g., 0.3 times height)
## 3. Iterate over all trees

#First, order the trees by decreasing height (????)
vegTreesfil <- vegTrees[order(vegTrees$height, decreasing=T),]

for(i in 1:nrow(vegTrees)) {
  if(is.na(vegTreesfil$height[i]))
    next
  dist <- sqrt((vegTrees$adjEasting[i]-vegTrees$adjEasting)^2 +
                 (vegTrees$adjNorthing[i]-vegTrees$adjNorthing)^2)
  vegTreesfil$height[which(dist<0.3*vegTrees$height[i] &
                        vegTrees$height<vegTrees$height[i])] <- NA
}

## Filter out heights == NA
vegTreesfil = vegTreesfil[which(!is.na(vegTreesfil$height))]

## ----filter-chm by max value--------------------------------------------------------------
## Now extract the raster values, as above, increasing the buffer size to better account for the uncertainty
## in the lidar data as well as the uncertainty in the ground locations

filterCHM <- raster::extract(chm, cbind(vegTreesfil$adjEasting, vegTreesfil$adjNorthing),
                             buffer=vegTreesfil$adjCoordinateUncertainty+1, fun=max)

png(filename = paste0(resultsDir, fullSite,"_alltrees.png"), width = 800, height = 600)
pLim = c(0, max(max(vegTreesfil$height, na.rm = TRUE), max(filterC, na.rm = TRUE)))

plot(filterCHM~vegTreesfil$height, pch=20,
     xlim = pLim, ylim = pLim,
     xlab = xLab, ylab = yLab,
     main = c(fullSite, 'All Trees'))

regress = lm(filterCHM ~ 0 + vegTreesfil$height)
lines(pLim, pLim*regress$coefficients, col="black", lwd = 3)
dev.off()

## Improved correlation between field measurements and CHM, filtering out most understory trees without losing
## as many overstory trees
print(paste0("Filtered by max CHM ???:", cor(filterCHM,vegTreesfil$height)))

filterCHM <- raster::extract(chm, 
                             cbind(vegTreesfil$adjEasting, vegTreesfil$adjNorthing),
                             buffer=vegTreesfil$adjCoordinateUncertainty+1, fun=max)

png(filename = paste0(resultsDir, fullSite,"_livingtrees.png"), width = 800, height = 600)

plot.window(xlim = pLim, ylim = pLim)
plot(filterCHM~vegTreesfil$height, pch=20, #CHMtrees~vegTrees$height
     xlim = pLim, ylim = pLim,
     xlab = xLab, ylab = yLab,
     main = c(fullSite, "Living Trees"))

regress = lm(filterCHM ~ 0 + vegTreesfil$height)
lines(pLim, pLim*regress$coefficients, col="black", lwd = 3)
dev.off()

print(paste0("Live trees: ", cor(filterCHM,vegTreesfil$height)))

## Write the data to a csv file
## TODO: insert the CHMheight into the vegTreesfil dataframe
write.csv(vegTreesfil, paste0(resultsDir, paste0(fullSite,'_vegTreesfil.csv')))
CHMheightStr = list(sprintf("%0.5f",filterCHM ))
write.csv(CHMheightStr, paste0(resultsDir, paste0(fullSite,'_CHMheight.csv')))

## plot the RGB image, overlay with the tree circles and add the plot boxes
## limit the extent to the area of veg
rgb = brick(RGB_file)

png(paste0(resultsDir, fullSite, "_tree_locations.png"),
    width = 1000, height = 600, unit = 'px')

ext_mgn = 40  ## extra margin so all the circles are shown completely
ext = extent(c(min(veg$adjEasting-ext_mgn,  na.rm = TRUE), max(veg$adjEasting+ext_mgn,  na.rm = TRUE),
               min(veg$adjNorthing-ext_mgn, na.rm = TRUE), max(veg$adjNorthing+ext_mgn, na.rm = TRUE)))
plotRGB(rgb,
        ext = ext,
        xlab = "UTM X (m)", ylab = "UTM Y (m)",
        main = c(fullSite, "Tree Centers in Red"),
        axes = FALSE, margins = TRUE, asp = 1)

## TODO: figure out how to properly place the axes on the plot
axis(1)
axis(2)
axis(3, labels = FALSE)
axis(4, labels = FALSE)

symbols(vegTreesfil$adjEasting,
        vegTreesfil$adjNorthing,
        circles=vegTreesfil$stemDiameter/100/2, inches = 0.1,
        lwd = 2, fg = 'red',
        add = TRUE)
dev.off()

# ## plot the outlines of the NEON field sampling plots.
# ## read in the shape file with the field sampling boundaries
# aop_shp = readOGR("F:/NEON/Site_data/AOP_flightboxesAllSites.shp")
#
# ## convert projection from lat/lon to UTM (CHM is in UTM)
# aop_shp_UTM = spTransform(aop_shp, crs(chm))
#
# ## extract the field sampling boundary for the desired site
# ii = which(aop_shp_UTM@data$siteID == site)
# ## flt_bndry: box including the complete flight boundary
# flt_bndry = aop_shp_UTM@polygons[[ii]]@Polygons[[1]]@coords

## }
# NEON_chm_analysis(
#   site = 'DEJU',
#   year = '2021',
#   domain = 'D19',
#   visit = '4')
