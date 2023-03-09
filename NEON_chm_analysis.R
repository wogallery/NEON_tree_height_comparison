## ----install_packages, eval=FALSE------------------------------------------------------------------------
##
# install.packages("neonUtilities")
# install.packages("sp")
# install.packages("raster")
# install.packages("devtools")
# devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
# install.packages("gdalUtilities")
##

## install.packages("dplyr")


## ----load-packages, results="hide"-----------------------------------------------------------------------

library(sp)
library(raster)
library(neonUtilities)
library(geoNEON)
## library(dplyr)
library(gdalUtilities)
library(rgdal)

options(stringsAsFactors=F)

# set working directory
# adapt directory path for your system
# wd <- "./data"
# setwd(wd)

## Specify the case: domain site, year, visit, CHM_file
# domain    = 'D19'
# site      = 'HEAL'
# year      = '2021'
# visit     = '4'
# CHM_file  = "F:/NEON/data/DP3.30010.001/neon-aop-products/2021/FullSite/D19/2021_HEAL_4/L3/DiscreteLidar/FCanopyHeightModelGtifMosaic/2021_HEAL_4_CHM_mosaic.tif"
# RGB_file  = "F:/NEON/data/DP3.30010.001/neon-aop-products/2021/FullSite/D19/2021_HEAL_4/L3/Camera/Mosaic/2021_HEAL_4.tif"

domain    = 'D19'
site      = 'DEJU'
year      = '2021'
visit     = '4'
CHM_file  = "F:/NEON/data/DP3.30015.001/neon-aop-products/2021/FullSite/D19/2021_DEJU_4/L3/DiscreteLidar/CanopyHeightModelGtifMosaic/2021_DEJU_4_CHM_mosaic.tif"
RGB_file  = "F:/NEON/data/DP3.30010.001/neon-aop-products/2021/FullSite/D19/2021_DEJU_4/L3/Camera/Mosaic/2021_DEJU_4.tif"

# There is no Vegetation structure (dp1.1000098) product for TOOL for any date
# domain    = 'D18'
# site      = 'TOOL'
# year      = '2019'
# visit     = '3'
#

fullSite  = paste0(year, '_', site, '_', visit)
startdate = paste0(year, '-01')
enddate   = paste0(year, '-12')
resultsDir = './results/'
nCores = 6

veg_dpID = "DP1.10098.001"
chm_dpID = "DP3.30015.001"
RGB_dpID = "DP3.30010.001"

# Note: do not end directory names with a "/" when used in file.path()
data_path_root = "F:/NEON/data"

CHMdataRoot  = paste0("F:/NEON/data/",chm_dpID,"/neon-aop-products/",year,"/FullSite/",domain,"/",fullSite,"/L3")
RGBdataRoot  = paste0("F:/NEON/data/",RGB_dpID,"/neon-aop-products/",year,"/FullSite/",domain,"/",fullSite,"/L3")

CHM_file  = paste0(CHMdataRoot, "/DiscreteLidar/CanopyHeightModelGtifMosaic/", fullSite, "_CHM_mosaic.tif")
RGB_file  = paste0(RGBdataRoot, "/Camera/Mosaic/", fullSite, "_RGB_mosaic.tif")

## ABoVE is dominated by spruce trees (see book ("Tree Line")
whiteSpruce = "Picea glauca (Moench) Voss"
blackSpruce = "Picea mariana (Mill.) Britton, Sterns & Poggenb."
spruceTrees = factor(c(whiteSpruce, blackSpruce))

## list of scientific names found in HEAL
species = factor(c('species',
                   "Alnus viridis (Chaix) DC",
                   "Betula glandulosa Michx.",
                   "Betula occidentalis Hook.",
                   "Ledum palustre L.",
                   "Picea glauca (Moench) Voss",
                   "Picea mariana (Mill.) Britton, Sterns & Poggenb.",
                   "Salix bebbiana Sarg.",
                   "Salix glauca L.",
                   "Salix myrtillifolia Andersson",
                   "Salix pulchra Cham.",
                   "Vaccinium uliginosum L."))

## which of species are trees, not shrubs (needs review)
isTree = c(FALSE, TRUE , TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)

## x and y plot labels
xLab = "Lidar Canopy Height Model (m)"
yLab = "Ground Measured Canopy Height (m)"

## ----veglist, results="hide"-----------------------------------------------------------------------------
# if(exists("veglist")== FALSE){
#   ## Check if the product data file has already been downloaded into data_path
#   data_path = file.path(data_path_root,veg_dpID,"neon-aop-products",year,
#                         "FullSite",domain,fullSite,"L1","veg_str")
#   if (file.exists(data_path)) {
#     zip_file = list.files(path = data_path, pattern = '*.zip', recursive = TRUE)
#     if (is.na(zip_file[1])) {
#       ##download the data to data_path
#       zipsByProduct(dpID=veg_dpID, site=site, startdate=startdate, enddate=enddate, package="basic",
#                     release="current", timeIndex="all", tabl="all", check.size=FALSE,
#                     savepath=data_path, load=TRUE)
#     }
#     zip_file = list.files(path = data_path, pattern = '*.zip', recursive = TRUE, full.names = TRUE)
#     veglist <- stackByTable(filepath= dirname(zip_file),
#                         savepath="./data", folder=TRUE, nCores=nCores,
#                         saveUnzippedFiles=TRUE)
#   }
# }

if(!exists("veglist")) {
  veglist<- loadByProduct(dpID="veg_dpID",
                         site=site,
                         startdate=startdate,
                         enddate=enddate,
                         package="basic",
                         nCores = nCores,
                         check.size = FALSE)
}

## ----vegmap, results="hide"------------------------------------------------------------------------------
if(exists("vegmap") == FALSE)
{
  vegmap <- getLocTOS(veglist$vst_mappingandtagging,
                      "vst_mappingandtagging")
}

## ----veg_merge-------------------------------------------------------------------------------------------
if(exists("veg") == FALSE)
{
  veg <- merge(veglist$vst_apparentindividual, vegmap,
               by=c("individualID","namedLocation",
                    "domainID","siteID","plotID"))
}

## filter by trees, not shrubs
if(exists("vegsub") == FALSE)
{
  vegsub = veg[which(veg$growthForm=="single bole tree" |
                       veg$growthForm=="multi-bole tree"  |
                       veg$growthForm=="small tree"),]
}

## filter by desired variables. variables obtained from colnames(veg)
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

## ----Plot tree locations and diameters ------------------------------------------------------------
# png(paste0(resultsDir, fullSite, '_tree_locations.png'), width = 800, height = 600)
#
# symbols(veg$adjEasting,
#         veg$adjNorthing,
#         circles=veg$adjCoordinateUncertainty, asp = 1,
#         xlab="Easting", ylab="Northing",
#         main = c(fullSite, 'Tree Locations and Diameters'),
#         inches=0.1, fg="lightblue")
#
# symbols(veg$adjEasting,
#         veg$adjNorthing,
#         circles=veg$stemDiameter/100/2, asp = 1,
#         inches=.1, add = T)
# dev.off()

## ----get-chm, results="hide"-----------------------------------------------------------------------------
chm <- raster(CHM_file)
png(filename = paste0(resultsDir, fullSite, "_CHM.png"), width = 800, height = 800)

plot(chm,
     col = topo.colors(5),
     xlab = "Easting (m)", ylab = "Northing (m)",
     main = c(fullSite, 'Canopy Height'))

dev.off()

## ----vegsub----------------------------------------------------------------------------------------------
##
vegsub <- veg[which(veg$adjEasting >= extent(chm)[1] &
                      veg$adjEasting <= extent(chm)[2] &
                      veg$adjNorthing >= extent(chm)[3] &
                      veg$adjNorthing <= extent(chm)[4]),]
write.csv(veg, paste0(resultsDir, paste0(fullSite, '_vegsub.csv')))

## ----buffer-chm------------------------------------------------------------------------------------------
bufferCHM <- extract(chm,
                     cbind(vegsub$adjEasting,
                           vegsub$adjNorthing),
                     buffer=vegsub$adjCoordinateUncertainty,
                     fun=max)

## Limit the plot to x/y extent of the [trees, CHM]
pLim = c(0.0, ##min(min(vegsub$height, na.rm = TRUE), min(bufferCHM, na.rm = TRUE)),
         max(max(vegsub$height, na.rm = TRUE), max(bufferCHM, na.rm = TRUE)))

png(filename = paste0(resultsDir, fullSite,"_alltrees.png"), width = 800, height = 600)
plot(bufferCHM~vegsub$height, pch=20,
     xlim = pLim, ylim = pLim,
     xlab = xLab, ylab = yLab,
     main = c(fullSite, 'All Trees'))
lines(c(0,50), c(0,50), col="black", lwd = 3)
dev.off()

## ----corr-buffer-----------------------------------------------------------------------------------------
print(paste0("Correlation, all trees: ", cor(bufferCHM, vegsub$height, use="complete")))

## ----round-x-y-------------------------------------------------------------------------------------------
easting10 <- 10*floor(vegsub$adjEasting/10)
northing10 <- 10*floor(vegsub$adjNorthing/10)
vegsub <- cbind(vegsub, easting10, northing10)

## ----vegbin----------------------------------------------------------------------------------------------
vegbin <- stats::aggregate(vegsub, by=list(vegsub$easting10, vegsub$northing10), FUN=max)

## ----CHM-10----------------------------------------------------------------------------------------------
CHM10 <- raster::aggregate(chm, fact=10, fun=max)

## ----adj-tree-coord--------------------------------------------------------------------------------------
vegbin$easting10 <- vegbin$easting10+5
vegbin$northing10 <- vegbin$northing10+5
binCHM <- extract(CHM10, cbind(vegbin$easting10,
                               vegbin$northing10))

png(paste0(resultsDir, fullSite, "_low_res_chm.png"),  width = 800, height = 600)
pLim = c(min(min(vegbin$height, na.rm = TRUE), min(binCHM, na.rm = TRUE)),
         max(max(vegbin$height, na.rm = TRUE), max(binCHM, na.rm = TRUE)))

plot(binCHM~vegbin$height, pch=20,
     xlim = pLim, ylim = pLim,
     xlab = xLab, ylab = yLab,
     main = paste0(c(fullSite, "Low Res CHM")))

lines(c(0,50), c(0,50),col="black", lwd = 3)
dev.off()

## ----cor-2-----------------------------------------------------------------------------------------------
print(paste0("After low res:", cor(binCHM, vegbin$height, use="complete")))

## ----vegsub-2--------------------------------------------------------------------------------------------
vegsub <- vegsub[order(vegsub$height, decreasing=T),]

## ----vegfil----------------------------------------------------------------------------------------------
vegfil <- vegsub
for(i in 1:nrow(vegsub)) {
  if(is.na(vegfil$height[i]))
    next
  dist <- sqrt((vegsub$adjEasting[i]-vegsub$adjEasting)^2 +
                 (vegsub$adjNorthing[i]-vegsub$adjNorthing)^2)
  vegfil$height[which(dist<0.3*vegsub$height[i] &
                        vegsub$height<vegsub$height[i])] <- NA
}

vegfil <- vegfil[which(!is.na(vegfil$height)),]

## ----filter-chm by max value--------------------------------------------------------------

filterCHM <- extract(chm, cbind(vegfil$adjEasting, vegfil$adjNorthing),
                     buffer=vegfil$adjCoordinateUncertainty+1, fun=max)

png(filename = paste0(resultsDir, fullSite,"_alltrees.png"), width = 800, height = 600)
pLim = c(min(min(vegsub$height, na.rm = TRUE), min(bufferCHM, na.rm = TRUE)),
         max(max(vegsub$height, na.rm = TRUE), max(bufferCHM, na.rm = TRUE)))

plot(bufferCHM~vegsub$height, pch=20,
     xlim = pLim, ylim = pLim,
     xlab = xLab, ylab = yLab,
     main = c(fullSite, 'All Trees'))
lines(c(0,50), c(0,50), col="black", lwd = 3)
dev.off()

print(paste0("Filtered by max CHM ???:", cor(filterCHM,vegfil$height)))

## -------Live-trees------------------------------------------------------------------------------------------

vegfil <- vegfil[which(vegfil$plantStatus=="Live"),]
filterCHM <- extract(chm, cbind(vegfil$adjEasting, vegfil$adjNorthing),
                     buffer=vegfil$adjCoordinateUncertainty+1, fun=max)

png(filename = paste0(resultsDir, fullSite,"_livingtrees.png"), width = 800, height = 600)

plot.window(xlim = pLim, ylim = pLim)
plot(filterCHM~vegfil$height, pch=20, #bufferCHM~vegsub$height
     xlim = pLim, ylim = pLim,
     xlab = xLab, ylab = yLab,
     main = c(fullSite, "Living Trees"))
lines(c(0,50), c(0,50), col="black", lwd = 3)
dev.off()

print(paste0("Live trees: ", cor(filterCHM,vegfil$height)))

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

symbols(vegfil$adjEasting,
        vegfil$adjNorthing,
        circles=vegfil$stemDiameter/100/2, inches = 0.1,
        lwd = 2, fg = 'red',
        add = TRUE)
dev.off()

write.csv(veg, paste0(resultsDir, paste0(fullSite,'_vegfil.csv')))

## plot the outlines of the NEON field sampling plots.
## read in the shape file with the field sampling boundaries
aop_shp = readOGR("F:/NEON/Site_data/AOP_flightboxesAllSites.shp")

## convert projection from lat/lon to UTM (CHM is in UTM)
aop_shp_UTM = spTransform(aop_shp, crs(chm))

## extract the field sampling boundary for the desired site
ii = which(aop_shp_UTM@data$siteID == site)
flt_bndry = aop_shp_UTM@polygons[[ii]]@Polygons[[1]]@coords


