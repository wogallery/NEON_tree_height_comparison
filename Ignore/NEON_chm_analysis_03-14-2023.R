NEON_chm_analysis <- function(site, year, domain, visit, wd = '.')  {

  ## ----install_packages, eval=FALSE------------------------------------------------------------------------
  ##
  # install.packages("neonUtilities")
  # install.packages("sp")
  # install.packages("raster")
  # install.packages("devtools")
  # devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
  # install.packages("gdalUtilities")
  # install.packages("dplyr")

  ## ----load-packages, results="hide"-----------------------------------------------------------------------

  library(sp)
  library(raster)
  library(neonUtilities)
  library(geoNEON)
  library(gdalUtilities)
  library(rgdal)
  ## library(dplyr)

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
  if(exists("vegmap") == FALSE)
  {
    ## Use getLocTOS to calculate precise protocol-specific locations for each sampling effort
    vegmap <- getLocTOS(veglist$vst_mappingandtagging,
                        "vst_mappingandtagging")
  }

  ## ----veg_merge-------------------------------------------------------------------------------------------
  if(exists("veg") == FALSE)
  {
    ## Merge the mapped locations of individuals (the vst_mappingandtagging table) with the annual measurements of height,
    ## diameter, etc. (the vst_apparentindividual table)
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

  ## View the extent of chm and subset the vegetation structure table to only those individuals that fall within the extent of the CHM tile
  extent(chm)

  vegsub <- veg[which(veg$adjEasting >= extent(chm)[1] &
                        veg$adjEasting <= extent(chm)[2] &
                        veg$adjNorthing >= extent(chm)[3] &
                        veg$adjNorthing <= extent(chm)[4]),]
  write.csv(veg, paste0(resultsDir, paste0(fullSite, '_vegsub.csv')))

  ## ----buffer-chm------------------------------------------------------------------------------------------
  ## Extract the CHM value that matches the coordinates of each mapped plant. Include a buffer equal to the uncertainty
  ## in the plant's location, and extract the highest CHM value within the buffer.
  bufferCHM <- extract(chm,
                       cbind(vegsub$adjEasting,
                             vegsub$adjNorthing),
                       buffer=vegsub$adjCoordinateUncertainty,
                       fun=max)

  ## ----scatterplot-buffer-chm------------------------------------------------------------------------------
  ## Limit the plot to x/y extent of the [trees, CHM]
  pLim = c(0.0, max(max(vegsub$height, na.rm = TRUE), max(bufferCHM, na.rm = TRUE)))

  #  png(filename = paste0(resultsDir, fullSite,"_allveg.png"), width = 800, height = 600)
  plot(bufferCHM~vegsub$height, pch=20,
       xlim = pLim, ylim = pLim,
       xlab = xLab, ylab = yLab,
       main = c(fullSite, 'All Vegetation'))
  lines(c(0,50), c(0,50), col="black", lwd = 3)
  dev.off()

  ## ----corr-buffer-----------------------------------------------------------------------------------------
  ## Strength of correlation between the ground and lidar measurements (low correlation)
  print(paste0("Correlation, all trees: ", cor(bufferCHM, vegsub$height, use="complete")))

  ## ----round-x-y-------------------------------------------------------------------------------------------

  ## Filtering understory from dataset (map-centric vs. tree-centric)
  ## Approach #1: Use a map-centric approach to filter out understory: select a pixel size (e.g., 10m) and aggregate both
  ## the vegetation structure data and the CHM  to find the tallest point in each pixel

  ## Use floor() instead of round() so each tree ends up in the pixel with the same numbering as the raster pixels
  easting10 <- 10*floor(vegsub$adjEasting/10)
  northing10 <- 10*floor(vegsub$adjNorthing/10)
  vegsub <- cbind(vegsub, easting10, northing10)

  ## ----vegbin----------------------------------------------------------------------------------------------
  ## Use the stats package version of the aggregate() function to get the tallest tree in each 10m bin
  vegbin <- stats::aggregate(vegsub, by=list(vegsub$easting10, vegsub$northing10), FUN=max)

  # symbols(vegbin$adjEasting[which(vegbin$plotID=="HEAL_033")],
  #         vegbin$adjNorthing[which(vegbin$plotID=="HEAL_033")],
  #         circles=vegbin$stemDiameter[which(vegbin$plotID=="HEAL_033")]/100/2,
  #         inches=F, xlab="Easting", ylab="Northing")

  ##
  ## ----CHM-10----------------------------------------------------------------------------------------------

  ## Use the raster package version of the aggregate() function to create a 10m resolution version of the CHM to match
  if(file.exists(CHM10_file) == FALSE) {
    print("Creating 10 m CHM file, may take some time")
    CHM10 <- raster::aggregate(chm, fact=10, fun=max)
    writeRaster(CHM10, CHM10_file)
  }
  CHM10 <- raster(CHM10_file)

  ## ----adj-tree-coord--------------------------------------------------------------------------------------

  ## Use the extract() function again to get the values from each pixel (uncertainty buffers no longer needed);
  ## add 5 to each tree coordinate to make sure it's in the correct pixel since grids are numbered by the corners

  vegbin$easting10 <- vegbin$easting10+5
  vegbin$northing10 <- vegbin$northing10+5
  binCHM <- raster::extract(CHM10, cbind(vegbin$easting10,
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
  ## Improved correlation between field measurements and CHM, but a lot of data has been lost  going to a lower resolution

  print(paste0("After low res:", cor(binCHM, vegbin$height, use="complete")))

  ## ----vegsub-2--------------------------------------------------------------------------------------------
  ## Approach #2: Use trees as the starting point instead of map area. Start by sorting the veg structure data by height

  vegsub <- vegsub[order(vegsub$height, decreasing=T),]

  ## ----vegfil----------------------------------------------------------------------------------------------
  # For each tree, estimate which nearby trees might be beneath its canopy and discard those points:
  ## 1. Calculate the distance of each tree from the target tree
  ## 2. Pick a reasonable estimate for canopy size, and discard shorter trees within that radius (e.g., 0.3 times height)
  ## 3. Iterate over all trees
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
  ## Now extract the raster values, as above, increasing the buffer size to better account for the uncertainty
  ## in the lidar data as well as the uncertainty in the ground locations

  filterCHM <- raster::extract(chm, cbind(vegfil$adjEasting, vegfil$adjNorthing),
                               buffer=vegfil$adjCoordinateUncertainty+1, fun=max)

  png(filename = paste0(resultsDir, fullSite,"_alltrees.png"), width = 800, height = 600)
  pLim = c(0, max(max(vegsub$height, na.rm = TRUE), max(bufferCHM, na.rm = TRUE)))

  plot(filterCHM~vegfil$height, pch=20,
       xlim = pLim, ylim = pLim,
       xlab = xLab, ylab = yLab,
       main = c(fullSite, 'All Trees'))
  lines(c(0,50), c(0,50), col="black", lwd = 3)
  dev.off()

  ## Improved correlation between field measurements and CHM, filtering out most understory trees without losing
  ## as many overstory trees
  print(paste0("Filtered by max CHM ???:", cor(filterCHM,vegfil$height)))

  ## -------Live-trees------------------------------------------------------------------------------------------

  ## The plantStatus field in the veg structure data indicates whether a plant is dead, broken, or otherwise damaged.
  ## It's possible but unlikely that dead stem is tallest structure, also less likely to get a good lidar return.
  ## Exclude trees that aren't alive

  vegfil <- vegfil[which(vegfil$plantStatus=="Live"),]
  filterCHM <- raster::extract(chm, cbind(vegfil$adjEasting, vegfil$adjNorthing),
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

  write.csv(vegfil, paste0(resultsDir, paste0(fullSite,'_vegfil.csv')))

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


  ## plot the outlines of the NEON field sampling plots.
  ## read in the shape file with the field sampling boundaries
  aop_shp = readOGR("F:/NEON/Site_data/AOP_flightboxesAllSites.shp")

  ## convert projection from lat/lon to UTM (CHM is in UTM)
  aop_shp_UTM = spTransform(aop_shp, crs(chm))

  ## extract the field sampling boundary for the desired site
  ii = which(aop_shp_UTM@data$siteID == site)
  ## flt_bndry: box including the complete flight boundary
  flt_bndry = aop_shp_UTM@polygons[[ii]]@Polygons[[1]]@coords

}
#
NEON_chm_analysis(
  site = 'DEJU',
  year = '2021',
  domain = 'D19',
  visit = '4')
