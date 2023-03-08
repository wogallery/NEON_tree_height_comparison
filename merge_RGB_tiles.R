## a program to merge all the 1 m camera rgb files into a single geotif image

# Load required packages
library(neonUtilities)
library(raster)
library(gdalUtilities)
library(data.table)
library(docstring)

dpIDget <- "DP3.30010.001" #Camera tiles
yearGet <- '2021'
siteGet <- 'HEAL'
dataDirs    <- 'f:/data/DP3.30010.001/neon-aop-products/2021/FullSite/D19/2021_HEAL_4/L3/Camera/Tiles'
outFileDir   <- 'f:/data/DP3.30010.001/neon-aop-products/2021/FullSite/D19/2021_HEAL_4/L3/Camera/Image'
outFileTif = "L3_meter.tif"

##
dataTiles <- list(list.files(dataDirs,pattern="*image.tif$",full.names=TRUE,ignore.case=TRUE))

if (length(dataTiles) == 0) {
  print("No RGB 1 meter tiles found")
  stop
}

cat('Creating full site mosaics\n')
fullMosaics <- list()
fullMosaicNames <- list()
for (i in 1:length(dataTiles)) {
  fullMosaics[[i]] <- mergeDataTiles(dataTiles[[i]])
  dataDirSplit <- unlist(strsplit(dataTiles[[i]][1],.Platform$file.sep))
  tileNameSplit <- unlist(strsplit(dataTiles[[i]][1],'_'))
  if (tail(tileNameSplit,1)=='error.tif') {
    fullMosaicNames[[i]] <- paste0(dataDirSplit[9],'_',tail(tileNameSplit,n=2)[1],'_error.tif')}
  else {
    fullMosaicNames[[i]] <- paste0(dataDirSplit[9],'_',tail(tileNameSplit,n=1))}
}

writeFullMosaicTif(fullMosaics[[i]],outFileDir,outFileTif)
