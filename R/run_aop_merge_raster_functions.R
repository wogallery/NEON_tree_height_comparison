##___________________________________________________________________
## Main program
## Set parameters
dpID =  "DP3.30024.001"
year = "2021"
siteCode = "DEJU"
visit = "4"
domain = "D19"
resolutionFac = 10
dataRootDir = "F:/NEON/data"

## Set the working directory
setwd(dataRootDir)


makeFullSiteMosaics(
  dpID,
  year,
  siteCode,
  domain,
  visit,
  resolutionFac, 
  dataRootDir)

