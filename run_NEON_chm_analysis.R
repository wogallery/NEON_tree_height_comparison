## Program to run the routine NEON_chm_analysis
site     = 'DEJU'
year     = '2017'
domain   = 'D19'
visit    = '1'
wd       = "F:/NEON/analysis"  # Modify to fit your system
resultsDir = wd  # Where to put the analysis
if(!file.exists(wd)) dir.create(wd)
setwd(wd)

print(paste0(
  "site: ", site,
  "year: ",  year,
  "domain: ", domain,
  "visit: ", visit, 
  "wd: ", wd, 
  "resultsDir = ", resultsDir
))

NEON_chm_analysis( 
  site,
  year,
  domain,
  visit,
  wd,
  resultsDir, 
  screen = FALSE)
