site     = 'SJER'
year     = '2017'
domain   = 'D17'
visit    = '2'
wd       = "F:/NEON/Tutorials/R/VegStrCHMR"

print(paste0(
  "site: ", site,
  "year: ",  year,
  "domain: ", domain,
  "visit: ", visit, 
  "wd: ", wd 
))

NEON_chm_analysis( 
  site     = 'SJER',
  year     = '2019',
  domain   = 'D17',
  visit    = '4',
  wd       = "F:/NEON/Tutorials/R/VegStrCHMR",
  screen = FALSE)
