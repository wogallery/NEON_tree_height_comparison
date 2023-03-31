create_NEON_datafile_path <- function(
    dpID,
    site,
    year,
    domain,
    visit,
    root_path = '.') {

  ## Get the product level from the product id, e.g. "L3" from "DP3.30010.001"
  level = paste0('L', substr(dpID, 3, 3))
  ## Get the full site, e.g. "2021_DEJU_4"
  full_site = paste(sep = '_', year,  site, visit)
  ## Create the root path to the data for this dpID, site, year, and visit
  ## TODO, create a table of domain vs site and eliminate domain as an input
  file_path = paste(sep = '/', root_path, dpID,"neon-aop-products", year, "FullSite", domain, full_site, level)

  return(file_path)
}
