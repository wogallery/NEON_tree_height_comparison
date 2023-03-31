create_aop_root_data_path <- function(
    dpID,
    site,
    year,
    domain,
    visit = "*",
    root_path = '.')
  {

  ## Create the path to the root directory for a NEON AOP product, e.g.:
  ## "./DP3.30015.001/neon-aop-products/2021/FullSite/D19/2021_BONA_4"
  
  ## Get the full site, e.g. "2021_DEJU_4"
  ## If visit is not known, set it to "*" for use in the file search command to find an existing dir
  full_site = paste(sep = '_', year,  site, visit)

  ## Create the root path to the data for this dpID, site, year, and visit
  ## TODO, create a table of domain vs site and eliminate domain as an input
  rootDataPath = file.path(root_path, dpID,"neon-aop-products", year, "FullSite", domain, full_site)

  return(rootDataPath)
}
