get_site_info <- function(siteCode, rootDataPath = './', method = 'file') {
  ## Needs further work
  ## 1. make the output of the two methods identical
  ## 2. method == file: fix so it downloads  or creates a site.json file
  
  library(rjson)
  library(httr)
  library(jsonlite)
  
  sitesFile = file.path(rootDataPath, "sites.json")
  
  if(method == "file") {
    ## Method 1: from the jason file sites.json
    ## Check for/get the sites.jason file
    if(!file.exists(sitesFile)) {
      print("Cannot create a json file at this time. Returning")
      return(NULL)
      ## Create the sites json file
      ## Get the site data as a list
      siteDataUrl = "https://data.neonscience.org/data-api/endpoints/sites/"
      siteFileData = GET(siteDataUrl)
      
      ## Convert the list to a json object
      siteFileData.text <- content(siteFileData, as="text")
      
      # Flatten json into a nested list
      siteFileList<- jsonlite::fromJSON(siteFileData, 
                                        simplifyDataFrame=T, 
                                        flatten=T)
      #siteDataJson = toJSON(siteFileData, method = "C")
      
      write(sitesFile, siteDataJson)
    }
    ## Read in the sites json file
    siteDataAll = rjson::fromJSON(file = sitesFile)
    if(is.na(siteDataAll)) {
      print(paste0("File not found: ", sitesFile))
      return(NULL)
    }
    
    j = -1
    for(i in array(1:87)) if(siteDataAll$data[[i]]$siteCode == siteCode) j = i
    if(j == -1) {
      print("from get_site_info: no info for site code ", siteCode)
      return(NULL)
    }
    
    siteData = siteDataAll$data[[j]][1:12]
    return(siteData)
  }
  else if(method == "API") {
    
    # Method 2: use the NEON API to get the site data
    siteDataAPI <- GET("http://data.neonscience.org/api/v0/sites")
    if(siteDataAPI$status_code != 200) {
      print(paste0("From get_site_info: API request unsuccessful, status code = ", siteDataAPI$status_code))
      return(NULL)
    }
    
    # Make the data readable by jsonlite
    siteDataAPI.text <- content(siteDataAPI, as="text")
    
    # Flatten json into a nested list
    siteData <- jsonlite::fromJSON(siteDataAPI.text,
                                   simplifyDataFrame=T,
                                   flatten=T)
    
    ## Find the index of the info for the requested site
    isite = which(siteData$data$siteCode == siteCode)
    if(length(isite) == 0) {
      print(paste0("From get_site_info: no info for site code ", siteCode))
      return(NULL)
    }
    
    siteData = siteData$data[isite, 1:10]
    return(siteData)
  }
  else {
    print(paste0("From get_site_info, method not supported, method = ", method))
    return(NULL)
  }
}