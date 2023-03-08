##
resize_NEON_RGB_images_all <- function(dataDir) {

  ## test if dataDir exists
  if (file.exists(dataDir) == FALSE) {
    print(paste0("dataDir does not exist:  " , dataDir))
    return
  }

  ## get a list of all the tif files in the directory (restrict to unresized files???)
  imageFiles = paste0(dataDir, list.files(dataDir, pattern = "*image.tif"))
  if (length(imageFiles) == 0) {
    paste0("No image files found")
    return
  }
  print(paste0('Number of image files: ', length(imageFiles)))

  ## resize all the files
  for (imgFile in imageFiles) {
    paste0("resizing file: ", imgFile)
    resize_NEON_RGB_image(imgFile)
  }
  return(paste0(imageFiles))
}
