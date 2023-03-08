## function to convert an RGB geotif tile to 1 meter resolution
resize_NEON_RGB_image <- function(imageFile, overwrite=FALSE) {

  ##test if input file exists
  if (file.exists(imageFile)==FALSE) {
    print(paste0('File does not exits: ', imageFile))
    return(FALSE)
  }

  ## check if the output file exists
  ## create the name for the resized image
  imageFile2 = paste0(strsplit(imageFile, ".tif"), "_1_meter.tif")
  if (file.exists(imageFile2) & overwrite==FALSE) {
    print(paste0('Refsized file exists, returning', imageFile2))
    return(FALSE)
  }

  Fact = c(10, 10, 1)

  ## read in the image. Note: only reads in one color, need to fix
  image = raster(imageFile)

  ## create the name for the resized image
  imageFile2 = paste0(strsplit(imageFile, ".tif"), "_1_meter.tif")
  paste0("Resizing file: ", imageFile2)
  image2 <- aggregate(image, fact =Fact, filename = imageFile2, overwrite = TRUE)

}
