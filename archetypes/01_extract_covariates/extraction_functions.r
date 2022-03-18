## -----------------------------------------------------------------------------------------------------------------
# Seasonality Classification
# 00_extraction_functions.r
# 
# Amelia Bertozzi-Villa, Institute for Disease Modeling, University of Oxford
# May 2018
# 
# Collection of functions for covariate extraction and matrix manipulation. 
# 
# For a detailed project write-up see
# https://paper.dropbox.com/doc/Cluster-MAP-pixels-by-seasonality-zga4UM1DnBx8pc11rStOS
## -----------------------------------------------------------------------------------------------------------------------

library(raster)
library(data.table)
library(stringr)

get_mask <- function(continent, in_fname, out_fname){
  print("generating mask")
  mask_vals <- list('africa'=1,
                    'americas'=2,
                    'asia'=3)
  mask <- raster(in_fname)
  # mask1 <- crop(mask1,extent(-25,53,-30,30))
  mask[mask!=mask_vals[[continent]]] <- NA
  #mask1[mask1==0] <- NA
  clipped_mask <- trim(mask)
  writeRaster(clipped_mask, out_fname, overwrite=T)
  #writeRaster(mask1, out_fname, overwrite=T)
  return(clipped_mask)
  #return(mask1)
}

crop_raster <- function(full_raster, mask, out_fname=NULL){
  # crop, save, and return values for a Raster* object
  vals <- extend(crop(full_raster, mask), mask)
  compareRaster(vals, mask)
  vals <- raster::mask(vals, mask, maskvalue=0)

  if (length(out_fname)>0){
    print("saving raster")
    writeRaster(vals, out_fname, bylayer=T, overwrite=T)
  }
  
  return(vals)
}

# function to align resolutions between two rasters
align_res <- function(rast, template.rast){
  if (!identical(res(rast), res(template.rast))) {
    rast  <- raster::resample(rast, template.rast, method = 'ngb')
  }
  return(rast)
}

save_raster <- function(full_raster, mask, out_fname){
  r1<-crop(full_raster,mask)
  m1<-crop(mask,r1)
  ms<-resample(r1,m1)
  #frm<-mask(r1,ms)
  frm<-raster::mask(ms,m1,maskvalue=0)
  writeRaster(frm, out_fname, bylayer=T, overwrite=T)
}



