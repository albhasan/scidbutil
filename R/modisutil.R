#' @title Add quality data to a MOD13Q1 time series
#' @name addMod13Q1quality
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Add readable quality data to a MOD13Q1 time series according to its data quality
#'
#' @param x A data frame containing the column c("quality")
#' @return A data frame with additional columns.
#' @export
addMod13Q1quality <- function(x){
  # get the codes
  x <- .mod13q1quality2codes(x)
  # convet codes to factors
  x$MODLAND_QA <- as.factor(x$MODLAND_QA)
  levels(x$MODLAND_QA)[levels(x$MODLAND_QA)=="00"] <- "VI produced, good quality"
  levels(x$MODLAND_QA)[levels(x$MODLAND_QA)=="01"] <- "VI produced, but check other QA"
  levels(x$MODLAND_QA)[levels(x$MODLAND_QA)=="10"] <- "Pixel produced, but most probably cloudy"
  levels(x$MODLAND_QA)[levels(x$MODLAND_QA)=="11"] <- "Pixel not produced due to other reasons than clouds"
  x$VI_useful <- as.factor(x$VI_useful)
  levels(x$VI_useful)[levels(x$VI_useful)=="0000"] <- "Highest quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="0001"] <- "Lower quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="0010"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="0100"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1000"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1001"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1010"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1100"] <- "Lowest quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1101"] <- "Quality so low that it is not useful"
  levels(x$VI_useful)[levels(x$VI_useful)=="1110"] <- "L1B data faulty"
  levels(x$VI_useful)[levels(x$VI_useful)=="1111"] <- "Not useful for any other reason/not processed"
  x$AerQuantity <- as.factor(x$AerQuantity)
  levels(x$AerQuantity)[levels(x$AerQuantity)=="00"] <- "Climatology"
  levels(x$AerQuantity)[levels(x$AerQuantity)=="01"] <- "Low"
  levels(x$AerQuantity)[levels(x$AerQuantity)=="10"] <- "Average"
  levels(x$AerQuantity)[levels(x$AerQuantity)=="11"] <- "High"
  x$AdjCloud <- sapply(x$AdjCloud, .tn2bool)
  x$AtmBRDF <- sapply(x$AtmBRDF, .tn2bool)
  x$MixCloud <- sapply(x$MixCloud, .tn2bool)
  x$LandWater <- as.factor(x$LandWater)
  levels(x$LandWater)[levels(x$LandWater)=="000"] <- "Shallow ocean"
  levels(x$LandWater)[levels(x$LandWater)=="001"] <- "Land (Nothing else but land)"
  levels(x$LandWater)[levels(x$LandWater)=="010"] <- "Ocean coastlines and lake shorelines"
  levels(x$LandWater)[levels(x$LandWater)=="011"] <- "Shallow inland water"
  levels(x$LandWater)[levels(x$LandWater)=="100"] <- "Ephemeral water"
  levels(x$LandWater)[levels(x$LandWater)=="101"] <- "Deep inland water"
  levels(x$LandWater)[levels(x$LandWater)=="110"] <- "Moderate or continental ocean"
  levels(x$LandWater)[levels(x$LandWater)=="111"] <- "Deep ocean"
  x$snowice <- sapply(x$snowice, .tn2bool)
  x$shadow <- sapply(x$shadow, .tn2bool)
  return(x)
}



# Cast MOD13Q1 quality data to codes
#
# @param x A data frame containing the column c("quality")
# @return A data frame with additional columns.
# @notes: See TABLE 2: MOD13Q1 VI Quality at https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1
.mod13q1quality2codes <- function(x){
  rqa <- .invertString(R.utils::intToBin(x$quality))
  #
  x$MODLAND_QA <-  substr(rqa, 1, 2)
  x$VI_useful  <-  substr(rqa, 3, 6)
  x$AerQuantity <- substr(rqa, 7, 8)
  x$AdjCloud <-    substr(rqa, 9, 9)
  x$AtmBRDF <-     substr(rqa, 10, 10)
  x$MixCloud <-    substr(rqa, 11, 11)
  x$LandWater <-   substr(rqa, 12, 14)
  x$snowice <-     substr(rqa, 15, 15)
  x$shadow <-      substr(rqa, 16, 16)
  return(x)
}



#' @title Get tile's H and V from MODIS' tile Id
#' @name getHV
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get the tileH and tileV from a MODIS tile Id
#'
#' @param modisTileId A character with a MODIS tile id (i.e "h10v08")
#' @return A character vector of 2 elements c(tH, tV)
#' @export
getHV <- function(modisTileId){
  tH <- substr(modisTileId, 2, 3)
  tV <- substr(modisTileId, 5, 6)
  return(c(tH, tV))
}



#' @title Return the GMPI of the first pixel (top left) of the given MODIS tile
#' @name getFirstGmip
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Return the GMPI of the first pixel (top left) of the given MODIS tile
#'
#' @param modisTileId A character with a MODIS tile id (i.e "h10v08")
#' @param nrows Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
#' @param ncols Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
#' @return Numeric vector containing the c(i,j) pixel coordinates in th GMPI
#' @export
getFirstGmip <- function(modisTileId, nrows, ncols){
  thtv <- as.numeric(getHV(modisTileId))
  iGpid <- thtv[1] * nrows
  jGpid <- thtv[2] * ncols
  return(c(iGpid, jGpid))
}



#' @title Get the adquisition time of a MODIS HDF file name
#' @name getTimeFromHdfFilename
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get the adquisition time of a MODIS HDF file name
#'
#' @param hdfFilename HDF filename
#' @return Character. A date in the format year and day of the year YYYYDOY
#' @export
getTimeFromHdfFilename <- function(hdfFilename){
  fileNameParts <- unlist(strsplit(hdfFilename, split = "[.]"))
  return (substr(fileNameParts[2], 2, nchar(fileNameParts[2])))
}



#' @title Compute the coordinates of the center of the given pixels
#' @name getxyMatrix
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Return the coords (MODIS Sinusoidal SR-ORG:6974) of the center of the given pixel
#'
#' @param colrowid.Matrix A numeric matrix with 2 columns: col_id and row_id
#' @param pixelSize Pixel size in meters
#' @return A 2-column matrix
#' @examples
#' lonlat.mat <- matrix(data = c(-6790774.88,-853140.53,-6786172.69,-847462.74),
#'               nrow = 2, byrow = TRUE)
#' getxyMatrix(lonlat.mat, calcPixelSize(4800, calcTileWidth()))
#' @export
getxyMatrix <- function(colrowid.Matrix, pixelSize){
  x <- vector(mode = "numeric", length = length(nrow(colrowid.Matrix)))
  y <- vector(mode = "numeric", length = length(nrow(colrowid.Matrix)))
  corner.ul.x <- -20015109.354
  corner.ul.y <- 10007554.677
  x <- corner.ul.x + (pixelSize/2) + (colrowid.Matrix[,1] * pixelSize)
  y <- corner.ul.y - (pixelSize/2) - (colrowid.Matrix[,2] * pixelSize)
  return(cbind(x,y))
}



#' @title Calculate the MODIS's tile index from the array's spatial indexes
#' @name ids2tile
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Calculate the MODIS's tile index from the given array's spatial indexes
#'
#' @param col_id Array's col ID
#' @param row_id Array's row ID
#' @param nrows Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
#' @param ncols Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
#' @return A list with 2 numeric values
#' @export
ids2tile <- function(col_id, row_id, nrows, ncols){
  th <- as.integer(col_id/nrows)
  tv <- as.integer(row_id/ncols)
  return(c(th, tv))
}



#' @title Get the col_id & row_id in MODIS sinusoidal coordinates
#' @name sinusoidal2gmpi
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Calculate the col_id & row_id corresponding to the given MODIS sinusoidal coordinates
#'
#' @param lonlat.mat A numeric matrix with 2 columns: lon and lat on MODIS sinusoidal coordinates
#' @param pixelSize Pixel size in meters
#' @return A 2-column matrix (col_id and row_id)
#' @export
sinusoidal2gmpi <- function(lonlat.mat, pixelSize){
  col_id <- vector(mode = "numeric", length = length(nrow(lonlat.mat)))
  row_id <- vector(mode = "numeric", length = length(nrow(lonlat.mat)))
  # Upper left corner of MODIS CRS
  corner.ul.x <- -20015109.354
  corner.ul.y <- 10007554.677
  # distance to origin
  dx <- lonlat.mat[,1] - corner.ul.x
  dy <- corner.ul.y - lonlat.mat[,2]
  # gmpi
  col_id <- trunc(dx %/% pixelSize - 1)
  row_id <- trunc(dy %/% pixelSize - 1)
  return(cbind(col_id, row_id))
}



#' @title Get the col_id & row_id in WGS84 coordinates
#' @name wgs84gmpi
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Calculate the col_id & row_id corresponding to the given WGS84 coordinates
#'
#' @param lonlat.mat A numeric matrix with 2 columns: lon and lat on WGS84
#' @param pixelSize Pixel size in meters
#' @return A 2-column matrix (col_id and row_id)
#' @examples
#' bbox <- matrix(c(minLon = -61.8061, maxLon = -61.16994, minLat = -8.110165,
#'                  maxLat = -7.628022), ncol = 2)
#' wgs84gmpi(lonlat.mat = bbox, pixelSize = calcPixelSize(4800, calcTileWidth()))
#' @export
wgs84gmpi <- function(lonlat.mat, pixelSize){
  proj4326 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  proj_modis_sinusoidal <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  S <- sp::SpatialPoints(lonlat.mat)
  sp::proj4string(S) <- sp::CRS(proj4326)
  llmat <- sp::spTransform(S, sp::CRS(proj_modis_sinusoidal))
  res <- sinusoidal2gmpi(llmat@coords, pixelSize)
  rownames(res) <- NULL
  colnames(res) <- c('col_id', 'row_id')
  return(res)
}



#' @title Get the MODIS tile id from the modis filename
#' @name getTileIdFromFilename
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get the MODIS tile id from the modis filename
#'
#' @param fileName Name of the file
#' @return The name of the file
#' @export
getTileIdFromFilename <- function(fileName){
  tmp <- unlist(strsplit(fileName, split = "[.]"))
  return(tmp[3])
}



#' @title Transform a time_id into dates
#' @name time_id2date
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description DEPRECATED. Transform a time_id into dates
#'
#' @param time_id.vector Vector of time indexes
#' @param period days between images (MOD09Q1 is 8, MOD13Q1 is 16)
#' @return a list of Date objects
#' @export
time_id2date <- function(time_id.vector, period){
  ydoy <- sapply(time_id.vector, FUN = time_id2ydoy, period = period)
  res <- lapply(ydoy, FUN = ydoy2date)
  return(res)
}



#' @title Add position columns to MODIS data retrieved from a SciDB's 3D array
#' @name addPosition
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Add position columns to MODIS data retrieved from a SciDB's 3D array
#'
#' @param sdbdf A data frame made of MODIS data. The ID columns must be named as "col_id", "row_id", and "time_id"
#' @param period Number of days between observations (e.g 8)
#' @param startyear Initial year of the index (e.g 2000)
#' @return A data frame with additional columns
#' @export
addPosition <- function(sdbdf, period, startyear){
  pixelSize <- calcPixelSize(4800, calcTileWidth())
  # get unique positions from the data
  cr.id <- unique(sdbdf[c("col_id", "row_id")])
  t.id <- unique(sdbdf["time_id"])
  # add MODIS SINUSOIDAL coordinates
  xy.sin <- cbind(cr.id, getxyMatrix(as.matrix(cr.id), pixelSize))
  xy.sin["crid"] <- apply(xy.sin[ , c("col_id", "row_id")] , 1 , paste , collapse = "-" )
  xy.sin["col_id"] <- xy.sin["row_id"] <- NULL
  # add year-day-of-the-year
  t.ydoy <- cbind(t.id, grid2date(unlist(t.id), period, startyear))
  colnames(t.ydoy)[2] <- "ydoy"
  sdbdf["crid"] <- apply(sdbdf[ , c("col_id", "row_id")] , 1 , paste , collapse = "-" )
  sdbdf <- merge(sdbdf, t.ydoy, by = "time_id")
  sdbdf <- merge(sdbdf, xy.sin, by ="crid")
  # add dates from ydoy
  if("cdoy" %in% names(sdbdf)){ # uses the reported DOY when available
    sdbdf["ydoy"] <- (floor(sdbdf["ydoy"] / 1000) * 1000) + sdbdf["cdoy"]
  }
  sdbdf["datetime"] <- ydoy2date(unlist(sdbdf["ydoy"]))
  return(sdbdf)
}



#' @title Calculate the length of a MODIS pixel
#' @name calcPixelSize
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Calculate the length of a MODIS pixel. Resolution is the number of pixel in one dimension (e.g 4800)
#'
#' @param resolution Square root of the number of pixels on an image
#' @param tileWidth Width of a tile
#' @return A number
#' @export
calcPixelSize <- function(resolution, tileWidth){
  #https://code.env.duke.edu/projects/mget/wiki/SinusoidalMODIS
  #earth.radius <- 6371007.181 # MODIS synusoidal parameter - SPHERICAL EARTH!
  #tile.rows <- resolution#4800
  #tile.cols <- tile.rows
  return(tileWidth / resolution)
}



#' @title Calculate the width of a MODIS tile
#' @name calcTileWidth
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Calculate the width of a MODIS tile
#'
#' @return A number
#' @export
calcTileWidth <- function(){
  #https://code.env.duke.edu/projects/mget/wiki/SinusoidalMODIS
  modisHtiles <- 36
  #modisVtiles <- 18
  corner.ul.x <- -20015109.354
  #corner.ul.y <- 10007554.677
  corner.lr.x <- 20015109.354
  #corner.lr.y <- -10007554.677
  tile.width <- (corner.lr.x - corner.ul.x) / modisHtiles
  #tile.height <- (corner.lr.y - corner.ul.y) / modisVtiles
  #tile.height <- tile.width # Tiles seem to be squared
  return(tile.width)
}







#' @title Transform a date into a time_id index
#' @name ymd2tid
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Calculate the width of a MODIS tile
#'
#' @param ymd    An int YYYYMMDD
#' @param origin An int YYYYMMDD. The day when the time_id == 0
#' @param period An int. The number of days between observations
#' @param yearly A boolean. Do the dates yearly match January the 1st?
#' @return       An integer. The time_id matching ymd or 0 is ymd doesn't match
#' @export
ymd2tid <- function(ymd, origin, period, yearly){
  # image, origin, period, yearly
  # [MOD09Q1, 20000101, 8, True]
  # [MOD13Q1, 20000101, 16, True]
  # [LD5Original-DigitalNumber, 19840411, 16, False]
  # [LD8Original-DigitalNumber, 20130418, 16, False]
  res = 0
  dy = 0
  # cast YYYYDDMMs to Date
  tymd <- scidbutil::ymd2ymd(ymd)
  ymdy <- tymd[1]
  ymdm <- tymd[2]
  ymdd <- tymd[3]
  oymd <- scidbutil::ymd2ymd(origin)
  ory <- oymd[1]
  orm <- oymd[2]
  or_d <- oymd[3]
  dtymd <- as.Date(ISOdate(ymdy, ymdm, ymdd))
  dtor <- as.Date(ISOdate(ory, orm, or_d))
  if(yearly){
    dy <- 1 + 365 %/% period                                                    # periods per year
  }
  dtor <- as.Date(ISOdate(ymdy, 1, 1))
  ndays <- as.integer(difftime(dtymd, dtor, units = "days"))                    # days from origin to ymd
  if(ndays %% period == 0){
    res = ndays/period + (ymdy - ory) * dy
  }
  return(res)
}
