#---- SCIDB FUNCTIONS ----



#' @title Get data over a SciDB connection of a bounding box
#' @name getSdbDataFromBB
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get data over a SciDB connection of a bounding box
#'
#' @param con                A SciDB connection object
#' @param arrayname          A string. The name of the array
#' @param pixelSize          A number. The length of one side of a pixel
#' @param lonlat.mat         A 2x2 matrix. The 2 columns are the WGS84 longitude, and WGS84 latitude
#' @param start              An integer. The start date as YYYYMMDD
#' @param end                An integer. The end date as YYYYMMDD
#' @param origin             An integer. A YYYYMMDD date. The day when the time_id == 0
#' @param period             An integer. The number of days between observations
#' @param yearly             A logical Do the dates yearly match January the 1st?
#' @return                   A data frame
#' @export
getSdbDataFromBB <- function(con, arrayname, pixelSize, lonlat.mat, start, end,
                             origin,period, yearly){
  return(.getSdbDataFromBB(con = con, arrayname = arrayname,
                           pixelSize = pixelSize, lonlat.mat = lonlat.mat,
                           start = start, end = end, origin = origin,
                           period = period, yearly = yearly))
}



#' @title Get SciDB data from sample points
#' @name getSdbDataFromPoints
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get data over a SciDB connection of a data.frame of sample points
#'
#' @param samples.df A data.frame of samples. It must have 2 columns with the WGS84 longitude and latitude of the sample
#' @param lonlat     A character vector. The names of the columns in samples.df that contain the  WGS84 longitude and latitude
#' @param con        A SciDB connection object
#' @param arrayname  A string. The name of the array
#' @param pixelSize  A number. The length of one side of a pixel
#' @return           A list of lists. Each inner list has 2 elements: A row of samples.df and the results of the SciDB query for the longitude and latitude
#' @export
getSdbDataFromPoints <- function(samples.df, lonlat, con, arrayname, pixelSize){
  return(.getSdbDataFromPoints(samples.df = samples.df, lonlat = lonlat, con = con, arrayname = arrayname, pixelSize = pixelSize))
}



#---- TIME FUNCTIONS ----



#' @title Transform a date into an exactly time_id index
#' @name ymd2tid
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Transform a date into an exactly time_id index
#'
#' @param ymd    An int. A YYYYMMDD date
#' @param origin An int. A YYYYMMDD date. The day when the time_id == 0
#' @param period An int. The number of days between observations
#' @param yearly A boolean. Do the dates yearly match January the 1st?
#' @return       An integer. The time_id matching ymd or 0 (the first id) is ymd doesn't match
#' @export
ymd2tid <- function(ymd, origin, period, yearly){
  return(.ymd2tid(ymd = ymd, origin = origin, period = period, yearly = yearly))
}



#' @title Transform a date into an time_id index
#' @name ymd2tid_approx
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Transform a date into an time_id index
#'
#' @param ymd    An int. A YYYYMMDD date
#' @param origin An int. A YYYYMMDD date. The day when the time_id == 0
#' @param period An int. The number of days between observations
#' @param yearly A boolean. Do the dates yearly match January the 1st?
#' @return       An integer. The time_id matching ymd or 0 (the first id) is ymd doesn't match
#' @export
ymd2tid_approx <- function(ymd, origin, period, yearly){
  .ymd2tid_approx(ymd = ymd, origin = origin, period = period, yearly = yearly)
}



#' @title Transform year-day-of-the-year into a date
#' @name ydoy2ymd
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Transform year-day-of-the-year into a date
#'
#' @param yyyydoy    An int YYYYDOY
#' @return           An integer YYYYMMDD
#' @export
ydoy2ymd <- function(yyyydoy){
  return(.ydoy2ymd(yyyydoy = yyyydoy))
}



#---- OLD TIME FUNCTIONS ----



#' @title year-day-of-the-year to time_id
#' @name date2grid
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description DEPRECATED. Return a time index (timid) from the input date (MODIS DOY) and time period (e.g 8 days).
#'
#' @param dateDOY Input day in year and day-of-the-year format (e.g 2001032 is Febraury the 2nd of 2001)
#' @param period Number of days between observations (e.g 8)
#' @param startyear Initial year of the index (e.g 2000)
#' @return A number
#' @export
date2grid <- function(dateDOY, period, startyear){
  return(.date2grid(dateDOY = dateDOY, period = period, startyear = startyear))
}



#' @title Date to year-day-of-the-year
#' @name date2ydoy
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description DEPRECATED. Transforms a date into the year-day_of_the_year date (YYYYDOY).
#'
#' @param dateAsText Date represented as a text string
#' @return Character representing a date as day-of-the-year (YYYYDOY)
#' @export
date2ydoy <- function(dateAsText){
  return(.date2ydoy(dateAsText = dateAsText))
}




#' @title time_id to year-day-of-the-year
#' @name grid2date
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description DEPRECATED. Return a year and day-of-the-year from the given time_id.
#'
#' @param time_id Input time index
#' @param period Number of days between observations (e.g 8)
#' @param startyear Initial year of the index (e.g 2000)
#' @return A number vector  representing a date in the format year and day-of-the-year format (e.g 2001032 is Febraury the 2nd of 2001)
#' @export
grid2date <- function(time_id, period, startyear){
  return(.grid2date(time_id = time_id, period = period, startyear = startyear))
}



#' @title Test for leap year
#' @name isLeapYear
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description DEPRECATED. Is the given year is a leap year?
#'
#' @param year Numeric year
#' @return TRUE is the year is leap, FALSE otherwise
#' @export
isLeapYear <- function(year){
  return(.isLeapYear(year = year))
}



#' @title Move dates certain number of years
#' @name moveDateByYears
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Move a Date object a certain number of years, i.e 2000-10-31 moved 5 years becomes 2005-10-31
#'
#' @param Date1 A list made of Date objects
#' @param numberOfYears An integer number representing a of years
#' @return Date1
#' @export
moveDateByYears <- function(Date1, numberOfYears){
  return(.moveDateByYears(Date1 = Date1, numberOfYears = numberOfYears))
}



#' @title Format a POSIXlt object
#' @name POSIXlt2txt
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Format a POSIXlt object
#'
#' @param aPOSIXlt A date object
#' @return A string
#' @export
POSIXlt2txt <- function(aPOSIXlt){
  return(.POSIXlt2txt(aPOSIXlt = aPOSIXlt))
}



#' @title Estimate the values fo the time-series for the supplied sample times
#' @name sampleTS
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Estimate the values fo the time-series for the supplied sample times
#'
#' @param  ts.df data.frame with 2 columns: time and value
#' @param  sampletime TODO
#' @return a vector of sampled values
#' @export
sampleTS <- function(ts.df, sampletime){
  return(.sampleTS(ts.df = ts.df, sampletime = sampletime))
}



#' @title Text to POSIXlt
#' @name text2date
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Transforms a text date given as text to a date object.
#'
#' @param dateAsText Date as string
#' @return A date object (POSIXlt)
#' @export
text2date <- function(dateAsText){
  return(.text2date(dateAsText = dateAsText))
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
  return(.time_id2date(time_id.vector = time_id.vector, period = period))
}



#' @title Transform a time_id into year-day_of_the_year
#' @name time_id2ydoy
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description DEPRECATED. Transform a time_id into year-day_of_the_year
#'
#' @param time_id A time id
#' @param period days between images (MOD09Q1 is 8, MOD13Q1 is 16)
#' @return A number
#' @export
time_id2ydoy <- function(time_id, period){
  return(.time_id2ydoy(time_id = time_id, period = period))
}



#' @title year-day_of_the_year to a date
#' @name ydoy2date
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description DEPRECATED. Transform a date in the year-day_of_the_year format to a date
#'
#' @param YYYYDOY Numeric or character with 4 digits for the year and 3 for the day of the year (i.e 2012324)
#' @return A date object
#' @export
ydoy2date <- function(YYYYDOY){
  return(.ydoy2date(YYYYDOY = YYYYDOY))
}



#---- SPACE FUNCTIONS ----



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
  return(.addPosition(sdbdf = sdbdf, period = period, startyear = startyear))
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
  return(.calcPixelSize(resolution = resolution, tileWidth = tileWidth))
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
  return(.calcTileWidth())
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
  return(.getHV(modisTileId = modisTileId))
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
  return(.getFirstGmip(modisTileId = modisTileId, nrows = nrows, ncols = ncols))
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
  return(.getTimeFromHdfFilename(hdfFilename = hdfFilename))
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
  return(.getxyMatrix(colrowid.Matrix = colrowid.Matrix, pixelSize = pixelSize))
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
  return(.ids2tile(col_id = col_id, row_id = row_id, nrows = nrows, ncols = ncols))
}



#' @title Report the missing time_ids
#' @name missingtids
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Report the missing time_ids
#'
#' @param tid A vector of time ids
#' @return A vector with the missing time ids between the maximum and minimum time id provided
#' @export
missingtids <- function(tid){
  return(.missingtids(tid = tid))
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
  return(.sinusoidal2gmpi(lonlat.mat = lonlat.mat, pixelSize = pixelSize))
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
  return(.wgs84gmpi(lonlat.mat = lonlat.mat, pixelSize = pixelSize))
}



#---- FILE NAME PROCESSING ----



#' @title Get the filename from the path to the file
#' @name getFilenameFromFilepath
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Return the filename of the path to the file
#'
#' @param filepath Character representing the full path to the file
#' @return Character representing the filename including the file extension
#' @export
getFilenameFromFilepath <- function(filepath){
  return(.getFilenameFromFilepath(filepath = filepath))
}



#' @title Get the filepath without the last part (filename)
#' @name getFilepathFromFilepath
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Return the filepath of the path without the last part (filename)
#'
#' @param filepath Character representing the full path to the file
#' @return Character representing the filepath without the file name
#' @export
getFilepathFromFilepath <- function(filepath){
  return(.getFilepathFromFilepath(filepath = filepath))
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
  return(.getTileIdFromFilename(fileName = fileName))
}



#---- OTHER ----



#' @title Removes the first and last characters from a string
#' @name removeFisrtLast
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Removes the first and last characters from a single string
#'
#' @param x A string
#' @return A string
#' @export
removeFisrtLast <- function(x){
  return(.removeFisrtLast(x = x))
}



#' @title Parse a SciDB's array schema
#' @name scidbProcessSchema
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Parse a SciDB's array schema
#'
#' @param schema A string. Array schema - "<red:int16,nir:int16,quality:uint16> [col_id=48000:67199,1014,5,row_id=38400:52799,1014,5,time_id=0:9200,1,0]"
#' @return A list containing 2 data.frame
#' @export
scidbProcessSchema <- function(schema){
  return(.scidbProcessSchema(schema = schema))
}



#' @title Get the characters of a string from right to left
#' @name substrRight
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get the characters of a string from right to left
#'
#' @param x A string
#' @param n The number of chars
#' @return The requested characters
#' @export
substrRight <- function(x, n){
  return(.substrRight(x = x, n = n))
}
