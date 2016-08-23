#*******************************************************
# SCIDB UTIL
#*******************************************************



# Get the adquisition time of a MODIS HDF file name
#
# @param hdfFilename HDF filename
# @return Character. A date in the format year and day of the year YYYYDOY
.getTimeFromHdfFilename <- function(hdfFilename){
  fileNameParts <- unlist(strsplit(hdfFilename, split = "[.]"))
  res <- substr(fileNameParts[2], 2, nchar(fileNameParts[2]))
  return (res)
}



# Return the tileH and tileV from a MODIS tile Id
#
# @param modisTileId A character with a MODIS tile id (i.e "h10v08")
# @return A character vector of 2 elements c(tH, tV)
.getHV <- function(modisTileId){
  tH <- substr(modisTileId, 2, 3)
  tV <- substr(modisTileId, 5, 6)
  res <- c(tH, tV)
  return(res)
} 



# Return the filename of the path to the file
#
# @param filepath Character representing the full path to the file
# @return Character representing the filename including the file extension
.getFilenameFromFilepath <- function(filepath){
  filePathParts <- unlist(strsplit(filepath, split = "/"))
  res <- filePathParts[length(filePathParts)]
  return(res)
}



# Return the filepath of the path witout the last part (filename)
#
# @param filepath Character representing the full path to the file
# @return Character representing the filepath without the file name
.getFilepathFromFilepath <- function(filepath){
  filePathParts <- unlist(strsplit(filepath, split = "/"))
  res <- filePathParts[-length(filePathParts)]
  res <- paste0(res, sep = '/', collapse="")
  res <- substr(res, 1, nchar(res) - 1)
  return(res)
}



# Get the MODIS tile id from the modis filename
#
# @param fileName Name of the file
# @return The name of the file
.getTileIdFromFilename <- function(fileName){
  tmp <- unlist(strsplit(fileName, split = "[.]"))
  res <- tmp[3]
  return(res)
}



# Return the GMPI of the first pixel (top left) of the given MODIS tile
#
# @param modisTileId A character with a MODIS tile id (i.e "h10v08")
# @param nrows Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
# @param ncols Number of rows in a MODIS image (i.e for MOD09Q1 is 4800)
# @return Numeric vector containing the c(i,j) pixel coordinates in th GMPI
.getFirstGmip <- function(modisTileId, nrows, ncols){
  thtv <- as.numeric(.getHV(modisTileId))
  iGpid <- thtv[1] * nrows
  jGpid <- thtv[2] * ncols
  res <- c(iGpid, jGpid)
}



# Get the MODIS tile id from the modis filename
#
# @param fileName Name of the file
# @return The name of the file
.getTileIdFromFilename <- function(fileName){
  tmp <- unlist(strsplit(fileName, split = "[.]"))
  res <- tmp[3]
  return(res)
}



# Removes the first and last character from a single string
#
# @param x A string
# @return A string
.removeFisrtLast <- function(x){
  substring(x, first = 2, last = nchar(x) - 1)
}



# Transforms a date given as text to a date object
#
# @param dateAsText Date as a text string
# @return A date object (POSIXlt)
.text2date <- function(dateAsText){
  if(nchar(dateAsText) == 7){# YYYYDOY
    d <- .ydoy2date(dateAsText)
  }else if(nchar(dateAsText) == 8){# YYYYMMDD
    yyyy <- substr(dateAsText, 1, 4)
    mm <- substr(dateAsText, 5, 6)
    dd <- substr(dateAsText, 7, 8)
    d <- as.POSIXlt(paste(yyyy, mm, dd, sep = "/"))
  }else if(nchar(dateAsText) == 10 & length(grep(".", dateAsText, fixed=TRUE, value=TRUE)) > 0){# YYYY.MM.DD
    d <- as.POSIXlt(gsub("[.]", "/", dateAsText))
  }else{
    d <- as.POSIXlt(dateAsText)
  }
  res <- d
  return(res)
}



# Transforms a date into the year-day_of_the_year date (YYYYDOY)
#
# @param dateAsText Date as a text string
# @return Character representing a date as day-of-the-year (YYYYDOY)
.date2ydoy <- function(dateAsText){
  d <- .text2date(dateAsText)
  yearOriginText <- paste(format(d, "%Y"), "/01/01", sep="")
  yearOrigin <- as.POSIXlt(yearOriginText)
  doy <- as.numeric(as.Date(d) - as.Date(yearOrigin)) + 1
  res <- paste(format(d, "%Y"), sprintf("%03d", doy), sep="")
  return(res)
}



# Find if the given year is a leap year
#
# @param year NUmeric year
# @return TRUE is the year is leap, FALSE otherwise
.isLeapYear <- function(year){
  leapyear <- sapply(year, .isLeapYearHelper)
  return (leapyear)
}
.isLeapYearHelper <- function(year){
  leapyear <- FALSE
  if (year %% 4 != 0){
    leapyear <- FALSE
  }else if (year %% 100 != 0){
    leapyear <- TRUE
  }else if (year %% 400 == 0){
    leapyear <- TRUE
  }
}


# Transform a date in the year-day_of_the_year format to a date
#
# @param YYYYDOY Numeric or character with 4 digits for the year and 3 for the day of the year (i.e 2012324)
# @return A date object
.ydoy2date <- function(YYYYDOY){
  #http://disc.gsfc.nasa.gov/julian_calendar.shtml
  res <- ""
  if(is.numeric(YYYYDOY)){
    year.vec <- YYYYDOY %/% 1000
    doy.vec <- YYYYDOY - (year.vec * 1000)
  }else if(is.character(YYYYDOY)){
    year.vec <- as.numeric(substr(YYYYDOY, 1, 4))
    doy.vec <- as.numeric(substr(YYYYDOY, 5, 7))
  }else{
    stop("Unexpected datatype")
  }
  if (!(doy.vec > 0 && doy.vec < 367)){
    stop("Invalid day-of-the-year interval")
  }
  charDates <- sapply(1:length(YYYYDOY), .ydoy2dateHelper, year.vec = year.vec, doy.vec = doy.vec)
  return (as.Date(charDates))
}
.ydoy2dateHelper <- function(i, year.vec, doy.vec){
  year <- year.vec[i]
  doy <- doy.vec[i]
  firstdayRegular <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  firstdayLeap    <- c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367)
  if(.isLeapYear(year)){
    firstday <- firstdayLeap
  }else{
    firstday <- firstdayRegular
  }
  for (i in 1:(length(firstday) - 1)){
    start <- firstday[i]
    end <- firstday[i + 1]
    if(doy >= start && doy < end){
      month <- i
      break
    }
  }
  day <- doy - firstday[month] + 1
  return(paste(year, month, day, sep = "/"))
}


# Transforms a date into the year-day_of_the_year date (YYYYDOY)
#
# @param dateAsText Date as a text string
# @return Character representing a date as day-of-the-year (YYYYDOY)
.date2ydoy <- function(dateAsText){
  d <- .text2date(dateAsText)
  yearOriginText <- paste(format(d, "%Y"), "/01/01", sep="")
  yearOrigin <- as.POSIXlt(yearOriginText)
  doy <- as.numeric(as.Date(d) - as.Date(yearOrigin)) + 1
  res <- paste(format(d, "%Y"), sprintf("%03d", doy), sep="")
  return(res)
}



# Return a time index (timid) from the input date (MODIS DOY) and time period (e.g 8 days).
#
# @param dateDOY Input day in year and day-of-the-year format (e.g 2001032 is Febraury the 2nd of 2001)
# @param period Number of days between observations (e.g 8)
# @param startyear Initial year of the index (e.g 2000) 
# @return A number
.date2grid <- function(dateDOY, period, startyear){
  res = -1
  year = as.numeric(substr(x = dateDOY, start = 1, stop = 4))
  doy = as.numeric(substr(x = dateDOY, start = 5, stop = 7))
  ppy = round(365 / period) # Periods per year
  if(period > 0 && (doy - 1) %% period == 0){
    idd = (doy - 1) / period
    idy = (year - startyear) * ppy
    res = idy + idd
  }
  return(res)
}



# Return a year and day-of-the-year from the given time_id.
#
# @param time_id Input time index
# @param period Number of days between observations (e.g 8)
# @param startyear Initial year of the index (e.g 2000) 
# @return A number vector  representing a date in the format year and day-of-the-year format (e.g 2001032 is Febraury the 2nd of 2001)
.grid2date <- function(time_id, period, startyear){
  res <- vector(mode = "numeric", length = length(time_id))
  ppy = trunc((365 / period)) + 1 # Periods per year
  ys <- (trunc(time_id / ppy) + as.numeric(startyear)) * 1000
  mod <- time_id %% ppy
  res = ys + (mod * period + 1)  
  return(res)
}



# Get the characters of a string from right to left
# @param x A string
# @param n The number of chars
# @return The requested characters
.substrRight <- function(x, n){
  res <- substr(x, nchar(x)-n+1, nchar(x))
  return(res)
}



# Parse an array schema
#
# @param schema A string. Array schema - "<red:int16,nir:int16,quality:uint16> [col_id=48000:67199,1014,5,row_id=38400:52799,1014,5,time_id=0:9200,1,0]"
# @return A list containing 2 data.frame
.scidbProcessSchema <- function(schema){
  arrayname <- substr(schema, start = 1, stop = which(strsplit(schema, "")[[1]]=="<")[1] - 1)
  attdim <- vector(mode = "character", length = 2)
  attdim[1] <- substr(schema, start = which(strsplit(schema, "")[[1]]=="<")[1], stop = which(strsplit(schema, "")[[1]]==">")[1])
  attdim[2] <- substr(schema, start = which(strsplit(schema, "")[[1]]=="[")[1], stop = which(strsplit(schema, "")[[1]]=="]")[1])
  attdef <- unlist(strsplit(x = .removeFisrtLast(attdim[1]), split = ","))
  dimdef <- unlist(strsplit(x = .removeFisrtLast(attdim[2]), split = ","))
  atname <- character(0)
  attype <- character(0)
  atnull <- logical(0)
  for(el in attdef){
    el.split <- unlist(strsplit(x = el, split = ":"))
    atname <- c(atname, el.split[1])
    atcomp <- el.split[2]
    if(" " %in% atcomp){
      atcompsplit <- unlist(strsplit(x = atcomp, split = " "))
      attype <- c(attype, atcompsplit[1])
      atnull <- c(atnull, as.logical(atcompsplit[2]))
    }else{
      attype <- c(attype, atcomp)
      atnull <- c(atnull, FALSE)
    }
  }
  dmname <- character(0)
  dmstart <- numeric(0)
  dmend <- numeric(0)
  dmchunk <- numeric(0)
  dmover <- numeric(0)
  for(i in 1:length(dimdef)){
    it <- i %% 3
    if(it == 0){
      dmover  <- c(dmover, as.numeric(dimdef[i]))
    }else if(it == 1){
      tmp1 <- unlist(strsplit(dimdef[i], split = "="))
      dmname <- c(dmname, tmp1[1])
      tmp2 <- unlist(strsplit(tmp1[2], split = ":"))
      dmstart <- c(dmstart, as.numeric(tmp2[1]))
      dmend <- c(dmend, as.numeric(tmp2[2]))
    }else if(it == 2){
      dmchunk  <- c(dmchunk, as.numeric(dimdef[i]))
    }
  }
  att.df <- data.frame(name = atname, type = attype, nullable = atnull)  
  dim.df <- data.frame(name = dmname, start = dmstart, end = dmend, chunk = dmchunk, overlap = dmover, stringsAsFactors = FALSE)
  list(arrayName = arrayname, dimensions = dim.df, attributes = att.df)
}



# Format a POSIXlt object
#
# @param aPOSIXlt A date object
# @return A string
.POSIXlt2txt <- function(aPOSIXlt){
  y <- aPOSIXlt[["year"]] + 1900
  m <- aPOSIXlt[["mon"]] + 1
  d <- aPOSIXlt[["mday"]]
  mtt <- m
  dtt <- d
  if(nchar(as.character(m)) < 2){
    mtt <- paste("0", m, sep = "")
  }
  if(nchar(as.character(d)) < 2){
    dtt <- paste("0", d, sep = "")
  }
  paste(y, mtt, dtt, sep = "-")
}



# Transform a time_id into dates
#
# @param time_id.vector Vector of time indexes 
# @param period days between images (MOD09Q1 is 8, MOD13Q1 is 16)
# @return a list of Date objects
.time_id2date <- function(time_id.vector, period){
  ydoy <- sapply(time_id.vector, FUN = .time_id2ydoy, period = period)
  res <- lapply(ydoy, FUN = .ydoy2date)
  return(res)
}



# Transform a time_id into year-day_of_the_year
#
# @param time_id A time id
# @param freqperyear Number of time_ids a year
# @return A number
.time_id2ydoy <- function(time_id, period){
  freqperyear <- round(365/period)
  YYYY <- as.integer(time_id / freqperyear) + 2000
  tid <- as.numeric(time_id)
  if(tid < freqperyear){
    DOY <- tid * period
  }else{
    DOY <- (tid)%%freqperyear * period
  }
  YYYY * 1000 + DOY + 1
}



# Calculate the width of a MODIS tile
#
# @return A number
.calcTileWidth <- function(){
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



# Calculate the length of a MODIS pixel. Resolution is the number of pixel in one dimension (e.g 4800)
#
# @param resolution Square root of the number of pixels on an image
# @param tileWidth Width of a tile
# @return A number
.calcPixelSize <- function(resolution, tileWidth){
  
  #https://code.env.duke.edu/projects/mget/wiki/SinusoidalMODIS
  #earth.radius <- 6371007.181 # MODIS synusoidal parameter - SPHERICAL EARTH!
  #tile.rows <- resolution#4800
  #tile.cols <- tile.rows
  #---------------------
  cell.size <- tileWidth / resolution
}



# Calculate the MODIS's tile index from the given array's spatial indexes
#
# @param col_id Array's col ID
# @param row_id Array's row ID
# @return A list with 2 numeric values
.ids2tile <- function(col_id, row_id, nrows, ncols){
  th <- as.integer(col_id/nrows)
  tv <- as.integer(row_id/ncols)
  res <- c(th, tv)
}

.ids2tile.dummy <- function(colrow_id, samples.mat, nrows, ncols){
  vals <- samples.mat[colrow_id,]
  .ids2tile(col_id = vals[1], row_id = vals[2], nrows = nrows, ncols = ncols)
}



#Return the coords (MODIS synusoidal SR-ORG:6974) of the center of the given pixel
#
# @param colrowid.Matrix A numeric matrix with 2 columns: col_id and row_id
# @param pixelSize Pixel size in meters
# @return A 2-column matrix
.getxyMatrix <- function(colrowid.Matrix, pixelSize){
  x <- vector(mode = "numeric", length = length(nrow(colrowid.Matrix)))
  y <- vector(mode = "numeric", length = length(nrow(colrowid.Matrix)))
  corner.ul.x <- -20015109.354
  corner.ul.y <- 10007554.677
  x <- corner.ul.x + (pixelSize/2) + (colrowid.Matrix[,1] * pixelSize)
  y <- corner.ul.y - (pixelSize/2) - (colrowid.Matrix[,2] * pixelSize)
  cbind(x,y)
}



# Calculate the col_id & row_id corresponding to the given MODIS sinusoidal coordinates
#
# @param lonlat.Matrix A numeric matrix with 2 columns: lon and lat on MODIS sinusoidal coordinates
# @param pixelSize Pixel size in meters
# @return A 2-column matrix (col_id and row_id)
.sinusoidal2gmpi <- function(lonlat.Matrix, pixelSize){
  col_id <- vector(mode = "numeric", length = length(nrow(lonlat.Matrix)))
  row_id <- vector(mode = "numeric", length = length(nrow(lonlat.Matrix)))
  # Upper left corner of MODIS CRS
  corner.ul.x <- -20015109.354
  corner.ul.y <- 10007554.677
  # distance to origin
  dx <- lonlat.Matrix[,1] - corner.ul.x
  dy <- corner.ul.y - lonlat.Matrix[,2]
  # gmpi
  col_id <- trunc(dx %/% pixelSize - 1)
  row_id <- trunc(dy %/% pixelSize - 1)
  cbind(col_id, row_id)
}


# Add position columns to MODIS data retrieved from a SciDB's 3D array
#
# @param sdbdf A data frame made of MODIS data. The ID columns must be named as "col_id", "row_id", and "time_id"
# @return A data frame with additional columns
addPosition <- function(sdbdf, period, startyear){
  #sinus = CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  pixelSize <- .calcPixelSize(4800, .calcTileWidth())
  # get unique positions from the data
  cr.id <- unique(sdbdf[c("col_id", "row_id")])
  t.id <- unique(sdbdf["time_id"])
  # add MODIS SINUSOIDAL coordinates
  xy.sin <- cbind(cr.id, .getxyMatrix(as.matrix(cr.id), pixelSize))
  xy.sin["crid"] <- apply(xy.sin[ , c("col_id", "row_id")] , 1 , paste , collapse = "-" )
  xy.sin["col_id"] <- xy.sin["row_id"] <- NULL
  # add year-day-of-the-year
  t.ydoy <- cbind(t.id, .grid2date(unlist(t.id), period, startyear))
  colnames(t.ydoy)[2] <- "ydoy"
  sdbdf["crid"] <- apply(sdbdf[ , c("col_id", "row_id")] , 1 , paste , collapse = "-" )
  sdbdf <- merge(sdbdf, t.ydoy, by = "time_id")
  sdbdf <- merge(sdbdf, xy.sin, by ="crid")
  # add dates from ydoy
  if("cdoy" %in% names(sdbdf)){ # uses the reported DOY when available
    sdbdf["ydoy"] <- (floor(sdbdf["ydoy"] / 1000) * 1000) + sdbdf["cdoy"]
  }
  sdbdf["datetime"] <- .ydoy2date(unlist(sdbdf["ydoy"]))
  return(sdbdf)
}



# Move a Date object a certain number of years, i.e 2000-10-31 moved 5 years becomes 2005-10-31
#
# @param Date1 A list made of Date objects
# @param numberOfYears An integer number representing a of years
# @return Date1
moveDateByYears <- function(Date1, numberOfYears){
  for(i in 1:length(Date1)){
    originY <- as.numeric(format(Date1[i], format = "%Y")) 
    originM <- format(Date1[i], format = "%m")
    originD <- format(Date1[i], format = "%d")
    newYear <- originY + numberOfYears
    Date1[i] <- as.Date(paste(newYear, originM, originD, sep = "-"))
  }
  return(Date1)
}


# Report the missing time_ids
#
# @param tid A vector of time ids
# @return A vecor with the missing time ids between the maximum and minimum time id provided
missingtids <- function(tid){
  test <- min(tid):max(tid)
  return(setdiff(test, tid))
}



sampleTS <- function(ts.df, sampletime){
  # Estimate the values fo the time-series fopr the supplied sample times
  #
  # ts.df data.frame with 2 columns: time and value
  #t <-c(1,3,6, 9,20)
  #v <-c(100,103,104,108,109)
  #ts.df <- as.data.frame(cbind(t, v))
  #sampletime <- c(6,8,10,12,14,16,18,20,22,24)
  #sampletime <- 8:25
  #---------------
  #ts.df <- tsdf[, c("sampleDate", "evi")]
  #sampletime <- as.numeric(tsdf$tileDate)

  val <- vector(mode = "numeric", length = length(sampletime))
  for(i in 1:length(val)){
    val[i] <- NaN
  }
  for(rid in 2:nrow(ts.df)){
    r0 <- ts.df[rid - 1,]
    r1 <- ts.df[rid,]
    for(stid in 1:length(sampletime)){
      if(sampletime[stid] > r0[1] && sampletime[stid] <= r1[1]){
        x <- as.numeric(c(r0[1], r1[1])) # time
        y <- as.numeric(c(r0[2], r1[2])) # value
        m <- lm(y~x)      
        val[stid] <- predict.lm(m, newdata = data.frame(x = sampletime[stid]))
      }else if(sampletime[stid] == r0[1]){
        val[stid] <- r0[2]
      }
    }  
  }
  #plot(x = t, y = v, type = "l")
  #points(x = t, y = v)
  #lines(x = sampletime, y = val, col = "blue")
  return(unlist(val))
}


# Get the data used by Christopher Stephan on his thesis "Automating Near Real-Time Deforestation Monitoring With Satellite Image Time Series"
#
# @return Character. A date in the format year and day of the year YYYYDOY
getCSBFastData <- function(){
  library(scidb)
  scidbconnect(host = "localhost")
  #BETWEEN(MOD13Q1, 57084, 46857, 0, 57104, 46881, 400); --    191 100 cells
  #BETWEEN(MOD13Q1, 56995, 46840, 0, 57264, 47069, 400); -- 22 604 400 cells
  siteA <- iquery("BETWEEN(MOD13Q1, 57084, 46857, 0, 57104, 46881, 400);", `return` = TRUE, afl = TRUE, iterative = FALSE, n = Inf)
  save(siteA, file = "siteA.Rbin")
  rm(siteA)
  siteB <- iquery("BETWEEN(MOD13Q1, 56995, 46840, 0, 57264, 47069, 400);", `return` = TRUE, afl = TRUE, iterative = FALSE, n = Inf)
  save(siteB, file = "siteB.Rbin")
}
