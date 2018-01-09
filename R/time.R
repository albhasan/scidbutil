
#---- TODO: ----
# - What is sampleTS for?
# - remove the deprecated functions
# - document parametes of .ydoy2dateHelper


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
  test <- min(tid):max(tid)
  return(setdiff(test, tid))
}



#' @title Estimate the values for the time-series for the supplied sample times
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
  y <- floor(yyyydoy / 1000)
  doy <- floor(yyyydoy - y * 1000)
  ymd <- .ydoy2dateHelper2(year = y, doy = doy)
  res <- ymd['year'] * 10000  + ymd['month'] * 100 + ymd['day']
  names(res) <- NULL
  return(res)
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
  for(i in 1:length(Date1)){
    originY <- as.numeric(format(Date1[i], format = "%Y"))
    originM <- format(Date1[i], format = "%m")
    originD <- format(Date1[i], format = "%d")
    newYear <- originY + numberOfYears
    Date1[i] <- as.Date(paste(newYear, originM, originD, sep = "-"), origin = "1970-01-01")
  }
  return(Date1)
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
  return(paste(y, mtt, dtt, sep = "-"))
}



#' @title Text to POSIXlt
#' @name text2date
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Transform a text date given as text to a date object.
#'
#' @param dateAsText Date as string
#' @return A date object (POSIXlt)
#' @export
text2date <- function(dateAsText){
  if(nchar(dateAsText) == 7){# YYYYDOY
    d <- ydoy2date(dateAsText)
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
#' @return       An integer vector. The time_id matching ymd or 0 (the first id) is ymd doesn't match
#' @export
ymd2tid <- function(ymd, origin, period, yearly){
  # image, origin, period, yearly
  # [MOD09Q1, 20000101, 8, True]
  # [MOD13Q1, 20000101, 16, True]
  # [LD5Original-DigitalNumber, 19840411, 16, False]
  # [LD8Original-DigitalNumber, 20130319, 16, False]
  return(unlist(lapply(ymd, .ymd2tid, origin = origin, period = period, yearly = yearly)))
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
#' @return       An integer vector. The time_id matching ymd or 0 (the first id) is ymd doesn't match
#' @export
ymd2tid_approx <- function(ymd, origin, period, yearly){
  # image, origin, period, yearly
  # [MOD09Q1, 20000101, 8, True]
  # [MOD13Q1, 20000101, 16, True]
  # [LD5Original-DigitalNumber, 19840411, 16, False]
  # [LD8Original-DigitalNumber, 20130319, 16, False]
  return(unlist(lapply(ymd, .ymd2tid_approx, origin = origin, period = period, yearly = yearly)))
}



#' @title Split a date into parts
#' @name ymd2ymd
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Split a date into its numeric parts
#'
#' @param ymd    An int YYYYMMDD
#' @return       A list of 3 integer vectors (year, month, day)
#' @export
ymd2ymd <- function(ymd){
  res <- as.data.frame(do.call(rbind, lapply(ymd, .ymd2ymd)), stringsAsFactors = F)
  return(list(year = res$year, month = res$month, day = res$day))
}



#---- DEPRECATED ----


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
  res <- vector(mode = "numeric", length = length(time_id))
  ppy = trunc((365 / period)) + 1 # Periods per year
  ys <- (trunc(time_id / ppy) + as.numeric(startyear)) * 1000
  mod <- time_id %% ppy
  res = ys + (mod * period + 1)
  return(res)
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
  freqperyear <- round(365/period)
  YYYY <- as.integer(time_id / freqperyear) + 2000
  tid <- as.numeric(time_id)
  if(tid < freqperyear){
    DOY <- tid * period
  }else{
    DOY <- (tid)%%freqperyear * period
  }
  return(YYYY * 1000 + DOY + 1)
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
  return (as.Date(charDates, origin = "1970-01-01"))
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
  leapyear <- sapply(year, .isLeapYearHelper)
  return (leapyear)
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
  d <- text2date(dateAsText)
  yearOriginText <- paste(format(d, "%Y"), "/01/01", sep="")
  yearOrigin <- as.POSIXlt(yearOriginText)
  doy <- as.numeric(as.Date(d, origin = "1970-01-01") - as.Date(yearOrigin, origin = "1970-01-01")) + 1
  res <- paste(format(d, "%Y"), sprintf("%03d", doy), sep="")
  return(res)
}



#---- util ----


# Split a YYYYMMDD date into its parts
#
# @param ymd    An int YYYYMMDD
# @return       An int vector with the year, month, and day
.ymd2ymd <- function(ymd){
  y <- ymd %/% 10000
  m <- (ymd - y * 10000) %/% 100
  d <- (ymd - y * 10000 - m * 100)
  return(c(year = y, month = m, day = d))
}


# Is the given year a leap year?
#
# @param year An int. The year
# @return     A logical
.isLeapYearHelper <- function(year){
  leapyear <- FALSE
  if (year %% 4 != 0){
    leapyear <- FALSE
  }else if (year %% 100 != 0){
    leapyear <- TRUE
  }else if (year %% 400 == 0){
    leapyear <- TRUE
  }
  return(leapyear)
}


# Transform a year and day-of-the-year to a integer vector
#
# @param year   An int. A year
# @param doy    An int. A day of the year (January the 1st is doy 1)
# @return       An int vector. The year, the month and the day
.ydoy2dateHelper2 <- function(year, doy){
  firstdayRegular <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  firstdayLeap    <- c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367)
  if(.isLeapYearHelper(year)){
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
  return(c(year = year, month = month, day = day)) # return(paste(year, month, day, sep = "/"))
}



.ydoy2dateHelper <- function(i, year.vec, doy.vec){
  ymd <- .ydoy2dateHelper2(year = year.vec[i], doy = doy.vec[i])
  return(paste(ymd['year'], ymd['month'], ymd['day'], sep = "/"))
}




.ymd2tid <- function(ymd, origin, period, yearly){
  res = 0
  dy = 0
  # cast YYYYDDMMs to numbers
  ymd.dvec <- .ymd2ymd(ymd)
  origin.dvec <- .ymd2ymd(origin)
  dtymd <- as.Date(paste(ymd.dvec['year'], ymd.dvec['month'], ymd.dvec['day'], sep = "/"), origin = "1970-01-01")
  dtor <- as.Date(paste(origin.dvec['year'], origin.dvec['month'], origin.dvec['day'], sep = "/"), origin = "1970-01-01")
  if(yearly){
    dy <- round(365/period)                                                     # periods per year
    dtor <- as.Date(paste(ymd.dvec['year'], 1, 1, sep = "/"), origin = "1970-01-01")
  }
  ndays <- as.integer(difftime(time1 = dtymd, time2 = dtor, units = "days"))    # days from origin to ymd
  if(ndays %% period == 0){
    res <- ndays/period + (ymd.dvec['year'] - origin.dvec['year']) * dy
    names(res) <- "time_id"
  }
  return(res)
}



.ymd2tid_approx <- function(ymd, origin, period, yearly){
  res = 0
  dy = 0
  # cast YYYYDDMMs to numbers
  ymd.dvec <- .ymd2ymd(ymd)
  origin.dvec <- .ymd2ymd(origin)
  dtymd <- as.Date(paste(ymd.dvec['year'], ymd.dvec['month'], ymd.dvec['day'], sep = "/"), origin = "1970-01-01")
  dtor <- as.Date(paste(origin.dvec['year'], origin.dvec['month'], origin.dvec['day'], sep = "/"), origin = "1970-01-01")
  if(yearly){
    dy <- round(365/period)                                                     # periods per year
    dtor <- as.Date(paste(ymd.dvec['year'], 1, 1, sep = "/"), origin = "1970-01-01")
  }
  ndays <- as.integer(difftime(time1 = dtymd, time2 = dtor, units = "days"))    # days from origin to ymd
  #if(ndays %% period == 0){
  res <- ndays/period + (ymd.dvec['year'] - origin.dvec['year']) * dy
  names(res) <- "time_id"
  #}
  return(res)
}
