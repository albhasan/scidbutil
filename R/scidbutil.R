#---- SCIDB FUNCTIONS ----

#---- TODO: ----

#---- public functions ----



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
  # transform to SciDB dimension indexes
  stid <- ymd2tid(ymd = start, origin = origin, period = period, yearly = yearly)
  etid <- ymd2tid(ymd = end, origin = origin, period = period, yearly = yearly)
  lonlat.mat <- wgs84gmpi(lonlat.mat = lonlat.mat, pixelSize = pixelSize)
  # build the bounding box matrix
  bb.mat <- cbind(lonlat.mat, c(stid, etid))
  rownames(bb.mat) <- NULL
  colnames(bb.mat) <- c(colnames(lonlat.mat), "time_id")
  bb.mat[, 1] <- sort(bb.mat[, 1])
  bb.mat[, 2] <- sort(bb.mat[, 2])
  bb.mat[, 3] <- sort(bb.mat[, 3])
  # run the AFl query
  afl <- .sdb_between(arrayname = arrayname, bb.mat = bb.mat)
  return(scidb::iquery(db = con, query = afl, return = TRUE, binary = FALSE))
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
  crid <- scidbutil::wgs84gmpi(as.matrix(samples.df[lonlat]), pixelSize)
  samples.df["col_id"] <- crid[, 1]
  samples.df["row_id"] <- crid[, 2]
  res <- list()
  for(i in 1:nrow(crid)){
    cr <- crid[i, ]
    afl <- paste("between(", paste(arrayname, cr[1], cr[2], 0, cr[1], cr[2], 500, sep =',' ), ")", sep = "")
    res[[i]] <- list(sample = samples.df[i, ], time_series = scidb::iquery(con, afl, return = TRUE, binary = FALSE))
  }
  return(res)
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
  arrayname <- substr(schema, start = 1, stop = which(strsplit(schema, "")[[1]]=="<")[1] - 1)
  attdim <- vector(mode = "character", length = 2)
  attdim[1] <- substr(schema, start = which(strsplit(schema, "")[[1]]=="<")[1], stop = which(strsplit(schema, "")[[1]]==">")[1])
  attdim[2] <- substr(schema, start = which(strsplit(schema, "")[[1]]=="[")[1], stop = which(strsplit(schema, "")[[1]]=="]")[1])
  attdef <- unlist(strsplit(x = removeFisrtLast(attdim[1]), split = ","))
  dimdef <- unlist(strsplit(x = removeFisrtLast(attdim[2]), split = ","))
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
  return(list(arrayName = arrayname, dimensions = dim.df, attributes = att.df))
}



#---- hidden functions ----


# Build a SciDB between query
#
# @param arrayname  A string. The name of the array
# @param bb.mat     A 2xd matrix. The bounding box. The rows are the minimum and maximum dimension ids. d is the number of dimensions
.sdb_between <- function(arrayname, bb.mat){
  ind <- paste(paste(bb.mat[1,], collapse = ","), paste(bb.mat[2,], collapse = ","), sep = ",")
  return(paste("between(", arrayname, ",", ind, ")", sep = ""))
}
