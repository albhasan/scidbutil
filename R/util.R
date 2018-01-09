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
  filePathParts <- unlist(strsplit(filepath, split = "/"))
  return(filePathParts[length(filePathParts)])
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
  filePathParts <- unlist(strsplit(filepath, split = "/"))
  res <- filePathParts[-length(filePathParts)]
  res <- paste0(res, sep = '/', collapse="")
  res <- substr(res, 1, nchar(res) - 1)
  return(res)
}



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
  return(substring(x, first = 2, last = nchar(x) - 1))
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
  return(substr(x, nchar(x)-n+1, nchar(x)))
}



#' @title Split a date into parts
#' @name ymd2ymd
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Split a date into its numeric parts
#'
#' @param ymd    An int YYYYMMDD
#' @return       An integer vector (year, month, day)
#' @export
ymd2ymd <- function(ymd){
  y = ymd %/% 10000
  m = (ymd - y * 10000) %/% 100
  d = (ymd - y * 10000 - m * 100)
  return(c(year = y, month = m, day = d))
}



#---- OLD TIME FUNCTIONS ----



# Estimate the values for the time-series for the supplied sample times
#
# @param  ts.df data.frame with 2 columns: time and value
# @param  sampletime ????????????????????????????
# @return a vector of sampled values
.sampleTS <- function(ts.df, sampletime){
  # ts.df data.frame with 2 columns: time and value
  #t <-c(1,3,6, 9,20)
  #v <-c(100,103,104,108,109)
  #ts.df <- as.data.frame(cbind(t, v))
  #sampletime <- c(6,8,10,12,14,16,18,20,22,24)
  #sampletime <- 8:25
  #
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
        m <- stats::lm(y~x)
        val[stid] <- stats::predict.lm(m, newdata = data.frame(x = sampletime[stid]))
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






#---- OTHER ----



# Invert strings
#
# @param x            A vector made of strings
# @return Character   A vector where the characters of each string are inverted
.invertString <- function(x){
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}



# Cast a string to logical
#
# @param val  A character. It an take the values "0" or "1"
# @return     A logical. False if val is "0" and True if val is "1"
.tn2bool <- function(val){
  if(val == "0") return(FALSE)
  if(val == "1") return(TRUE)
  return(NA)
}



# ---- DEPRECATED ----



# Get the data used by Christopher Stephan on his thesis "Automating Near Real-Time Deforestation Monitoring With Satellite Image Time Series"
#
#.getCSBFastData <- function(){
#scidb::scidbconnect(host = "localhost")
##BETWEEN(MOD13Q1, 57084, 46857, 0, 57104, 46881, 400); --    191 100 cells
##BETWEEN(MOD13Q1, 56995, 46840, 0, 57264, 47069, 400); -- 22 604 400 cells
#siteA <- scidb::iquery("BETWEEN(MOD13Q1, 57084, 46857, 0, 57104, 46881, 400);", `return` = TRUE, afl = TRUE, iterative = FALSE, n = Inf)
#save(siteA, file = "siteA.Rbin")
#rm(siteA)
#siteB <- scidb::iquery("BETWEEN(MOD13Q1, 56995, 46840, 0, 57264, 47069, 400);", `return` = TRUE, afl = TRUE, iterative = FALSE, n = Inf)
#save(siteB, file = "siteB.Rbin")
#}



# # NOTE: DEPRECATED
# .ids2tile.dummy <- function(colrow_id, samples.mat, nrows, ncols){
#   vals <- samples.mat[colrow_id,]
#   .ids2tile(col_id = vals[1], row_id = vals[2], nrows = nrows, ncols = ncols)
# }
