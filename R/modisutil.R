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

