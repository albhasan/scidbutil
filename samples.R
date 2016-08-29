############################################################################
# SCIDBUTIL - example
# 2016-08-23
############################################################################

library(sp)
source("/scidbUtil.R")

# calculate the size of a pixel
pixelSize <- .calcPixelSize(4800, .calcTileWidth())

# spatial reference system objects
proj4326 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj_modis_sinusoidal <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

# create a matrix of points
x <- c(-56.325531, -56.325531, -56.239700, -56.239700)
y <- c(-11.623990, -11.676781, -11.676781, -11.623990)
lonlat.Matrix <- cbind(x, y)

# build spatial-points
S <- SpatialPoints(lonlat.Matrix)
proj4string(S) <-CRS(proj4326)

# transforms SRS to MODIS
lonlat.Matrix.res <- spTransform(S, CRS(proj_modis_sinusoidal))

# transform MODIS to SciDB's col_id, row_id
res <- .sinusoidal2gmpi(t(bbox(lonlat.Matrix.res)), pixelSize)
res

# NOTE: Due to origin differences in the SRS (MODIS sinusoidal versus SciDB's col_id, row_id), the min & max values are inverted

  