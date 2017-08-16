#library(scidbutil)
#library(testthat)



tid <- .ymd2tid(ymd = 20000101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid(ymd = 20000101, origin = 20000101, period = 8, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid(ymd = 20000101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid(ymd = 20000101, origin = 20000101, period = 16, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid(ymd = 20010101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- .ymd2tid(ymd = 20010101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 23)

tid <- .ymd2tid(ymd = 20020101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 92)

tid <- .ymd2tid(ymd = 20020101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- .ymd2tid(ymd = 20010103, origin = 20000101, period = 8, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- .ymd2tid(ymd = 20010103, origin = 20000101, period = 16, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 23)







dvec <- .ymd2ymd(ymd = 20000101)
testthat::expect_equal(as.vector(dvec), expected = c(2000, 1, 1))

dvec <- .ymd2ymd(ymd = 20001231)
testthat::expect_equal(as.vector(dvec), expected = c(2000, 12, 31))

dvec <- .ymd2ymd(ymd = 20000231)
testthat::expect_equal(as.vector(dvec), expected = c(2000, 02, 31))






yyyymmdd <- .ydoy2ymd(yyyydoy = 2000001)
testthat::expect_equal(as.vector(yyyymmdd), expected = c(20000101))

yyyymmdd <- .ydoy2ymd(yyyydoy = 2001001)
testthat::expect_equal(as.vector(yyyymmdd), expected = c(20010101))
