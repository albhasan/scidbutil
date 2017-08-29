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



tid <- .ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 8, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 16, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid_approx(ymd = 20010101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- .ymd2tid_approx(ymd = 20010101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 23)

tid <- .ymd2tid_approx(ymd = 20020101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 92)

tid <- .ymd2tid_approx(ymd = 20020101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- .ymd2tid_approx(ymd = 20010103, origin = 20000101, period = 8, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- .ymd2tid_approx(ymd = 20010103, origin = 20000101, period = 16, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 23)

tid <- .ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- .ymd2tid_approx(ymd = 20000102, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.125)

tid <- .ymd2tid_approx(ymd = 20000103, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.25)

tid <- .ymd2tid_approx(ymd = 20000104, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.375)

tid <- .ymd2tid_approx(ymd = 20000105, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.5)

tid <- .ymd2tid_approx(ymd = 20000106, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.625)

tid <- .ymd2tid_approx(ymd = 20000107, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.75)

tid <- .ymd2tid_approx(ymd = 20000108, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.875)

tid <- .ymd2tid_approx(ymd = 20000109, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1)

tid <- .ymd2tid_approx(ymd = 20000110, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.125)

tid <- .ymd2tid_approx(ymd = 20000111, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.25)

tid <- .ymd2tid_approx(ymd = 20000112, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.375)

tid <- .ymd2tid_approx(ymd = 20000113, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.5)

tid <- .ymd2tid_approx(ymd = 20000114, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.625)

tid <- .ymd2tid_approx(ymd = 20000115, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.75)

tid <- .ymd2tid_approx(ymd = 20000116, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.875)

tid <- .ymd2tid_approx(ymd = 20000117, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 2)

tid <- .ymd2tid_approx(ymd = 20000118, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 2.125)



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





arrayname <-  "MOD13Q1"
bb.mat <- matrix(c(1,11,111,2,22,222), nrow = 2, byrow = T)
res <- .sdb_between(arrayname = arrayname, bb.mat = bb.mat)
testthat::expect_equal(res, "between(MOD13Q1,1,11,111,2,22,222)")





.ymd2tid(ymd = 20010101, origin = 20000101, period = 16, yearly = TRUE)
