#library(scidbutil)
#library(testthat)



tid <- ymd2tid(ymd = 20000101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid(ymd = 20000101, origin = 20000101, period = 8, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid(ymd = 20000101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid(ymd = 20000101, origin = 20000101, period = 16, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid(ymd = 20010101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- ymd2tid(ymd = 20010101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 23)

tid <- ymd2tid(ymd = 20020101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 92)

tid <- ymd2tid(ymd = 20020101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- ymd2tid(ymd = 20010103, origin = 20000101, period = 8, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- ymd2tid(ymd = 20010103, origin = 20000101, period = 16, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 23)



tid <- ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 8, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 16, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid_approx(ymd = 20010101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- ymd2tid_approx(ymd = 20010101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 23)

tid <- ymd2tid_approx(ymd = 20020101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 92)

tid <- ymd2tid_approx(ymd = 20020101, origin = 20000101, period = 16, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- ymd2tid_approx(ymd = 20010103, origin = 20000101, period = 8, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 46)

tid <- ymd2tid_approx(ymd = 20010103, origin = 20000101, period = 16, yearly = F)
testthat::expect_equal(as.vector(tid), expected = 23)

tid <- ymd2tid_approx(ymd = 20000101, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0)

tid <- ymd2tid_approx(ymd = 20000102, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.125)

tid <- ymd2tid_approx(ymd = 20000103, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.25)

tid <- ymd2tid_approx(ymd = 20000104, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.375)

tid <- ymd2tid_approx(ymd = 20000105, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.5)

tid <- ymd2tid_approx(ymd = 20000106, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.625)

tid <- ymd2tid_approx(ymd = 20000107, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.75)

tid <- ymd2tid_approx(ymd = 20000108, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 0.875)

tid <- ymd2tid_approx(ymd = 20000109, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1)

tid <- ymd2tid_approx(ymd = 20000110, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.125)

tid <- ymd2tid_approx(ymd = 20000111, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.25)

tid <- ymd2tid_approx(ymd = 20000112, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.375)

tid <- ymd2tid_approx(ymd = 20000113, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.5)

tid <- ymd2tid_approx(ymd = 20000114, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.625)

tid <- ymd2tid_approx(ymd = 20000115, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.75)

tid <- ymd2tid_approx(ymd = 20000116, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 1.875)

tid <- ymd2tid_approx(ymd = 20000117, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 2)

tid <- ymd2tid_approx(ymd = 20000118, origin = 20000101, period = 8, yearly = T)
testthat::expect_equal(as.vector(tid), expected = 2.125)








yyyymmdd <- ydoy2ymd(yyyydoy = 2000001)
testthat::expect_equal(as.vector(yyyymmdd), expected = c(20000101))

yyyymmdd <- ydoy2ymd(yyyydoy = 2001001)
testthat::expect_equal(as.vector(yyyymmdd), expected = c(20010101))





arrayname <-  "MOD13Q1"
bb.mat <- matrix(c(1,11,111,2,22,222), nrow = 2, byrow = T)
res <- .sdb_between(arrayname = arrayname, bb.mat = bb.mat)
testthat::expect_equal(res, "between(MOD13Q1,1,11,111,2,22,222)")





ymd2tid(ymd = 20010101, origin = 20000101, period = 16, yearly = TRUE)





testthat::expect_equal(ymd2ymd(20000101), list(year = 2000, month = 01, day = 01))
testthat::expect_equal(ymd2ymd(20000231), list(year = 2000, month = 02, day = 31))
testthat::expect_equal(ymd2ymd(20001231), list(year = 2000, month = 12, day = 31))
testthat::expect_equal(ymd2ymd(20100101), list(year = 2010, month = 01, day = 01))
testthat::expect_equal(ymd2ymd(20101231), list(year = 2010, month = 12, day = 31))







testthat::expect_equal(as.vector(ymd2tid(20000101, 20000101, 8, T)), 0)
testthat::expect_equal(as.vector(ymd2tid(20000109, 20000101, 8, T)), 1)
testthat::expect_equal(as.vector(ymd2tid(20000117, 20000101, 8, T)), 2)
testthat::expect_equal(as.vector(ymd2tid(20000102, 20000101, 8, T)), 0)
testthat::expect_equal(as.vector(ymd2tid(20000108, 20000101, 8, T)), 0)
testthat::expect_equal(as.vector(ymd2tid(20000110, 20000101, 8, T)), 0)

testthat::expect_equal(as.vector(ymd2tid(20000101, 20000101, 16, T)), 0)
testthat::expect_equal(as.vector(ymd2tid(20000117, 20000101, 16, T)), 1)
testthat::expect_equal(as.vector(ymd2tid(20000202, 20000101, 16, T)), 2)
testthat::expect_equal(as.vector(ymd2tid(20000102, 20000101, 16, T)), 0)
testthat::expect_equal(as.vector(ymd2tid(20000116, 20000101, 16, T)), 0)
testthat::expect_equal(as.vector(ymd2tid(20000118, 20000101, 16, T)), 0)

testthat::expect_equal(as.vector(ymd2tid(20000101, 20000101, 16, T)), 0)
testthat::expect_equal(as.vector(ymd2tid(20001218, 20000101, 16, T)), 22)
testthat::expect_equal(as.vector(ymd2tid(20010101, 20000101, 16, T)), 23)
testthat::expect_equal(as.vector(ymd2tid(20011219, 20000101, 16, T)), 45)
testthat::expect_equal(as.vector(ymd2tid(20020101, 20000101, 16, T)), 46)


