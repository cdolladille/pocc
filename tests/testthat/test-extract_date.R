test_that("file date is the good one", {
  good_date <- as.character(Sys.Date())

  tf <-
    tempfile()

   write.csv2(mtcars, file = tf)

   ed_date <-
     extract_date(tf)

   expect_equal(good_date, ed_date)

   file.remove(tf)
})
