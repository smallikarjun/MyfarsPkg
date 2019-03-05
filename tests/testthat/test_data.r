test_that(".csv data files are available", {
  testthat::expect_equal(list.files(system.file("data", package = "MyfarsPkg")),
               c("accident_2013.csv.bz2",
                 "accident_2014.csv.bz2",
                 "accident_2015.csv.bz2"))
})