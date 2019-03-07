library(MyfarsPkg)
context("make_filename")
test_that("invalid years result in an error",
          {
            expect_error(make_filename(lakgjg),"object 'lakgjg' not found")
          })
