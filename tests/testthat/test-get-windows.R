library(dplyr)
test_that("Window function works horizonally for 4x1", {
     M <- matrix(data = 1:60,6,10)
     my_data <- get_windows(M,"H")
     # check that each element is larger than the previous one.
     my_test <- (as.numeric(my_data$w)<lead(as.numeric(my_data$w)))
     expect_equal(sum(my_test,na.rm = TRUE),41)
})

test_that("Window function works vertically  4x1", {
  M <- matrix(data = 1:60,6,10, byrow = TRUE)
  my_data <- get_windows(M,"V")
  # check that each element is larger than the previous one.
  my_test <- (as.numeric(my_data$w)<lead(as.numeric(my_data$w)))
  expect_equal(sum(my_test,na.rm = TRUE),(30-1))
})
