library(dplyr)
test_that("We can generate data for the 1x4 matrix, i.e. the parallel space", {
  my_data <- generate_parallel_training_data()
  test_data <- my_data |> 
    filter(State %in% c("1101","1011"))
  expect_equal(sum(test_data$Reward),-2)
  test_data <- my_data |> 
    filter(!State %in% c("1101","1011")) |> 
    filter(Action == "Pass")
  expect_equal(sum(test_data$Reward),nrow(test_data))
  test_data <- my_data |> 
    filter(!State %in% c("1101","1011")) |> 
    filter(Action != "Pass")
  expect_equal(sum(-test_data$Reward),nrow(test_data))
  
})
