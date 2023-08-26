# Script to run Monte Carlo Simulation
library(xmas3)
library(dplyr)

set.seed(1234)
run_monte_carlo <- function(geometry = "3x3", iterations = 10, strategy="bestmove"){
  output_filename <- here::here("monte-carlo",paste0("mc-",geometry,"-",strategy,".rds"))
  message("Output file: ", output_filename)
  
  purrr::map(1:iterations,~{
    my_time <- system.time({
      B <- initialize_board()
      ret <- play_strategy(B,geometry,strategy)
    })
    ret$time <- my_time['elapsed']
    ret$geometry <- geometry
    ret$strategy <- strategy
    ret
  }) |> bind_rows() -> res
  message("Saving to: ", output_filename)
  readr::write_rds(res,output_filename)
}
n_iter <- 100*100 # equivalent of 100 games
run_monte_carlo('3x3',n_iter)
run_monte_carlo('3x4',n_iter)
run_monte_carlo('3x5',n_iter)
run_monte_carlo('4x4',n_iter)
run_monte_carlo('5x3',n_iter)
run_monte_carlo('5x3',n_iter)
run_monte_carlo('6x3',n_iter)

