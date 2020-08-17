# Contents of file `test-epi_df.R'

## Tests both epi_check and epi_converge
context("epi_df")

good_x <- c(1, 2, 3, 4, 5, 6, 7)
good_y <- c(2, 3, 4, 5, 6, 7, 8)
good_map <- 'china'
good_rate <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 1)
good_case <- c(1000, 2000, 3000, 4000, 
                 5000, 6000, 10000)
good_population <- c(10000, 20000, 30000, 
                       40000, 50000, 60000, 100000)

missing_x <- c(NA, 2, 3, 4, 5, 6, 7)
missing_y <- c(2, 3, 4, NA, 6, 7, 8)
missing_rate <- c(0.1, 0.2, 0.3, NA, 0.5, 0.6, 1)
bad_map <- 'japan'
bad_rate <- c(0.1, 2, 1, 1, 1, 1.1, 1)
missing_case <- c(1000, 2000, 3000, 4000, 
                  5000, NA, 10000)
bad_case <- c(1000, -0.001, 3000, 4000, 
                  -5000, 6000, -10000)
missing_population <- c(10000, NA, 30000, 
                        40000, 50000, 60000, 100000)
bad_population <- c(-10000, 20000, -30000, 
                       40000, 50000, -60000, 100000)
zero_population <- c(10000, 20000, 0, 
                    40000, 50000, 60000, 100000)

zero_case <- c(1000, 2000, 0, 4000, 
                5000, 6000, 10000)

null <- NULL ## No data entered; of course not a valid EpiVis df.

## The coordinates and rates are valid, check if is a data frame
## This is the closet one, no need to test others
non_df <- matrix(c(1, 2), c(2, 3), c(0.3, 0.4)) 

## There is a missing coordinate, cannot be a valid EpiVis df.
non_eql_len <- data.frame(x=c(1,2), y=c(1,NA), rate=c(.1, .2))

## Map one wish to plot is not supported, cannot be a valid EpiVis df.
non_map <- data.frame(x=c(1, 2), y=c(2, 3), rate=c(.2, .3), map='japan')

## Not a valid EpiVis df if there is a missing parameter.
missing_par_map <- data.frame(x=c(1,2), y=c(2, 3), rate=c(.2, .3))
missing_par_rate <- data.frame(x=c(1, 2), y=c(2, 3), map='china')
missing_par_x <- data.frame(y=c(2, 3), rate=c(.2, .3), map='china')
missing_par_y <- data.frame(x=c(1, 2), rate=c(.2, .3), map='china')

## Rate >= implies that there are more people infected than total number of people.
non_valid_rate <- data.frame(x=c(1,2), y=c(2, 3), rate=c(1.01, .3), map='china')
valid_epi <- data.frame(x=c(1,2), y=c(2, 3), rate=c(1, .3), map='china')
superset_epi <- data.frame(x=c(1,2), y=c(2, 3), rate=c(1, .3), case=c(100, 300), map='china')

fail_test_cases <- list(non_df, non_eql_len, non_map,
                        non_valid_rate, missing_par_y,
                        missing_par_map, missing_par_x,
                        missing_par_rate, null)
success_test_cases <- list(valid_epi, superset_epi)

result <- TRUE
for (ii in fail_test_cases){
  if(epi_check(ii) != FALSE)
    result <- FALSE; break
}

for (ii in success_test_cases){
  if(epi_check(ii) != TRUE)
    result <- FALSE; break
}
result

