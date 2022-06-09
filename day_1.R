#~~~~~~~~~~~~~~
##Day 1
#~~~~~~~~~~~~~~


# Part one ----------------------------------------------------------------

## Data --------------------------------------------------------------------

sonar_data <- read.csv(file = "data/day_one_sonar",  col.names = "depth", header = FALSE)
head(sonar_data)
nrow(sonar_data)

## Analyze -----------------------------------------------------------------

diff_depth <- diff(sonar_data$depth)
head(diff_depth)
length(diff_depth)
sum_inc <- sum(diff_depth > 0)
sum_inc

shift_sum <- vector(mode = "numeric", length = nrow(sonar_data) - 2)
for(i in seq_along(shift_sum)) {
  shift_sum[i] <- sum(sonar_data[i : (i + 2), "depth"])
}

sum(diff(shift_sum) > 0)