#~~~~~~~~~~~~~~
##Day 2
#~~~~~~~~~~~~~~


# part one ----------------------------------------------------------------

## * test --------------------------------------------------------------------

dir_data <- read.csv(file = "data/day_two_test",  col.names = c("direction", "value"), 
                     header = FALSE, sep = " ")
head(dir_data)
nrow(dir_data)

#forward position
forward_pos <- sum(with(dir_data, value[direction == "forward"]))

#depth
depth_values <- within(with(dir_data, dir_data[direction != "forward",]), {
  depth_dir <- ifelse(direction == "up",  -value, value)
})

depth_pos <- sum(depth_values[, "depth_dir"])

## * solution --------------------------------------------------------------------

dir_data <- read.csv(file = "data/day_two",  col.names = c("direction", "value"), 
                     header = FALSE, sep = " ")
head(dir_data)

#forward position
forward_pos <- sum(with(dir_data, value[direction == "forward"]))

#depth
depth_values <- within(with(dir_data, dir_data[direction != "forward",]), {
  depth_dir <- ifelse(direction == "up",  -value, value)
})

depth_pos <- sum(depth_values[, "depth_dir"])
(answer <- depth_pos * forward_pos)


# part two ----------------------------------------------------------------

## * test --------------------------------------------------------------------

dir_data <- read.csv(file = "data/day_two_test",  col.names = c("direction", "value"), 
                     header = FALSE, sep = " ")

dir_data <- within(dir_data, {
  aim <- ifelse(direction != "forward", value, 0)
  aim <- ifelse(aim != 0 & direction == "up", -aim, aim)
  aim <- cumsum(aim)
  hor_change <- ifelse(direction == "forward", value, 0)
  depth_change <- ifelse(direction == "forward", aim * value, 0)
})

hor_pos <- sum(dir_data[["hor_change"]])
depth_pos <- sum(dir_data[["depth_change"]])

## * solution --------------------------------------------------------------------

dir_data <- read.csv(file = "data/day_two",  col.names = c("direction", "value"), 
                     header = FALSE, sep = " ")

dir_data <- within(dir_data, {
  aim <- ifelse(direction != "forward", value, 0)
  aim <- ifelse(aim != 0 & direction == "up", -aim, aim)
  aim <- cumsum(aim)
  hor_change <- ifelse(direction == "forward", value, 0)
  depth_change <- ifelse(direction == "forward", aim * value, 0)
})

hor_pos <- sum(dir_data[["hor_change"]])
depth_pos <- sum(dir_data[["depth_change"]])

(answer <- hor_pos * depth_pos)
