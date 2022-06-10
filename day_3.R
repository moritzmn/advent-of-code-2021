#~~~~~~~~~~~~~~
##Day 3
#~~~~~~~~~~~~~~


# solution ----------------------------------------------------------------

# * part one --------------------------------------------------------------------

bin_data <- read.csv(file = "data/day_three", col.names = "bin", header = FALSE, colClasses = "character")

bin_m <- matrix(data = as.integer(unlist(strsplit(bin_data[["bin"]], split = ""))), ncol = 12, byrow = TRUE)


gamma   <- as.integer(colSums(bin_m) > nrow(bin_data)/2)
epsilon <- as.integer(colSums(bin_m) < nrow(bin_data)/2)

strtoi(paste(gamma, collapse = ""), base = 2)
strtoi(paste(epsilon, collapse = ""), base = 2)

(answer <- strtoi(paste(gamma, collapse = ""), base = 2) * strtoi(paste(epsilon, collapse = ""), base = 2))


# * part two ----------------------------------------------------------------

i <- 1L
while(i <= ncol(bin_m)) {
  filter_m <- ifelse(sum(bin_m[, i]) >= nrow(bin_m)/2, 1, 0)
  bin_m    <- bin_m[bin_m[, i] == filter_m,]
  i <- i + 1L
  if(is.null(nrow(bin_m))) break
}
ox <- strtoi(paste(bin_m, collapse = ""), base = 2)

bin_m   <- matrix(data = as.integer(unlist(strsplit(bin_data[["bin"]], split = ""))), ncol = 12, byrow = TRUE)
i <- 1L
while(i <= ncol(bin_m)) {
  filter_m <- ifelse(sum(bin_m[, i]) >= nrow(bin_m)/2, 0, 1)
  bin_m    <- bin_m[bin_m[, i] == filter_m,]
  i <- i + 1L
  if(is.null(nrow(bin_m))) break
}
co_2 <- strtoi(paste(bin_m, collapse = ""), base = 2)

(answer <- co_2 * ox)

# test --------------------------------------------------------------------

# * part one --------------------------------------------------------------------

bin_data <- read.csv(file = "data/day_three_test", col.names = "bin", header = FALSE, colClasses = "character")
bin_m   <- matrix(data = as.integer(unlist(strsplit(bin_data[["bin"]], split = ""))), ncol = 5, byrow = TRUE)

gamma   <- as.integer(colSums(bin_m) > nrow(bin_data)/2)
epsilon <- as.integer(colSums(bin_m) < nrow(bin_data)/2)

strtoi(paste(gamma, collapse = ""), base = 2)
strtoi(paste(epsilon, collapse = ""), base = 2)

# * part two ----------------------------------------------------------------

i <- 1L
while(i <= ncol(bin_m)) {
  filter_m <- ifelse(sum(bin_m[, i]) >= nrow(bin_m)/2, 1, 0)
  bin_m    <- bin_m[bin_m[, i] == filter_m,]
  i <- i + 1L
  if(is.null(nrow(bin_m))) break
}
ox <- strtoi(paste(bin_m, collapse = ""), base = 2)

bin_m   <- matrix(data = as.integer(unlist(strsplit(bin_data[["bin"]], split = ""))), ncol = 5, byrow = TRUE)
i <- 1L
while(i <= ncol(bin_m)) {
  filter_m <- ifelse(sum(bin_m[, i]) >= nrow(bin_m)/2, 0, 1)
  bin_m    <- bin_m[bin_m[, i] == filter_m,]
  i <- i + 1L
  if(is.null(nrow(bin_m))) break
}
co_2 <- strtoi(paste(bin_m, collapse = ""), base = 2)

answer <- co_2 * ox
