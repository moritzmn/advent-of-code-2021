#~~~~~~~~~~~~~~
##Day 8
#~~~~~~~~~~~~~~


# data --------------------------------------------------------------------
#test
raw_data <- readLines(con = "data/day_eight_test")
raw_data <- strsplit(raw_data, split = "[|]")

output_signals <- lapply(raw_data, function(x) {
  unlist(strsplit(x[2], split = " "))[-1]
  })

# 1, 4, 7, 8 signals

count_chars <- lapply(output_signals, function(x){
  sum(nchar(x) %in% c(2, 3, 4, 7))
  
})
sum(unlist(count_chars))

#puzzle part 1
raw_data <- readLines(con = "data/day_eight_one")
raw_data <- strsplit(raw_data, split = "[|]")

output_signals <- lapply(raw_data, function(x) {
  unlist(strsplit(x[2], split = " "))[-1]
})

# 1, 4, 7, 8 signals

count_chars <- lapply(output_signals, function(x){
  sum(nchar(x) %in% c(2, 3, 4, 7))
  
})
sum(unlist(count_chars))

#part 2
patterns <- c("cagedb","ab", "gcdfa", "fbcad", "eafb", "cdfbe", "cdfgeb", "dab", "acedgfb", "cefabd")

patterns_sorted <- sapply(patterns, function(x){
  unname(paste(sort(strsplit(x, split = "")[[1]]), collapse = ""))
})

output_sorted <- lapply(output_signals, function(x){
  sapply(x, function(y){
    paste(sort(strsplit(y, split = "")[[1]]), collapse = "") 
  })
  
})


match(output_sorted[[100]], patterns_sorted) 

test <- c("fdgacbe", "cefdb", "cefbgd", "gcbe")
test_sorted <- sapply(test, function(x) {
  paste(sort(strsplit(x, split = "")[[1]]), collapse = "") 
})
match(test_sorted, patterns_sorted)
