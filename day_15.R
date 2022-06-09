#~~~~~~~~~~~~~~
##Day 15
#~~~~~~~~~~~~~~

raw_data <- readLines(con = "data/day_15_test")

integer_split <- lapply(raw_data, function(x){
  as.integer(strsplit(x, split = "")[[1]])
})

m <- do.call(rbind, integer_split)

