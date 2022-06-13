#~~~~~~~~~~~~~~
##Day 13
#~~~~~~~~~~~~~~


# solution ----------------------------------------------------------------

FoldMatrix <- function(m, ax_is = "y", value) {
  if(ax_is == "y") {
    #fold y
    y <- value
    
    y_y <- min(max(seq(nrow(m) - y)), max(y - seq(y)))
    where_one <- m[y + seq(y_y), ] == 1 
    
    m[y-seq(y_y), ][where_one] <- 1
    
    m <- m[-c(y : nrow(m)),]
  } else {
    y <- value
    
    y_y <- min(max(seq(ncol(m) - y)), max(y - seq(y)))
    where_one <- m[,y + seq(y_y)] == 1 
    
    m[,y - seq(y_y)][where_one] <- 1
    
    m <- m[,-c(y:ncol(m))]
  }
  return(m)
}


mat <- as.matrix(read.csv(file = "data/day_thirteen_paper", header = FALSE))
dot_mat <- matrix(data = NA, ncol = max(mat[, 1] + 1), nrow = max(mat[, 2] + 1))
dot_mat[cbind(mat[, 2] + 1 , mat[, 1] + 1)] <- 1
sum(dot_mat, na.rm = TRUE)


# * part one ---------------------------------------------------------------------


folded_m <- FoldMatrix(dot_mat, ax_is = "x", value = 656)
(answer <- sum(folded_m, na.rm = TRUE))


# * part two --------------------------------------------------------------

instructions <- read.csv(file = "data/day_thirteen_instructions", sep = " ", header = FALSE)
instructions <- instructions[["V3"]]
instructions <- strsplit(instructions, split = "=")

instructions <- lapply(instructions, function(x) {
  return(data.frame("axis" = x[1], "value" = as.integer(x[2]) + 1))
})


for(i in seq(instructions)){
  ins_set <- instructions[[i]]
  dot_mat <- FoldMatrix(dot_mat, ax_is = ins_set[, 1], value = ins_set[, 2])
  print(dim(dot_mat))
  
}
dot_mat


sum(dot_mat, na.rm = TRUE)


# test --------------------------------------------------------------------

set.seed(1401)
m <- sample(c(1,0), 25, replace = TRUE)
m <- matrix(m, nrow = 5)

#fold y
y <- 4L

y_y <- min(max(seq(nrow(m) - y)), max(y - seq(y)))
where_one <- m[y + y_y, ] == 1 

m[y-y_y, ][where_one] <- 1

m <- m[-c(y, y + y_y),]

#fold x
set.seed(1401)
m <- sample(c(1,0), 25, replace = TRUE)
m <- matrix(m, nrow = 5)



#FoldMatrix(m, value = 4L, ax_is = "x")

#test
mat <- as.matrix(read.csv(file = "data/day_thirteen_test", header = FALSE))
dot_mat <- matrix(data = NA, ncol = max(mat[,1] + 1), nrow = max(mat[,2] + 1))
dot_mat[cbind(mat[, 2] + 1, mat[,1] + 1)] <- 1
sum(dot_mat, na.rm = TRUE)

folded_m <- FoldMatrix(dot_mat, ax_is = "y", value = 11)
sum(folded_m, na.rm = TRUE)
folded_m <- FoldMatrix(folded_m, ax_is = "x", value = 3)
sum(folded_m, na.rm = TRUE)
#part1



