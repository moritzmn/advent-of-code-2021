#~~~~~~~~~~~~~~
##Day 5
#~~~~~~~~~~~~~~


# solution --------------------------------------------------------------------

string_vec <- readLines(con = "data/day_five_one")
string_vec <- unlist(strsplit(string_vec, split = "->"))
string_vec <- trimws(string_vec)
string_vec <- as.integer(unlist(strsplit(string_vec, split = ",")))
matrix_vec <- matrix(data = string_vec, ncol = 4, byrow = TRUE)
matrix_vec <- matrix_vec[(matrix_vec[, 1] == matrix_vec[, 3]) | (matrix_vec[, 2] == matrix_vec[, 4]),]

ini_mat <- matrix(0L, nrow = 1000, ncol = 1000)

for(i in 1 : nrow(matrix_vec)){
  x_ind <- matrix_vec[i, 1] : matrix_vec[i, 3]
  y_ind <- matrix_vec[i, 2] : matrix_vec[i, 4]
  ini_mat[x_ind, y_ind] <- ini_mat[x_ind, y_ind] + 1
}

length(ini_mat[ini_mat > 1])

matrix_vec <- matrix(data = string_vec, ncol = 4, byrow = TRUE)

matrix_vec <- matrix_vec[(matrix_vec[, 1] != matrix_vec[, 3]) & (matrix_vec[, 2] != matrix_vec[, 4]),]
matrix_vec <- cbind(matrix_vec, (matrix_vec[, 4] - matrix_vec[, 2])/(matrix_vec[, 3]- matrix_vec[, 1]))
matrix_vec <- cbind(matrix_vec, matrix_vec[, 2] - (matrix_vec[, 1] * matrix_vec[, 5]))

for(i in 1 : nrow(matrix_vec)){
  print(i)
  x_ind <- matrix_vec[i, 1] : matrix_vec[i, 3]
  y_ind <- x_ind * matrix_vec[i, 5] + matrix_vec[i, 6]
  ini_mat[cbind(x_ind, y_ind)] <- ini_mat[cbind(x_ind, y_ind)] + 1
}
length(ini_mat[ini_mat > 1])

# test --------------------------------------------------------------------

string_vec <- readLines(con = "data/day_five_test")
string_vec <- unlist(strsplit(string_vec, split = "->"))
string_vec <- trimws(string_vec)
string_vec <- as.integer(unlist(strsplit(string_vec, split = ","))) + 1
matrix_vec <- matrix(data = string_vec, ncol = 4, byrow = TRUE)

matrix_vec <- matrix_vec[(matrix_vec[, 1] != matrix_vec[, 3]) & (matrix_vec[, 2] != matrix_vec[, 4]),]
matrix_vec <- cbind(matrix_vec, (matrix_vec[, 4] - matrix_vec[, 2])/(matrix_vec[, 3]- matrix_vec[, 1]))
matrix_vec <- cbind(matrix_vec, matrix_vec[, 2] - (matrix_vec[, 1] * matrix_vec[, 5]))

ini_mat <- matrix(0L, nrow = 10, ncol = 10)

for(i in 1 : nrow(matrix_vec)){
  print(i)
  x_ind <- matrix_vec[i, 1] : matrix_vec[i, 3]
  y_ind <- x_ind * matrix_vec[i, 5] + matrix_vec[i, 6]
  ini_mat[cbind(x_ind, y_ind)] <- ini_mat[cbind(x_ind, y_ind)] + 1
}
length(ini_mat[ini_mat > 1])
