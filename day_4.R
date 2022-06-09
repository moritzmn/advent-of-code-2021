#~~~~~~~~~~~~~~
##Day 4
#~~~~~~~~~~~~~~


# data --------------------------------------------------------------------

rand_n   <- read.csv(file = "data/day_four_seq", header = FALSE, sep = ",")
# rand_seq <- readChar("data/day_four_seq", nchars = rand_n * 2)
# rand_seq <- as.integer(unlist((strsplit(rand_seq, split = ","))))
rand_seq <- as.integer(rand_n[1,])
#read the matrices

mat_n <- lapply(1:100, function(x){
  as.matrix(read.csv(file = "data/day_four_mat", nrows = 5L, skip = (x - 1L) * 6L , 
                     sep = "", header = FALSE))
})

#who wins when
who_won <- lapply(mat_n, function(x) {
  mask <- matrix(FALSE, nrow = 5, ncol = 5)
  m    <- na.omit(arrayInd(match(rand_seq, x), .dim = dim(x)))
  mask[m[,]] <- TRUE
  row_sums <- rowSums(mask)
  col_sums <- colSums(mask)
  if(any(c(row_sums, col_sums) > 4)){
    
    if(any(row_sums > 4)) {
      row_mat <- x[which(row_sums > 4),,drop = FALSE]
      max_ind_row <- vector(mode = "integer", length = nrow(row_mat))
      for(i in 1:nrow(row_mat)) {
        max_ind_row[i] <- max(which(rand_seq %in% row_mat[i,])) 
      }
      max_ind_row <- min(max_ind_row)
    } else {
      max_ind_row <- NA
    }
    if(any(col_sums > 4)){
      col_mat <- x[,which(col_sums > 4), drop = FALSE]
      max_ind_col <- vector(mode = "integer", length = ncol(col_mat))
      for(i in 1:ncol(col_mat)) {
        max_ind_col[i] <- max(which(rand_seq %in% col_mat[,i])) 
      }
      max_ind_col <- min(max_ind_col)
    } else {
      max_ind_col <- NA
    } 
    min(na.omit(c(max_ind_row, max_ind_col)))
  } else {
    NA
  }
})
who_won <- unlist(who_won)
index_seq <- min(who_won, na.rm = TRUE)
last_number_drawn    <- rand_seq[[index_seq]]
bingo_card_won       <- mat_n[[which.min(who_won)]]

mask <- matrix(FALSE, nrow = 5, ncol = 5)
m    <- na.omit(arrayInd(match(rand_seq[1:index_seq], bingo_card_won ), .dim = dim(bingo_card_won )))
mask[m[,]] <- TRUE

sum_not_drawn <- sum(bingo_card_won[!mask])
answer <- last_number_drawn * sum_not_drawn

#who wins last
index_seq <- max(who_won, na.rm = TRUE)
last_number_drawn    <- rand_seq[[index_seq]]
bingo_card_won       <- mat_n[[which.max(who_won)]]

mask <- matrix(FALSE, nrow = 5, ncol = 5)
m    <- na.omit(arrayInd(match(rand_seq[1:index_seq], bingo_card_won ), .dim = dim(bingo_card_won )))
mask[m[,]] <- TRUE

sum_not_drawn <- sum(bingo_card_won[!mask])
answer <- last_number_drawn * sum_not_drawn

x <- mat_n[[5]]

mask <- matrix(FALSE, nrow = 5, ncol = 5)
m    <- na.omit(arrayInd(match(rand_seq, x), .dim = dim(x)))
mask[m[,]] <- TRUE
row_sums <- rowSums(mask)
col_sums <- colSums(mask)
if(any(c(row_sums, col_sums) > 4)){
  
  if(any(row_sums > 4)) {
    row_mat <- x[which(row_sums > 4),,drop = FALSE]
    max_ind_row <- vector(mode = "integer", length = nrow(row_mat))
    for(i in 1:nrow(row_mat)) {
      max_ind_row[i] <- max(which(rand_seq %in% row_mat[i,])) 
    }
    max_ind_row <- min(max_ind_row)
  } else {
    max_ind_row <- NA
  }
  if(any(col_sums > 4)){
    col_mat <- x[,which(col_sums > 4), drop = FALSE]
    max_ind_col <- vector(mode = "integer", length = ncol(col_mat))
    for(i in 1:ncol(col_mat)) {
      max_ind_col[i] <- max(which(rand_seq %in% col_mat[,i])) 
    }
    max_ind_col <- min(max_ind_col)
  } else {
    max_ind_col <- NA
  } 
  min(na.omit(c(max_ind_row, max_ind_col)))
} else {
  NA
}


# test day 4 --------------------------------------------------------------

rand_n   <- read.csv(file = "data/day_four_test_seq", header = FALSE, sep = ",")
# rand_seq <- readChar("data/day_four_seq", nchars = rand_n * 2)
# rand_seq <- as.integer(unlist((strsplit(rand_seq, split = ","))))
rand_seq <- as.integer(rand_n[1,])
#read the matrices

mat_n <- lapply(1:3, function(x){
  as.matrix(read.csv(file = "data/day_four_test_mat", nrows = 5L, skip = (x - 1L) * 6L , 
                     sep = "", header = FALSE))
})


who_won <- lapply(mat_n, function(x) {
  mask <- matrix(FALSE, nrow = 5, ncol = 5)
  m    <- na.omit(arrayInd(match(rand_seq, x), .dim = dim(x)))
  mask[m[,]] <- TRUE
  row_sums <- rowSums(mask)
  col_sums <- colSums(mask)
  if(any(c(row_sums, col_sums) > 4)){
    
    if(any(row_sums > 4)) {
      row_mat <- x[which(row_sums > 4),,drop = FALSE]
      max_ind_row <- vector(mode = "integer", length = nrow(row_mat))
      for(i in 1:nrow(row_mat)) {
        max_ind_row[i] <- max(which(rand_seq %in% row_mat[i,])) 
      }
      max_ind_row <- min(max_ind_row)
    } else {
      max_ind_row <- NA
    }
    if(any(col_sums > 4)){
      col_mat <- x[,which(col_sums > 4), drop = FALSE]
      max_ind_col <- vector(mode = "integer", length = ncol(col_mat))
      for(i in 1:ncol(col_mat)) {
        max_ind_col[i] <- max(which(rand_seq %in% col_mat[,i])) 
      }
      max_ind_col <- min(max_ind_col)
    } else {
      max_ind_col <- NA
    } 
    min(na.omit(c(max_ind_row, max_ind_col)))
  } else {
    NA
  }
})
who_won <- unlist(who_won)
index_seq <- min(who_won, na.rm = TRUE)
last_number_drawn    <- rand_seq[[index_seq]]
bingo_card_won       <- mat_n[[which.min(who_won)]]

mask <- matrix(FALSE, nrow = 5, ncol = 5)
m    <- na.omit(arrayInd(match(rand_seq[1:index_seq], bingo_card_won ), .dim = dim(bingo_card_won )))
mask[m[,]] <- TRUE

sum_not_drawn <- sum(bingo_card_won[!mask])
answer <- last_number_drawn * sum_not_drawn  