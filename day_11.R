#~~~~~~~~~~~~~~
##Day 11
#~~~~~~~~~~~~~~


# solution --------------------------------------------------------------------

MNeighbors <- function(mat, i = 2L, j = 3L, zero = FALSE) {
  #mat[i, j] < c(mat[i, j - c(-1L, 1L)], mat[i - c(-1L, 1L), j]), na.rm = TRUE)
  range_vec <- -1:1
  if(zero) {
    mat[i + range_vec, j + range_vec] <- mat[i + range_vec, j + range_vec] <- 0L
  } else {
    mat[i + range_vec, j + range_vec] <- mat[i + range_vec, j + range_vec] + 1L
  }
  mat[c(1, nrow(mat)),] <- NA
  mat[,c(1, ncol(mat))] <- NA
  return(mat)
}

raw_data <- readLines(con = "data/day_eleven")

integer_split <- lapply(raw_data, function(x){
  as.integer(strsplit(x, split = "")[[1]])
})
m <- do.call(rbind, integer_split)


# # * part 1 --------------------------------------------------------------

#add NA
m_t <- cbind(NA, rbind(NA, m,  NA), NA)


steps <- 100L
sum_flashes <- integer(length = steps)
for(k in seq(steps)) {
  m_t <- m_t + 1L
  flashes_mat <- which(m_t > 9L, arr.ind = TRUE)
  flashed     <- flashes_mat
  while(any(flashes_mat)) {
    for(i in 1 : nrow(flashes_mat)) {
      m_t <- MNeighbors(m_t, i = flashes_mat[i, "row"], j = flashes_mat[i, "col"])
      
    }
    flashed <- rbind(flashed, flashes_mat)
    #update flashes mat
    flashes_mat <- which(m_t > 9L, arr.ind = TRUE)
    flashes_mat <- flashes_mat[!(paste0(flashes_mat[,1], flashes_mat[,2])) %in% (paste0(flashed[, 1], flashed[, 2])),, drop = FALSE]
    
  }
  sum_flashes[k] <- sum(m_t > 9L, na.rm = TRUE)
  m_t[m_t > 9L] <- 0L
}
sum(sum_flashes)


# # * part 2 --------------------------------------------------------------

all_flashed <- ncol(m) * nrow(m)
m_t <- cbind(NA, rbind(NA, m,  NA), NA)

sum_flashes <- 0L
k <- 0L
while(sum_flashes < all_flashed) {
  m_t <- m_t + 1L
  flashes_mat <- which(m_t > 9L, arr.ind = TRUE)
  flashed     <- flashes_mat
  while(any(flashes_mat)) {
    for(i in 1 : nrow(flashes_mat)) {
      m_t <- MNeighbors(m_t, i = flashes_mat[i, "row"], j = flashes_mat[i, "col"])
      
    }
    flashed <- rbind(flashed, flashes_mat)
    #update flashes mat
    flashes_mat <- which(m_t > 9L, arr.ind = TRUE)
    flashes_mat <- flashes_mat[!(paste0(flashes_mat[,1], flashes_mat[,2])) %in% (paste0(flashed[, 1], flashed[, 2])),, drop = FALSE]
    
  }
  sum_flashes <- sum(m_t > 9L, na.rm = TRUE)
  print(length(all_flashed))
  m_t[m_t > 9L] <- 0L
  k <- k + 1L
}

k





