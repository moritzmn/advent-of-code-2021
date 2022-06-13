#~~~~~~~~~~~~~~
##Day 9
#~~~~~~~~~~~~~~


# solution ----------------------------------------------------------------

# * part one -------------------------------------------------------------------

#functions
MNeighbors <- function(mat, i = 2L, j = 3L) {
  all(mat[i, j] < c(mat[i, j - c(-1L, 1L)], mat[i - c(-1L, 1L), j]), na.rm = TRUE)
}
vMNeighbors <- Vectorize(FUN = MNeighbors, "j")

raw_data <- readLines(con = "data/day_nine")
integer_split <- lapply(raw_data, function(x){
  as.integer(strsplit(x, split = "")[[1]])
})
m <- do.call(rbind, integer_split)
#add NA
m_t <- cbind(NA, rbind(NA, m,  NA), NA)

#get borders
rows <- 2L : (nrow(m_t) - 1L)
cols <- 2L : (ncol(m_t) - 1L)

mask <- lapply(rows, function(y) {
  vMNeighbors(m_t, i = y, j = cols)
})
mask <- do.call(rbind, mask)
(answer <- sum(m[mask] + 1))

# test --------------------------------------------------------------------

raw_data <- readLines(con = "data/day_nine_test")
integer_split <- lapply(raw_data, function(x){
  as.integer(strsplit(x, split = "")[[1]])
})
m <- do.call(rbind, integer_split)
#add NA
m_t <- cbind(NA, rbind(NA, m,  NA), NA)

MNeighbors <- function(mat, i = 2L, j = 3L) {
  all(mat[i, j] < c(mat[i, j - c(-1L, 1L)], mat[i - c(-1L, 1L), j]), na.rm = TRUE)
}
MNeighbors(mat = m_t)
i <- 2; j <- 3
#Vectorize
vMNeighbors <- Vectorize(FUN = MNeighbors, "j")
vMNeighbors(m_t, i = 2, j = 2:11)
#get borders
rows <- 2L : (nrow(m_t) - 1L)
cols <- 2L : (ncol(m_t) - 1L)
mask <- lapply(rows, function(y) {
  vMNeighbors(m_t, i = y, j = cols)
})
mask <- do.call(rbind, mask)
m[mask] + 1
