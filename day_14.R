#~~~~~~~~~~~~~~
##Day 14
#~~~~~~~~~~~~~~


# solution ----------------------------------------------------------------


# * part one ------------------------------------------------------------

InsertV <- function(a, bs, pos){
  as  <- split(a,cumsum(seq(a) %in% (pos + 1)))
  idx <- order(c(seq_along(as), seq_along(bs)))
  unlist(c(as,bs)[idx])
}


template <- c("OOFNFCBHCKBBVNHBNVCP")
template <- unlist(strsplit(template, split = ""))
plolymeres <- readLines(con = "data/day_fourteen")
poly_splits <- lapply(plolymeres, function(x){
  trimws(unlist(strsplit(x, split = "->")))
})
poly_splits <- do.call(rbind, poly_splits)
ins_vec <- template
k <-  0L
while(k <= 9L) {
  k <- k + 1L
  ind <- vector(mode = "integer", length = length(ins_vec) -1L)
  insert_values <- vector(mode = "integer", length = length(ins_vec) -1L)
  for(i in 1L : (length(ins_vec) - 1L)){
    pairs_vec <- ins_vec[i : (i + 1L)]
    ind[i] <- match(paste0(pairs_vec, collapse = ""), poly_splits)
    insert_values[i] <- poly_splits[ind[i], 2]
  }
  
  ins_vec <- InsertV(a = ins_vec, bs =  insert_values, seq(length(ins_vec) - 1))
}
ins_vec <- factor(unname(ins_vec))

sum_levels <- table(ins_vec)
max(sum_levels) - min(sum_levels)


# * part two --------------------------------------------------------------


