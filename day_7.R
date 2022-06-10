#~~~~~~~~~~~~~~
##Day 7
#~~~~~~~~~~~~~~


# solution ----------------------------------------------------------------

fuel_con <- function(pos_vec) {
  align_pos <- min(pos_vec): max(pos_vec)
  fuel_consumption <- vector(mode = "integer", length = length(align_pos))
  real_fuel <- vector(mode = "integer", length = length(align_pos))
  for(i in seq_along(align_pos)) {
    diff_pos <- abs(align_pos[i] - pos_vec)
    fuel_consumption[i] <- sum(diff_pos)
    real_fuel[i]        <- sum(sapply(diff_pos[diff_pos > 0], function(x) sum(1 : x)))
  }
  print(real_fuel)
  return(list("min_fuel" = min(fuel_consumption),
              "real_fuel" = min(real_fuel),
              "final_pos" = align_pos[which.min(real_fuel)]))
}

FuelCon <- function(crab_pos) {
  align_pos <- min(crab_pos): max(crab_pos)
  fuel_sum     <- sapply(align_pos, function(x){
    diff_pos   <- abs(x - crab_pos)
    wrong_fuel <- sum(diff_pos)
    real_fuel  <- sum(sapply(diff_pos[diff_pos > 0], function(y) sum(1 : y)))
    list(wrong_fuel, real_fuel)
  })
  return(fuel_sum)
}


# # * part one ------------------------------------------------------------

crab_pos <- readLines(con = "data/day_seven_one")
crab_pos <- as.integer(unlist(strsplit(crab_pos, split =",")))

fuel_con(crab_pos)

crab_pos <- readLines(con = "data/day_seven_one")
crab_pos <- as.integer(unlist(strsplit(crab_pos, split =",")))

fuel_con <- FuelCon(crab_pos)  

# test --------------------------------------------------------------------

crab_pos <- c(16,1,2,0,4,2,7,1,2,14)
fuel_consumption <- fuel_con(crab_pos)

pos_vec <- c(16,1,2,0,4,2,7,1,2,14)


fuel_consumption <- FuelCon(pos_vec)
min(unlist((fuel_consumption[1, ])))
min(unlist((fuel_consumption[2,])))



 
