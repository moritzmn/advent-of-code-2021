#~~~~~~~~~~~~~~
##Day 6
#~~~~~~~~~~~~~~


# solution --------------------------------------------------------------------

initial_state <- readLines(con = "data/day_six_one")
initial_state <- as.integer(unlist(strsplit(initial_state, split =",")))

sim_fish <- function(ini_state = c(3, 4, 3, 1, 2), n_run = 18L) {
  m <- 1L
  while(m <= n_run) {
    
    for(i in seq_along(ini_state)) {
      ini_state[i] <- ini_state[i] - 1L
      if(ini_state[i] < 0) {
        ini_state[i] <- 6L
        ini_state <- c(ini_state, 8L)
      }
    }
    print(m)
    m <- m + 1L
  }
  
  return(ini_state)
}

sim_fish_fast <- function(ini_state = c(3L, 4L, 3L, 1L, 2L), n_run = 18L) {
  m <- 1L
  #ini_state[(length(ini_state)+1L): 1e12] <- NA_integer_
  # fish_vec <- rep(NA_integer_, 1e9)
  # fish_vec[1 : length(ini_state)] <- ini_state
  while(m <= n_run) {
    
    ini_state  <- ini_state - 1L
    update_ind <- ini_state < 0L
    ini_state[update_ind] <- 6L
    
    if(sum(update_ind) > 0) {
      update_where <- (length(ini_state) + 1) : (length(ini_state) +  sum(update_ind))
      ini_state[update_where] <- 8L
    } else {
      
    }
    m <- m + 1L
  }
  
  return(na.omit(ini_state))
}

sim_fish_faster <- function(ini_state = c(3L, 4L, 3L, 1L, 2L), n_run = 18L, vec_length = 1e4) {
  m <- 1L
  #ini_state[(length(ini_state)+1L): 1e12] <- NA_integer_
  fish_vec <- rep(NA_integer_, vec_length)
  fish_vec[1 : length(ini_state)] <- ini_state
  update_max <- min(which(is.na(fish_vec)))
  while(m <= n_run) {
    print(m)
    #fish_vec[1 : (update_max - 1L)] 
    fish_vec   <- fish_vec - 1L
    new_fish_n <- sum(fish_vec < 0L, na.rm = TRUE)
    fish_vec[fish_vec < 0L] <- 6L
    
    if(new_fish_n > 0) {
      fish_vec[update_max: (new_fish_n + update_max - 1L)] <- 8L
      update_max <- min(which(is.na(fish_vec)))
    } else {
      
    }
    m <- m + 1L
  }
  
  return(na.omit(fish_vec))
}


# * part 1 ------------------------------------------------------------------

fish_state <- sim_fish_faster(ini_state = initial_state, n_run = 80L, vec_length = 1e7)
length(fish_state)

# * part 2 ------------------------------------------------------------------

#does not finish
system.time(ocean_fish <- sim_fish_fast(n_run = 200L))
system.time(ocean_fish <- sim_fish_faster(ini_state = 1L, n_run = 256L, vec_length = 1e9))

# test --------------------------------------------------------------------

fish_state <- sim_fish(n_run = 80L)
length(fish_state)
fish_fast <- sim_fish_fast(n_run = 80L)
length(fish_fast)
length(sim_fish_faster())
length(sim_fish_faster(n_run = 80L))




