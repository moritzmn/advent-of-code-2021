#~~~~~~~~~~~~~~
##Day 10
#~~~~~~~~~~~~~~
patterns <- "{}"
a <- c("{([(<{}[<>[]}>{[]{[(<()>")

open_close <- c(gregexpr("{", a, fixed = TRUE)[[1]], gregexpr("}", a, fixed = TRUE)[[1]])

open_close <- matrix(NA_character_, nrow = 4, ncol = 2)
open_close[, 1] <- c("(", "[", "{", "<")
open_close[, 2] <- c(")", "]", "}", ">")
string_log <- vector(mode = "list", length = length(a))

for (i in seq(nrow(open_close))) {
  open  <- gregexpr(open_close[i, 1], a, fixed = TRUE)[[1]]
  close <- gregexpr(open_close[i, 2], a, fixed = TRUE)[[1]]
  
  string_log[[i]] <- rep(NA_integer_, times = nchar(a))
  string_log[[i]][open]  <- 1
  string_log[[i]][close] <- 0  
}



