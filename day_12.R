#~~~~~~~~~~~~~~
##Day 12
#~~~~~~~~~~~~~~

raw_data <- readLines(con = "data/day_12")

connections <- strsplit(raw_data, split = "-")
connections <- lapply(connections, function(x){
  c("from" = x[1], "to" = x[2])
})
connections          <- do.call(rbind, connections)
connections <- rbind(connections, connections[,c("to", "from")])



m <- matrix(nrow = length(unique(connections[,"from"])), ncol = length(unique(connections[,"to"])),
            dimnames = list(unique(connections[,"from"]), unique(connections[,"to"])))
m[connections] <- 1


#test
starts <- c(na.omit(m["start",]), na.omit(m[, "start"]))

starts[1]
GoWhere <- function(cell, mat){
  pos_cells <- names(na.omit(mat[cell,]))
  pos_cells <- pos_cells[!("start" %in% pos_cells)]
  return(pos_cells)
}

start_cells <- GoWhere(cell = "start", mat = m)
#GoWhere(cell = start_cells[1], mat = m)

n_paths <- length(start_cells)
k <- 0
path_taken <- start_cells[1]
while(k <= n_paths){
  k <- k + 1L
  go_to      <- GoWhere(path_taken[k], mat = m)
  path_taken <- c(path_taken, go_to)
  n_paths    <- length(path_taken)
}

