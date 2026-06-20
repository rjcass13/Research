dims <- matrix(nrow = 0, ncol = 4)

max_blocks <- 576
for (a_w in 2:24) {
  for (a_h in 2:24) {
    for (l_w in 1:24) {
      for (l_h in 1:24) {
        if ((a_w * a_h * l_w * l_h) == max_blocks) {
          row <- c(a_w, a_h, l_w, l_h)
          dims <- rbind(dims, row)
        }
      }
    }
  }
}


write.csv(dims, 'possible_dims_576.csv', row.names = FALSE)