dims <- matrix(nrow = 0, ncol = 4)

max_blocks <- 48*48
for (a_w in 2:20) {
  for (a_h in 2:20) {
    for (l_w in 1:20) {
      for (l_h in 1:20) {
        if ((a_w * a_h * l_w * l_h) <= max_blocks) {
          row <- c(a_w, a_h, l_w, l_h)
          dims <- rbind(dims, row)
        }
      }
    }
  }
}

for (a in 2:48) {
  for (l in 1:48) {
    if ((a^2 * l^2) == max_blocks) {
      row <- c(a, a, l, l)
      dims <- rbind(dims, row)
    }
  }
}



write.csv(dims, 'possible_dims_square_2304.csv', row.names = FALSE)
