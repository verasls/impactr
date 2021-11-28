remove_nonwear <- function(data, window1 = 60, window2 = 15, threshold = 2) {

  window1 <- window1 * 60 * attributes(data)$samp_freq
  window2 <- window2 * 60 * attributes(data)$samp_freq

  n_blocks <- floor(nrow(data) / window2)
  crit <- ((window1 / window2) / 2) + 1

  # Non-wear criteria based on acceleration siganl range and standard deviation
  range_crit <- 0.05
  sd_crit <- 0.013

  # Initialize matrix to hold non-wear values
  non_wear <- matrix(0, n_blocks, 3)

  for (i in 1:n_blocks) {
    print(i)
    if (i <= crit) {
      # Beginning of data
      start <- 1
      end <- window1
    } else if (i > crit & i < (n_blocks - crit)) {
      start <- (((i - 1) * window2) + window2 / 2) - window1 / 2
      end <- (((i - 1) * window2) + window2 / 2) + window1 / 2
    } else if (i >= (n_blocks - crit)) {
      start <- (n_blocks - crit) * window2
      end <- n_blocks * window2
      # End of data
    }

    for (j in 1:3) {
      max_acc <- max(data[[j + 1]][(start + 1):end], na.rm = TRUE)
      min_acc <- min(data[[j + 1]][(start + 1):end], na.rm = TRUE)
      range_acc <- abs(max_acc - min_acc)
      sd_acc <- sd(data[[j + 1]][(start + 1):end], na.rm = TRUE)

      if (is.numeric(range_acc) & is.numeric(sd_acc)) {
        if (range_acc < range_crit & sd_acc < sd_crit) {
          non_wear[i, j] <- 1
        }
      } else {
        non_wear[i, j] <- 1
      }
    }
  }

}
