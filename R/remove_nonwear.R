remove_nonwear <- function(data, window1 = 60, window2 = 15, threshold = 2) {

  window1 <- window1 * 60 * attributes(data)$samp_freq
  window2 <- window2 * 60 * attributes(data)$samp_freq

  n_blocks <- floor(nrow(data) / window2)
  crit <- ((window1 / window2) / 2) + 1

  range_crit <- 0.05
  sd_crit <- 0.013

  # Step 1
  non_wear <- matrix(0, n_blocks, 3)

  for (i in 1:n_blocks) {
    print(i)
    if (i <= crit) {
      start <- 1
      end <- window1
    } else if (i > crit & i < (n_blocks - crit)) {
      start <- (((i - 1) * window2) + window2 / 2) - window1 / 2
      end <- (((i - 1) * window2) + window2 / 2) + window1 / 2
    } else if (i >= (n_blocks - crit)) {
      start <- (n_blocks - crit) * window2
      end <- n_blocks * window2
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
  non_wear <- rowSums(non_wear)

  # Step 2
  non_wear_idx <- which(non_wear >= threshold)
  non_wear_final <- matrix(0, length(non_wear), 1)
  non_wear_final[non_wear_idx] <- 1
  non_wear_final <- c(0, non_wear_final, 0)

  start_wear <- which(diff(non_wear_final) == 1) + 1
  start_non_wear <- which(diff(non_wear_final) == - 1) + 1

  if (length(start_wear) > 1) {
    length_wear <- matrix(0, length(start_wear) - 1, 1)
    length_non_wear_after <- matrix(0, length(start_wear) - 1, 1)
    length_non_wear_before <- matrix(0, length(start_wear) - 1, 1)

    for (i in 1:length(start_wear) - 1) {
      length_wear[i] <- abs(start_wear[i + 1] - start_non_wear[i])
      length_non_wear_after <- abs(start_non_wear[i + 1] - start_wear[i])
      length_non_wear_before <- abs(start_non_wear[i] - start_wear[i])
    }
  }

}
