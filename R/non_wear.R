detect_nonwear <- function(data, window1 = 60, window2 = 15, threshold = 2) {

  window1 <- window1 * 60 * attributes(data)$samp_freq
  window2 <- window2 * 60 * attributes(data)$samp_freq

  non_wear_s1 <- nonwear_stage1(data, window1, window2, threshold)
  non_wear_s2 <- nonwear_stage2(non_wear_s1, window1, window2)

  return(list(non_wear_s1 = non_wear_s1, non_wear_s2 = non_wear_s2))

}

delete_nonwear <- function(data, non_wear_s1, non_wear_s2, window2) {
  # window2 must be in samples, not minutes, here
  non_wear <- non_wear_s1 + non_wear_s2

  block_start <- seq(1, nrow(data), by = window2)
  block_end <- seq(window2, nrow(data), by = window2)
  if (nrow(data) - block_start[length(block_start)] < window2) {
    block_start <- block_start[1:(length(block_start) - 1)]
  }

  wear_start_i <- block_start[which(non_wear == 0)]
  wear_end_i <- block_end[which(non_wear == 0)]

  wear <- rep(0, nrow(data))
  for (i in seq_len(length(wear_start_i))) {
    wear[wear_start_i[i]:wear_end_i[i]] <- 1
  }
  data$wear <- wear
  data
}

nonwear_stage1 <- function(data, window1, window2, threshold) {
  range_crit <- 0.05
  sd_crit <- 0.013

  n_blocks <- floor(nrow(data) / window2)
  crit <- ((window1 / window2) / 2) + 1
  non_wear_s1 <- matrix(0, n_blocks, 3)

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
          non_wear_s1[i, j] <- 1
        }
      } else {
        non_wear_s1[i, j] <- 1
      }
    }
  }

  non_wear_s1 <- rowSums(non_wear_s1)
  non_wear_s1[which(non_wear_s1 >= threshold)] <- 1
  non_wear_s1

}

nonwear_stage2 <- function(non_wear_s1, window1, window2) {
  h_crit_1 <- 1 / (window2 / window1)
  h_crit_3 <- 3 / (window2 / window1)
  h_crit_6 <- 6 / (window2 / window1)
  h_crit_24 <- 24 / (window2 / window1)

  non_wear_original <- non_wear_s2 <- matrix(0, length(non_wear_s1), 1)
  non_wear_idx <- which(non_wear_s1 == 1)
  non_wear_original[non_wear_idx] <- 1
  non_wear_original <- c(0, non_wear_original, 0)
  non_wear_s2 <- c(0, non_wear_s2, 0)

  start_wear <- which(diff(non_wear_original) == 1) + 1
  start_non_wear <- which(diff(non_wear_original) == - 1) + 1


  if (length(start_wear) > 1) {
    length_wear <- matrix(0, length(start_wear) - 1, 1)
    length_non_wear_after <- matrix(0, length(start_wear) - 1, 1)
    length_non_wear_before <- matrix(0, length(start_wear) - 1, 1)
    surrounding_non_wear <- matrix(0, length(start_wear) - 1, 1)

    for (i in 1:(length(start_wear) - 1)) {
      length_wear[i] <- abs(start_wear[i + 1] - start_non_wear[i])
      length_non_wear_after[i] <- abs(start_non_wear[i + 1] - start_wear[i + 1])
      length_non_wear_before[i] <- abs(start_non_wear[i] - start_wear[i])
      surrounding_non_wear[i] <- length_non_wear_after[i] -
        length_non_wear_before[i]

      if (
        length_wear[i] < h_crit_6 &
        (length_wear[i] / surrounding_non_wear[i]) < 0.3
      ) {
        non_wear_s2[start_non_wear[i]:start_wear[i + 1] - 1] <- 1
      }
      if (
        length_wear[i] < h_crit_3 &
        (length_wear[i] / surrounding_non_wear[i]) < 0.8
      ) {
        non_wear_s2[start_non_wear[i]:start_wear[i + 1] - 1] <- 1
      }
      if (start_wear[i] > length(non_wear_s1) - h_crit_24) {
        if (length_wear[i] < h_crit_3 & length_non_wear_before[i] > h_crit_1) {
          non_wear_s2[start_non_wear[i]:start_wear[i + 1] - 1] <- 1
        }
      }
    }
  }

  if (length(start_wear) > 0) {
    if (start_wear[1] < h_crit_3 & start_wear[1] > 1) {
      non_wear_s2[1:(start_wear[1] - 1)] <- 1
    }

    last_non_wear <- start_non_wear[length(start_non_wear)]
    if (
      last_non_wear > length(non_wear_s2) - h_crit_3 &
      last_non_wear != length(non_wear_s2)
    ) {
      non_wear_s2[last_non_wear:length(non_wear_s2)] <- 1
    }
  }

  non_wear_original <- non_wear_original[-c(1, length(non_wear_original))]
  non_wear_s2 <- non_wear_s2[-c(1, length(non_wear_s2))]

  for (i in 1:2) {
    non_wear_original_b <- non_wear_s2 + non_wear_original
    non_wear_original_b[which(non_wear_original_b > 1)] <- 1
    non_wear_original_b <- c(0, non_wear_original_b, 0)
    non_wear_s2_b <- c(0, non_wear_s2, 0)

    start_wear_b <- which(diff(non_wear_original_b) == 1) + 1
    start_non_wear_b <- which(diff(non_wear_original_b) == - 1) + 1

    if (length(start_wear_b) > 1) {
      length_wear_b <- matrix(0, length(start_wear_b) - 1, 1)
      length_non_wear_after_b <- matrix(0, length(start_wear_b) - 1, 1)
      length_non_wear_before_b <- matrix(0, length(start_wear_b) - 1, 1)
      surrounding_non_wear_b <- matrix(0, length(start_wear_b) - 1, 1)

      for (j in 1:(length(start_wear_b) - 1)) {
        length_wear_b[i] <- abs(start_wear_b[i + 1] - start_non_wear_b[i])
        length_non_wear_after_b[i] <- abs(
          start_non_wear_b[i + 1] - start_wear_b[i + 1]
        )
        length_non_wear_before_b[i] <- abs(
          start_non_wear_b[i] - start_wear_b[i]
        )
        surrounding_non_wear_b[i] <- length_non_wear_after_b[i] -
          length_non_wear_before_b[i]

        if (
          length_wear_b[i] < h_crit_6 &
          (length_wear_b[i] / surrounding_non_wear_b[i]) < 0.3
        ) {
          non_wear_s2_b[start_non_wear_b[i]:start_wear_b[i + 1] - 1] <- 1
        }
        if (
          length_wear_b[i] < h_crit_3 &
          (length_wear_b[i] / surrounding_non_wear_b[i]) < 0.8
        ) {
          non_wear_s2_b[start_non_wear_b[i]:start_wear_b[i + 1] - 1] <- 1
        }
        if (start_wear_b[i] > length(non_wear_s1) - h_crit_24) {
          if (
            length_wear_b[i] < h_crit_3 & length_non_wear_before_b > h_crit_1
          ) {
            non_wear_s2_b[start_non_wear_b[i]:start_wear_b[i + 1] - 1] <- 1
          }
        }
      }
    }
    non_wear_s2 <- non_wear_s2_b[-c(1, length(non_wear_s2_b))]
  }

  non_wear_s2[which(non_wear_s1 + non_wear_s2 == 2)] <- 0
  non_wear_s2

}
