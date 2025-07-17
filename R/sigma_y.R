
sigma_y <- function(scores_mat, unknown_info) {

  unknown_count <- unknown_info[[1]]
  unknown_mat_to_list <- unknown_info[[2]]
  unknown_list_to_mat <- unknown_info[[3]]

  n_person <- nrow(scores_mat)
  n_item <- ncol(scores_mat)

  row_sum <- apply(scores_mat, 1, function(x) sum(x, na.rm = TRUE))
  mat_sum <- sum(row_sum, na.rm = TRUE)

  A_mat <- matrix(0, unknown_count, unknown_count)
  b_array <- numeric(unknown_count)

  const <- 2.0 / n_person / (n_person - 1)

  for (num_iter_1 in 1:unknown_count) {
    for (num_iter_2 in 1:unknown_count) {
      if (num_iter_1 == num_iter_2) {
        A_mat[num_iter_1, num_iter_1] <- 2.0 / n_person
        b_array[num_iter_1] <- row_sum[unknown_list_to_mat[[num_iter_1]][1]+1] * 2.0 / (n_person - 1) - mat_sum * const
      } else {
        if (unknown_list_to_mat[[num_iter_1]][1] == unknown_list_to_mat[[num_iter_2]][1]) {
          A_mat[num_iter_1, num_iter_2] <- 2.0 / n_person
        } else {
          A_mat[num_iter_1, num_iter_2] <- -const
        }
      }
    }
  }

  c_scaler <- 0
  scores_squared <- row_sum^2
  c_scaler <- (sum(scores_squared) - sum(row_sum)^2 / n_person) / (n_person - 1)

  return(list(A_mat, b_array, c_scaler))
}
