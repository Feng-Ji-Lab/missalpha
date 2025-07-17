

sigma_x <- function(scores_mat, unknown_info) {

  unknown_count <- unknown_info[[1]]
  unknown_mat_to_list <- unknown_info[[2]]
  unknown_list_to_mat <- unknown_info[[3]]

  n_person <- nrow(scores_mat)
  n_item <- ncol(scores_mat)
  column_sum <- colSums(scores_mat, na.rm = TRUE)

  A_mat <- matrix(0, nrow = unknown_count, ncol = unknown_count)
  b_array <- rep(0, unknown_count)

  const <- 2.0 / n_person / (n_person - 1)

  for (num_iter_1 in 1:unknown_count) {
    for (num_iter_2 in 1:unknown_count) {
      if (num_iter_1 == num_iter_2) {
        A_mat[num_iter_1, num_iter_1] <- 2.0 / n_person
        b_array[num_iter_1] <- -column_sum[unknown_list_to_mat[[num_iter_1]][2]+1] * const
      } else {
        if (unknown_list_to_mat[[num_iter_1]][2] == unknown_list_to_mat[[num_iter_2]][2]) {
          A_mat[num_iter_1, num_iter_2] <- -const
        } else {
          A_mat[num_iter_1, num_iter_2] <- 0
        }
      }
    }
  }
  c_scaler <- 0
  for (item_iter in 1:n_item) {
    scores_squared <- scores_mat[, item_iter]^2
    c_scaler <- c_scaler + (sum(scores_squared, na.rm = TRUE) - sum(scores_mat[, item_iter], na.rm = TRUE)^2 / n_person) / (n_person - 1)
  }

  return(list(A_mat = A_mat, b_array = b_array, c_scaler = c_scaler))
}
