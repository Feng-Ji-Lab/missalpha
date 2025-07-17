#' @import stats
sigma_y_value <- function(scores_mat, unknown_info, unknown_value) {

  unknown_count <- unknown_info[[1]]
  unknown_mat_to_list <- unknown_info[[2]]
  unknown_list_to_mat <- unknown_info[[3]]
  if (unknown_count != length(unknown_value)) {
    stop('Length does not match')
  }

  scores_tmp <- scores_mat

  for (num_iter in 1:unknown_count) {
    loc <- unknown_list_to_mat[[num_iter]]
    scores_tmp[loc[1] + 1, 1] <- unknown_value[num_iter]
  }

  n_person <- nrow(scores_tmp)
  n_item <- ncol(scores_tmp)

  # Calculate the variance
  biased_var <- function(x) {
    mean_x <- mean(x, na.rm = TRUE)
    sum((x - mean_x)^2, na.rm = TRUE) / length(na.omit(x))
  }
  var_sum <- biased_var(rowSums(scores_tmp)) * n_person / (n_person - 1)

  return(var_sum)
}
