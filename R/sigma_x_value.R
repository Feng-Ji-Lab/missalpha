#' @import stats
sigma_x_value <- function(scores_mat, unknown_info, unknown_value) {

  unknown_count <- unknown_info[[1]]
  unknown_mat_to_list <- unknown_info[[2]]
  unknown_list_to_mat <- unknown_info[[3]]

  stopifnot(unknown_count == length(unknown_value))

  scores_tmp <- scores_mat
  for (num_iter in 1:unknown_count) {
    loc <- unknown_list_to_mat[[num_iter]]
    scores_tmp[loc[1] + 1, 1] <- unknown_value[num_iter]
  }

  n_person <- nrow(scores_tmp)
  n_item <- ncol(scores_tmp)
  biased_var <- function(x) {
    mean_x <- mean(x, na.rm = TRUE)
    sum((x - mean_x)^2, na.rm = TRUE) / length(na.omit(x))
  }
  result <- sum(apply(scores_tmp, 2, biased_var)) * n_person / (n_person - 1)

  return(result)
}
