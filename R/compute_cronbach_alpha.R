
compute_cronbach_alpha <- function(scores_mat, unknown_info, unknown_value) {

  n_person <- nrow(scores_mat)

  sig_x <- sigma_x_value(scores_mat, unknown_info, unknown_value)
  sig_y <- sigma_y_value(scores_mat, unknown_info, unknown_value)

  cronbach_alpha <- (n_person / (n_person - 1.0)) * (1.0 - sig_x / sig_y)

  return(cronbach_alpha)
}
