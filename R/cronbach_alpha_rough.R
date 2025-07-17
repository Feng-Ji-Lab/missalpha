#' Compute Rough Approximation of Cronbach's Alpha Bounds
#'
#' This function computes a rough approximation of the lower and upper bounds
#' of Cronbach's alpha by performing random sampling or integer sampling for
#' missing values in the score matrix.
#'
#' @param scores_mat A matrix where rows represent persons and columns represent tests (or items),
#'   providing the performance of a person on a test. NA should be used for missing values.
#' @param score_max An integer indicating the largest possible score of the test.
#' @param num_try An integer specifying the number of random samples to generate in order to estimate the lower and upper bounds. Default is 1000.
#' @param int_only A logical value indicating whether to sample only integer values for missing scores. If FALSE, floating-point values between 0 and \code{score_max} are sampled. Default is FALSE.
#'
#' @details
#' This function performs random sampling to estimate the bounds of Cronbach's alpha for a given test score matrix with missing values. It first calculates the alpha assuming all missing values are either 0 or \code{score_max}. Then, it iteratively samples either integer values or continuous values (depending on the value of \code{int_only}) for the missing scores and recalculates the Cronbach's alpha. The minimum and maximum alphas observed over all iterations are returned as the estimated bounds.
#'
#' @return A numeric vector of length 2, where the first value is the estimated minimum Cronbach's alpha and the second value is the estimated maximum Cronbach's alpha.
#'
#' @seealso \code{\link{cronbachs_alpha}}
#' @import stats
#' @export

cronbach_alpha_rough <- function(
    scores_mat,
    score_max,
    num_try = 1000,
    int_only = FALSE
) {
  unknown_info <- unknown_loc(scores_mat)

  unknown_count <- unknown_info$count

  alpha_all_zero <- compute_cronbach_alpha(scores_mat, unknown_info, as.numeric(rep(0, unknown_count)))
  alpha_all_max <- compute_cronbach_alpha(scores_mat, unknown_info, rep(score_max, unknown_count))
  if (alpha_all_zero < alpha_all_max) {
    alpha_min <- alpha_all_zero
    alpha_max <- alpha_all_max
  } else {
    alpha_min <- alpha_all_max
    alpha_max <- alpha_all_zero
  }
  for (num_iter in 1:num_try) {
    if (int_only) {
      unknown_values <- sample(0:score_max, unknown_count, replace = TRUE)
    } else {
      unknown_values <- runif(unknown_count, min = 0, max = score_max)
    }
    alpha <- compute_cronbach_alpha(scores_mat, unknown_info, unknown_values)

    if (alpha < alpha_min) {
      alpha_min <- alpha
    }
    if (alpha > alpha_max) {
      alpha_max <- alpha
    }
  }

  return(c(alpha_min, alpha_max))
}
