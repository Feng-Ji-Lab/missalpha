#' Compute Lower and Upper Bound of Cronbach's Alpha
#'
#' This function computes the lower and upper bound of Cronbach's alpha
#' using various methods such as enumeration, random sampling, or
#' optimization algorithms. The function also supports rough approximations
#' and allows integer-only or floating-point scores during sampling.
#'
#' @param scores_mat A matrix where rows represent persons and columns represent tests (or items),
#'   providing the performance of a person on a test. NA should be used for missing values.
#' @param score_max An integer indicating the largest possible score of the test.
#' @param tol A numeric value representing the desired accuracy in computing the lower and upper bound of Cronbach's alpha.
#' @param num_random An integer specifying the number of random samples used in estimating the lower and upper bound. Default is 1000.
#' @param enum_all A logical value indicating whether to enumerate all possible scores for Cronbach's alpha. Default is FALSE.
#' @param rough A logical value indicating whether to compute a rough approximation of Cronbach's alpha bounds. Default is FALSE.
#' @param num_opt An integer specifying the number of times to run the optimization algorithm. Default is 1.
#' @param int_only A logical value indicating whether the random sampling should be restricted to integer-only scores. Default is TRUE.
#' @param method A character string specifying the optimization method to be used ('GA', 'DEoptim', 'nloptr'). Default is 'GA'.
#' @param ... Additional parameters passed to the optimization algorithm.
#'
#' @return A list containing:
#' \item{alpha_min_opt}{The smallest possible Cronbach's alpha computed by the optimization algorithm (if used).}
#' \item{alpha_max_opt}{The largest possible Cronbach's alpha computed by the optimization algorithm (if used).}
#' \item{alpha_min_enum}{The smallest possible Cronbach's alpha obtained by enumerating all possible scores (if used).}
#' \item{alpha_max_enum}{The largest possible Cronbach's alpha obtained by enumerating all possible scores (if used).}
#' \item{alpha_min_rough}{The smallest possible Cronbach's alpha obtained by rough approximation (if used).}
#' \item{alpha_max_rough}{The largest possible Cronbach's alpha obtained by rough approximation (if used).}
#' \item{method}{The optimization method used.}
#' \item{runtime}{The total computation time in seconds.}
#'
#' @seealso \code{\link{compute_alpha_min}}, \code{\link{compute_alpha_max}}, \code{\link{cronbach_alpha_enum}}, \code{\link{cronbach_alpha_rough}}, \code{\link{generate_scores_mat_bernoulli}}, \code{\link{qp_solver}}
#'
#' @examples
#' \dontrun{
#' # Example 1: Run `cronbachs_alpha` with a sample matrix
#' scores_mat <- matrix(c(
#'   NaN, 1, 0, 0, 0, 0, 0, 0, NaN, 0, 0, 0,
#'   2, 0, 0, 1, NaN, 0, 0, 0, 0, 0, 0, 0,
#'   1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1
#' ), nrow = 10, ncol = 4, byrow = TRUE)
#'
#' result <- cronbachs_alpha(scores_mat, score_max = 4, enum_all = FALSE)
#' print(result$alpha_min_opt)
#' print(result$alpha_max_opt)
#'
#' # Example 2: Generate a Bernoulli matrix and compute Cronbach's alpha
#' score_max <- 2
#' scores_mat_bernoulli <- generate_scores_mat_bernoulli(50, 10, 20, score_max)
#' result <- cronbachs_alpha(scores_mat_bernoulli, score_max, enum_all = FALSE)
#' print(result$alpha_min_opt)
#' print(result$alpha_max_opt)
#'
#' # Example 3: Using a predefined dataset from missalpha
#' scores_df <- missalpha::sample
#' scores_mat <- as.matrix(scores_df)
#' result <- cronbachs_alpha(scores_mat, score_max = 4, enum_all = FALSE)
#' print(result$alpha_min_opt)
#' print(result$alpha_max_opt)
#' }
#' 
#' @export
cronbachs_alpha <- function(
    scores_mat,
    score_max,
    tol = 1e-3,
    num_random = 1000,
    enum_all = FALSE,
    rough = FALSE,
    num_opt = 1,
    int_only = TRUE,
    method = "GA",
    ...
) {
  # Record start time
  start_time <- Sys.time()

  # Main function in computing lower and upper bound of Cronbach's alpha.
  #
  # Args:
  #   scores_mat: A person by test (or item) matrix providing
  #     the performance of a person on a test,
  #     NA if the result is missing.
  #   score_max: An integer indicating the largest possible score of the test.
  #   tol: The desired accuracy in the lower and upper bound.
  #   num_random: An integer indicating the number of random samples
  #     in estimating the lower and upper bound.
  #   enum_all: A bool indicating whether to enumerate all possible scores.
  #   num_opt: An integer indicating the number of times to run the optimization algorithm.
  #
  # Returns:
  #   A list containing:
  #     alpha_min_opt: The smallest possible alpha given by the optimization algorithm.
  #     alpha_max_opt: The largest possible alpha given by the optimization algorithm.

  # Remove logging features (no logging in this function).

  # # Random sampling (integer only)
  # res_int <- cronbach_alpha_rough(
  #   scores_mat,
  #   score_max,
  #   num_try = num_random,
  #   int_only = TRUE
  # )
  # alpha_min_sample_int <- res_int[1]
  # alpha_max_sample_int <- res_int[2]
  #
  # # Random sampling (float allowed)
  # res_float <- cronbach_alpha_rough(
  #   scores_mat,
  #   score_max,
  #   num_try = num_random,
  #   int_only = FALSE
  # )
  # alpha_min_sample <- res_float[1]
  # alpha_max_sample <- res_float[2]

  # Enumerate all possible scores (if requested)
  if (enum_all) {
    res_enum <- cronbach_alpha_enum(
      scores_mat,
      score_max
    )
    alpha_min_enum <- res_enum[1]
    alpha_max_enum <- res_enum[2]
    runtime <- Sys.time() - start_time
    result <- list(
      alpha_min_enum = alpha_min_enum,
      alpha_max_enum = alpha_max_enum,
      method = method,
      runtime = runtime
    )
    class(result) <- "CronbachAlphaResult"
    return(result)
  }

  # Rough approximation (if requested)
  if (rough) {
    res_rough <- cronbach_alpha_rough(
      scores_mat,
      score_max,
      num_try = num_random,
      int_only = int_only
    )
    alpha_min_rough <- res_rough[1]
    alpha_max_rough <- res_rough[2]
    runtime <- Sys.time() - start_time
    result <- list(
      alpha_min_rough = alpha_min_rough,
      alpha_max_rough = alpha_max_rough,
      method = method,
      runtime = runtime
    )
    class(result) <- "CronbachAlphaResult"
    return(result)
  }

  # Prepare data for optimization
  unknown_info <- unknown_loc(scores_mat)
  sigma_x_info <- sigma_x(scores_mat, unknown_info)
  sigma_y_info <- sigma_y(scores_mat, unknown_info)

  # Optimization to compute alpha_min and alpha_max
  alpha_min_opt <- compute_alpha_min(
    nrow(scores_mat),
    sigma_x_info,
    sigma_y_info,
    score_max = score_max,
    num_try = num_opt,
    method = method,
    ...
  )
  alpha_max_opt <- compute_alpha_max(
    nrow(scores_mat),
    sigma_x_info,
    sigma_y_info,
    score_max = score_max,
    num_try = num_opt,
    method = method,
    ...
  )

  # Record end time and calculate runtime
  runtime <- Sys.time() - start_time

  # Return the results including optimization method and runtime
  result <- list(
    alpha_min_opt = alpha_min_opt,
    alpha_max_opt = alpha_max_opt,
    method = method,
    runtime = runtime
  )
  class(result) <- "CronbachAlphaResult"
  return(result)
}
#' @noRd
#' @method summary CronbachAlphaResult
#' @export
summary.CronbachAlphaResult <- function(object, ...) {
  ### Print Summary Results ###
  cat("Summary of Cronbach's Alpha Bounds Calculation: \n\n")

  if (!is.null(object$alpha_min_opt) && !is.null(object$alpha_max_opt)) {
    cat(sprintf("Optimization Method: %s \n", object$method))
    cat(sprintf("Alpha Min (Optimized): %f \n", object$alpha_min_opt))
    cat(sprintf("Alpha Max (Optimized): %f \n", object$alpha_max_opt))
  }

  if (!is.null(object$alpha_min_enum) && !is.null(object$alpha_max_enum)) {
    cat("Enumeration Method Used:\n")
    cat(sprintf("Alpha Min (Enumerated): %f \n", object$alpha_min_enum))
    cat(sprintf("Alpha Max (Enumerated): %f \n", object$alpha_max_enum))
  }

  if (!is.null(object$alpha_min_rough) && !is.null(object$alpha_max_rough)) {
    cat("Rough Approximation Used:\n")
    cat(sprintf("Alpha Min (Rough): %f \n", object$alpha_min_rough))
    cat(sprintf("Alpha Max (Rough): %f \n", object$alpha_max_rough))
  }

  cat("\nRuntime Information:\n")
  cat(sprintf("Total Runtime: %f seconds \n", object$runtime))
}
summary <- function(object, ...) UseMethod("summary")
