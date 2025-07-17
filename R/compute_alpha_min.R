#' Compute Minimum Possible Alpha Value
#'
#' This function calculates the minimum possible value of Cronbach's alpha by using
#' a binary search algorithm with optimization methods. The function iteratively narrows
#' the bounds of alpha until the desired tolerance level is reached.
#'
#' @param n_person An integer specifying the number of individuals (rows) in the score matrix.
#' @param sigma_x_info A list containing the quadratic function information for sigma_x. It should include:
#' \itemize{
#'   \item A matrix representing the quadratic coefficients for sigma_x.
#'   \item A vector representing the linear coefficients for sigma_x.
#'   \item A scalar constant term for sigma_x.
#' }
#' @param sigma_y_info A list containing the quadratic function information for sigma_y. It should include:
#' \itemize{
#'   \item A matrix representing the quadratic coefficients for sigma_y.
#'   \item A vector representing the linear coefficients for sigma_y.
#'   \item A scalar constant term for sigma_y.
#' }
#' @param score_max An integer specifying the largest possible score for any test item. Default is 1.
#' @param alpha_lb A numeric value specifying the lower bound of alpha, usually set to 0.0.
#' @param alpha_ub A numeric value specifying the upper bound of alpha, usually set to 1.0.
#' @param tol A numeric value representing the desired accuracy for narrowing down the bounds between \code{alpha_lb} and \code{alpha_ub}. Default is 1e-3.
#' @param num_try An integer specifying the number of times to run the optimization algorithm in each iteration. Default is 1.
#' @param method A character string specifying the optimization method to be used. Options are 'GA' (Genetic Algorithm), 'DEoptim' (Differential Evolution), and 'nloptr' (Sequential Least Squares Programming). Default is 'GA'.
#' @param ... Additional parameters passed to the optimization algorithm.
#'
#' @details
#' This function finds the minimum possible Cronbach's alpha by using an iterative binary search algorithm.
#' It evaluates the feasibility of each midpoint value of alpha by solving the corresponding optimization problem with the chosen method.
#'
#' The optimization methods can be specified via the \code{method} parameter, and additional control parameters for the optimization methods can be passed through the \code{...} argument. The function adjusts the upper and lower bounds of alpha until the tolerance criterion is met.
#'
#' @return A numeric value representing the minimum possible Cronbach's alpha.
#'
#' @seealso \code{\link{compute_alpha_max}}, \code{\link{examine_alpha_bound}}
#' @export


compute_alpha_min <- function(
    n_person,        # integer
    sigma_x_info,    # list containing matrix, vector, and float
    sigma_y_info,    # list containing matrix, vector, and float
    score_max = 1,   # integer
    alpha_lb = 0.0,  # float
    alpha_ub = 1.0,  # float
    tol = 1e-3,      # float
    num_try = 1,      # integer
    method = "GA",
    ...
) {
  # The function to compute the smallest possible alpha
  #
  # Args:
  #     n_person: The number of all people
  #     sigma_x_info: The info in quadratic function of sigma_x
  #     sigma_y_info: The info in quadratic function of sigma_y
  #     score_max: The largest possible score
  #     alpha_lb: A lower bound of alpha, usually 0.0
  #     alpha_ub: An upper bound of alpha, usually 1.0
  #     tol: The desired accuracy of lower and upper bound of alpha
  #     num_try: The number of optimization algorithms we run
  #
  # Returns:
  #     ans: The smallest possible alpha

  lb <- alpha_lb + 0.0
  ub <- alpha_ub + 0.0

  while ((ub - lb) > tol) {
    alpha_mid <- (ub + lb) / 2
    result_list <- examine_alpha_bound(
      alpha_mid,
      n_person,
      sigma_x_info,
      sigma_y_info,
      'min',
      score_max = score_max,
      num_try = num_try,
      method = method,
      ...
    )
    result <- result_list[[1]]

    if (result) {
      ub <- alpha_mid
    } else {
      lb <- alpha_mid
    }
  }

  return((lb + ub) / 2)
}
