#' Check Feasibility of Alpha Bound for Optimization Problem
#'
#' This function checks whether a given value of \code{alpha} is a feasible solution
#' to a min/max optimization problem using quadratic functions for sigma_x and sigma_y.
#' The function supports different optimization methods and iteratively attempts to solve the problem.
#'
#' @param alpha A numeric value representing the \code{alpha} value to check.
#' @param n_person An integer representing the number of individuals or rows in the data.
#' @param sigma_x_info A list containing the quadratic function information for sigma_x, including:
#' \itemize{
#'   \item A matrix representing the quadratic coefficients for sigma_x.
#'   \item A vector representing the linear coefficients for sigma_x.
#'   \item A scalar constant term for sigma_x.
#' }
#' @param sigma_y_info A list containing the quadratic function information for sigma_y, including:
#' \itemize{
#'   \item A matrix representing the quadratic coefficients for sigma_y.
#'   \item A vector representing the linear coefficients for sigma_y.
#'   \item A scalar constant term for sigma_y.
#' }
#' @param alpha_type A character string indicating whether the problem is to minimize or maximize alpha.
#'   It must be either \code{'min'} or \code{'max'}.
#' @param score_max An integer representing the largest possible score for any test item. Default is 1.
#' @param num_try An integer specifying the number of times to run the optimization algorithm. Default is 1.
#' @param method A character string specifying the optimization method to use.
#'   Options are \code{'GA'} (Genetic Algorithm), \code{'DEoptim'} (Differential Evolution), and \code{'nloptr'} (Sequential Least Squares Programming). Default is 'GA'.
#' @param ... Additional parameters passed to the optimization algorithm.
#'
#' @details
#' The function combines quadratic information from \code{sigma_x_info} and \code{sigma_y_info}
#' to form a new optimization problem. The optimization checks whether the value of \code{alpha} is feasible
#' for either a minimization or maximization problem, depending on the value of \code{alpha_type}.
#'
#' The function supports multiple optimization methods, including Genetic Algorithm (GA),
#' Differential Evolution (DEoptim), and Sequential Least Squares Programming (nloptr).
#' Additional control parameters can be passed through the \code{...} argument to fine-tune the optimization process.
#'
#' For each iteration, the function calls \code{qp_solver} with the combined quadratic function
#' and checks whether the objective function's value is feasible (i.e., less than or equal to 0).
#'
#' @return A list with the following elements:
#' \item{result}{A logical value indicating whether the \code{alpha} is feasible (\code{TRUE}) or not (\code{FALSE}).}
#' \item{x_value}{A numeric vector representing the optimal values of the decision variables, or \code{NULL} if not feasible.}
#'
#' @seealso \code{\link{qp_solver}}, \code{\link{compute_alpha_min}}, \code{\link{compute_alpha_max}}
#' @export


examine_alpha_bound <- function(
    alpha,        # Numeric: The value of alpha we want to check
    n_person,     # Integer: The number of all people
    sigma_x_info, # List: The info in quadratic function of sigma_x (A_mat_x, b_array_x, c_scaler_x)
    sigma_y_info, # List: The info in quadratic function of sigma_y (A_mat_y, b_array_y, c_scaler_y)
    alpha_type,   # Character: 'min' or 'max' indicating the problem type
    score_max = 1,# Integer: The largest possible score
    num_try = 1,   # Integer: The number of optimization algorithms we run
    method = "GA", # Character: The optimization method to use ('GA', 'DEoptim', 'nloptr')
    ...           # Additional control parameters for the chosen optimization method
) {
  # The function to check whether alpha is a feasible solution to min/max problem

  if (alpha_type == 'min') {
    const_x <- -1.0
    const_y <- 1.0 - alpha * (n_person - 1.0) / n_person
  } else if (alpha_type == 'max') {
    const_x <- 1.0
    const_y <- -1.0 + alpha * (n_person - 1.0) / n_person
  } else {
    stop('alpha_type can only be "min" or "max"')
  }

  # Unpack sigma_x_info and sigma_y_info
  A_mat_x <- sigma_x_info[[1]]
  b_array_x <- sigma_x_info[[2]]
  c_scaler_x <- sigma_x_info[[3]]

  A_mat_y <- sigma_y_info[[1]]
  b_array_y <- sigma_y_info[[2]]
  c_scaler_y <- sigma_y_info[[3]]

  # Compute the combined A_mat, b_array, c_scaler
  A_mat <- const_x * A_mat_x + const_y * A_mat_y
  b_array <- const_x * b_array_x + const_y * b_array_y
  c_scaler <- const_x * c_scaler_x + const_y * c_scaler_y

  for (num_iter in 1:num_try) {
    result <- qp_solver(
      n = length(b_array),
      A = A_mat,
      b = b_array,
      c = c_scaler,
      x_max = score_max,
      method = method,
      print_message = FALSE,
      ...
    )
    opt_value <- result$f_cd
    x_value <- result$x_value

    if (opt_value <= 0) {
      return(list(result = TRUE, x_value = as.numeric(x_value)))
    }
  }

  return(list(result = FALSE, x_value = NULL))
}
