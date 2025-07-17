#' Solve Quadratic Programming Problem using DEoptim
#'
#' This function solves a quadratic programming problem using the Differential Evolution optimization method from the \code{DEoptim} package.
#'
#' @param n An integer representing the number of decision variables.
#' @param A A matrix representing the quadratic coefficients.
#' @param b A numeric vector representing the linear coefficients.
#' @param c A numeric scalar representing the constant term in the objective function.
#' @param x_max An integer representing the upper bound for the decision variables. Default is 1.
#' @param print_message A logical value indicating whether to print optimization details. Default is FALSE.
#' @param NP An integer specifying the population size for the DEoptim algorithm. Default is 50.
#' @param itermax An integer specifying the maximum number of iterations. Default is 100.
#' @param ... Additional control parameters for \code{DEoptim}.
#'
#' @return A list containing:
#' \item{f_cd}{The optimal objective function value.}
#' \item{x_value}{The optimal values of the decision variables.}
#'
#' @import DEoptim
#' @seealso \code{\link{qp_solver}}, \code{\link{qp_solver_GA}}, \code{\link{qp_solver_nloptr}}
#' @export


qp_solver_DEoptim <- function(n, A, b, c, x_max = 1, print_message = FALSE, NP = 100, itermax = 100, ...) {
  # Define the objective function
  objective_DE <- function(x) {
    (1/2) * t(x) %*% A %*% x + sum(b * x) + c
  }

  # Lower and upper bounds
  lower <- rep(0, n)
  upper <- rep(x_max, n)

  # Run DEoptim
  res <- DEoptim(fn = objective_DE,
                 lower = lower,
                 upper = upper,
                 control = DEoptim.control(NP = NP, itermax = itermax, trace = print_message))

  # Get the result
  f_cd <- res$optim$bestval
  x_value <- res$optim$bestmem

  if (print_message) {
    cat("DEoptim: objective", f_cd, "\n")
    print(x_value)
  }

  return(list(f_cd = f_cd, x_value = x_value))
}

#' Solve Quadratic Programming Problem using GA
#'
#' This function solves a quadratic programming problem using the Genetic Algorithm (GA) from the \code{GA} package.
#'
#' @param n An integer representing the number of decision variables.
#' @param A A matrix representing the quadratic coefficients.
#' @param b A numeric vector representing the linear coefficients.
#' @param c A numeric scalar representing the constant term in the objective function.
#' @param x_max An integer representing the upper bound for the decision variables. Default is 1.
#' @param print_message A logical value indicating whether to print optimization details. Default is FALSE.
#' @param maxiter An integer specifying the maximum number of iterations. Default is 1000.
#' @param popSize An integer specifying the population size for the GA. Default is 50.
#' @param pmutation A numeric value for mutation probability. Default is 0.2.
#' @param elitism An integer specifying the number of elite individuals to carry over to the next generation. Default is 5.
#' @param monitor A logical value indicating whether to display progress. Default is FALSE.
#' @param seed A numeric value used for the random number generator. Default is 123.
#' @param ... Additional control parameters for \code{ga}.
#'
#' @return A list containing:
#' \item{f_cd}{The optimal objective function value.}
#' \item{x_value}{The optimal values of the decision variables.}
#'
#' @import GA
#' @seealso \code{\link{qp_solver}}, \code{\link{qp_solver_DEoptim}}, \code{\link{qp_solver_nloptr}}
#' @export


qp_solver_GA <- function(n, A, b, c, x_max = 1, print_message = FALSE, maxiter = 1000,
                         popSize = 50,
                         pmutation = 0.2,
                         elitism = 5,
                         monitor = FALSE,
                         seed = 123, ...) {
  objective_GA <- function(x) {
    (1/2) * t(x) %*% A %*% x + sum(b * x) + c
  }

  lower <- rep(0, n)
  upper <- rep(x_max, n)

  res <- ga(type = "real-valued",
            fitness = function(x) -objective_GA(x),
            lower = lower,
            upper = upper,
            maxiter = maxiter,
            popSize = popSize,
            pmutation = pmutation,
            elitism = elitism,
            monitor = monitor,
            seed = seed
  )

  f_cd <- -res@fitnessValue
  if (is.matrix(res@solution)) {
    x_value <- as.numeric(res@solution[1, ])
  } else {
    x_value <- as.numeric(res@solution)
  }

  x_value <- matrix(x_value, nrow = n, ncol = 1)

  if (print_message) {
    cat("GA: objective", f_cd, "\n")
    print(x_value)
  }

  return(list(f_cd = f_cd, x_value = x_value))
}
#' Solve Quadratic Programming Problem using nloptr
#'
#' This function solves a quadratic programming problem using the Sequential Least Squares Programming (SLSQP) algorithm from the \code{nloptr} package.
#'
#' @param n An integer representing the number of decision variables.
#' @param A A matrix representing the quadratic coefficients.
#' @param b A numeric vector representing the linear coefficients.
#' @param c A numeric scalar representing the constant term in the objective function.
#' @param x_max An integer representing the upper bound for the decision variables. Default is 1.
#' @param print_message A logical value indicating whether to print optimization details. Default is FALSE.
#' @param xtol_rel A numeric value specifying the relative tolerance for convergence. Default is \code{1e-8}.
#' @param maxeval An integer specifying the maximum number of function evaluations. Default is 10000.
#' @param print_level An integer controlling the verbosity of output. Default is 0.
#' @param ... Additional control parameters for \code{nloptr}.
#'
#' @return A list containing:
#' \item{f_cd}{The optimal objective function value.}
#' \item{x_value}{The optimal values of the decision variables.}
#'
#' @import nloptr
#' @seealso \code{\link{qp_solver}}, \code{\link{qp_solver_DEoptim}}, \code{\link{qp_solver_GA}}
#' @export

qp_solver_nloptr <- function(n, A, b, c, x_max = 1, print_message = FALSE,       xtol_rel = 1e-8,
                             maxeval = 10000,
                             print_level = 0, ...) {
  # Define the objective function
  objective_nloptr <- function(x) {
    (1/2) * t(x) %*% A %*% x + sum(b * x) + c
  }

  # Gradient of the objective function
  grad_objective_nloptr <- function(x) {
    A %*% x + b
  }

  # Lower and upper bounds
  lower <- rep(0, n)
  upper <- rep(x_max, n)

  # Initial guess
  x0 <- rep(x_max / 2, n)

  # Optimization using nloptr with SQP algorithm
  res <- nloptr(
    x0 = x0,
    eval_f = objective_nloptr,
    eval_grad_f = grad_objective_nloptr,
    lb = lower,
    ub = upper,
    opts = list(
      algorithm = "NLOPT_LD_SLSQP",
      xtol_rel = xtol_rel,
      maxeval = maxeval,
      print_level = print_level
    )
  )

  f_cd <- res$objective
  x_value <- res$solution

  x_value <- matrix(x_value, nrow = n, ncol = 1)

  if (print_message) {
    cat("NLOPT_LD_SLSQP: objective", f_cd, "\n")
    print(x_value)
  }

  return(list(f_cd = f_cd, x_value = x_value))
}

#' General Solver for Quadratic Programming Problems
#'
#' This function provides a general interface to solve quadratic programming problems using different optimization methods.
#' It supports Genetic Algorithm (GA), Differential Evolution (DEoptim), and Sequential Least Squares Programming (SLSQP).
#'
#' @param n An integer representing the number of decision variables.
#' @param A A matrix representing the quadratic coefficients.
#' @param b A numeric vector representing the linear coefficients.
#' @param c A numeric scalar representing the constant term in the objective function.
#' @param x_max An integer representing the upper bound for the decision variables. Default is 1.
#' @param method A character string specifying the optimization method to use. Can be \code{'GA'}, \code{'DEoptim'}, or \code{'nloptr'}.
#' @param print_message A logical value indicating whether to print optimization details. Default is FALSE.
#' @param ... Additional control parameters passed to the chosen optimization method.
#'
#' @return A list containing:
#' \item{f_cd}{The optimal objective function value.}
#' \item{x_value}{The optimal values of the decision variables.}
#'
#' @seealso \code{\link{qp_solver_DEoptim}}, \code{\link{qp_solver_GA}}, \code{\link{qp_solver_nloptr}}
#' @export


qp_solver <- function(n, A, b, c, x_max = 1, method = "GA", print_message = FALSE, ...) {

  res <- switch(method,
                "GA" = qp_solver_GA(n, A, b, c, x_max = x_max, print_message = print_message, ...),
                "DEoptim" = qp_solver_DEoptim(n, A, b, c, x_max = x_max, NP = n*10, print_message = print_message, ...),
                "nloptr" = qp_solver_nloptr(n, A, b, c, x_max = x_max, print_message = print_message, ...),
                stop("Invalid method. Choose from 'GA', 'DEoptim', or 'nloptr'")
  )

  return(res)
}
