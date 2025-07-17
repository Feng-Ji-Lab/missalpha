unknown_loc <- function(scores_mat) {

  
  n_person <- nrow(scores_mat)
  n_item <- ncol(scores_mat)
  unknown_mat_to_list <- matrix(0, nrow = n_person, ncol = n_item)
  unknown_list_to_mat <- list()
  count <- 0
  
  for (iter_person in 1:n_person) {
    for (iter_item in 1:n_item) {
      if (is.na(scores_mat[iter_person, iter_item])) {
        unknown_mat_to_list[iter_person, iter_item] <- count
        unknown_list_to_mat[[count + 1]] <- c(iter_person, iter_item)
        count <- count + 1
      } else {
        unknown_mat_to_list[iter_person, iter_item] <- -1
      }
    }
  }
  unknown_list_to_mat = lapply(unknown_list_to_mat, function(x) x - 1)
  
  return(list(count = count, unknown_mat_to_list = unknown_mat_to_list, unknown_list_to_mat = unknown_list_to_mat))
}