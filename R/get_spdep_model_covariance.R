get_spdep_model_covariance <- function(adjacency_matrix, lambda, tau2) {
  # make sure adjacency_matrix is square
  n_roi <- nrow(adjacency_matrix)
  stopifnot(ncol(adjacency_matrix) == n_roi)

  # name handling
  rn <- rownames(adjacency_matrix)
  cn <- colnames(adjacency_matrix)
  if (is.null(rn) && is.null(cn) || !identical(rn, cn)) {
    rn <- cn <- seq_len(n_roi)
  } else if (is.null(rn)) {
    rn <- cn
  } else if (is.null(cn)) {
    cn <- rn
  }
  dimnames(adjacency_matrix) <- list(rn, cn)

  # identify isolated and connected roi
  row_sums <- rowSums(adjacency_matrix)
  idx_isolated <- which(row_sums == 0L)
  idx_connected <- which(row_sums != 0L)

  # get model covariance matrix
  Sigma <- diag(tau2, n_roi)

  if (length(idx_connected) != 0L) {
    # get covariance matrix (without isolates)
    H <- adjacency_matrix[idx_connected, idx_connected, drop = FALSE]

    # precision matrix
    Ik <- diag(nrow(H)) # binary weight (listw$style == "B")
    Q <- Ik - lambda * H
    # Dk <- diag(rowSums(H)) # degree weight (listw$style == "W")
    # Q <- Dk - lambda * H

    # check for positive definiteness
    if (inherits(try(chol(Q), silent = TRUE), "try-error")) {
      stop("I - lambda * H is not positive-definite.")
    }

    # model covariance
    S <- tau2 * solve(Q)

    # add isolated roi back
    Sigma[idx_connected, idx_connected] <- S
  }

  # name
  dimnames(Sigma) <- dimnames(adjacency_matrix)

  # convert to correlation matrix
  Rho <- cov2cor(Sigma)

  return(list(Sigma = Sigma, Rho = Rho))
}


# # model covariance
# cov_mat <- get_spdep_model_covariance(
#   adjacency_matrix = adj_dti,
#   lambda = 0.1,
#   tau2 = 0.1
# )
# 
# # heatmap color preparation
# col_correlation <-
#   colorRampPalette(c("darkred", "red", "white", "green", "darkgreen"))(255)
# breaks_correlation <-
#   seq(-1L - 10^-10, 1L + 10^-10, length.out = length(col_correlation) + 1)
# 
# # correlation heatmap
# heatmap(
#   cov_mat$Rho,
#   Rowv = NA,
#   symm = TRUE,
#   breaks = breaks_correlation,
#   col = col_correlation
# )
# 
# # covariance heatmap
# heatmap(cov_mat$Sigma, Rowv = NA, symm = TRUE)
# heatmap(cov_mat$Sigma)
# 
# 
# # a <- matrix(0, 5, 5)
# a <- matrix(1, 5, 5)
# dimnames(a) <- list(LETTERS[1:5], letters[1:5]) # different dimnames
# a[1, 3] <- a[3, 1] <- 1
# a[4, 5] <- a[5, 4] <- 1
# diag(a) <- 0
# cov_mat <- get_spdep_model_covariance(
#   adjacency_matrix = a,
#   lambda = 0.2499,
#   tau2 = 1
# )
# heatmap(
#   cov_mat$Rho,
#   Rowv = NA,
#   symm = TRUE,
#   breaks = breaks_correlation,
#   col = col_correlation
# )
