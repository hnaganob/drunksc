lattice_complete <- function(n = 5, display_plot = TRUE) {
  # adjacency matrix ----
  adj <- matrix(1, ncol = n, nrow = n, dimnames = list(1:n, 1:n))
  diag(adj) <- 0


  # coordinates ----
  theta <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  x <- sin(theta)
  y <- cos(theta)
  coord <- data.frame(x = x, y = y)


  # network ----
  net <- as.network(adj, directed = FALSE)


  # plot ----
  if (isTRUE(display_plot)) {
    par(mai = rep(0, 4))
    plot(
      net,
      coord = coord,
      jitter = FALSE,
      displaylabels = TRUE,
      label.pos = 1,
      edge.col = "gray"
    )
  }

  return(list(adj = adj, coord = coord, net = net))
}
