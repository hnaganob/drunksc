#' Generate a Cycle Graph
#'
#' Construct an adjacency matrix for a cycle graph with `n` nodes.
#'
#' @param n  A positive integer specifying the number of nodes in the cycle graph.
#' @param display_plot  Logical. If `TRUE`, the lattice is plotted using `plot.network()` from the **network** package.
#'
#' @returns  A list with the following components:
#' \describe{
#'   \item{`adj`}{A symmetric adjacency matrix representing the circular lattice.}
#'   \item{`coord`}{A data frame of node coordinates arranged in a circle.}
#'   \item{`net`}{A `network` object from the **network** package representing the graph.}
#' }
#'
#' @importFrom network as.network plot.network
#' @export
#'
#' @examples
#' lat <- lattice_cycle(n = 5, display_plot = FALSE)
#' lat$adj
#' plot(lat$net, coord = lat$coord, jitter = FALSE, displaylabels = TRUE)
lattice_cycle <- function(n = 5, display_plot = TRUE) {
  stopifnot(n >= 2, is.numeric(n), n == as.integer(n))

  # adjacency matrix ----
  adj <- matrix(0, ncol = n, nrow = n, dimnames = list(1:n, 1:n))
  from <- 1:n
  to <- from %% n + 1
  adj[cbind(from, to)] <- 1

  # symmetrize adjacency matrix (undirected)
  adj <- ((adj + t(adj)) > 0) * 1


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
