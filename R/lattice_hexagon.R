#' Generate a Lattice for Hexagon
#'
#' Construct an adjacency matrix for a hexagonal shape lattice with \eqn{n_x \times n_y} nodes, optionally wrapped as a torus.
#'
#' @param nx Integer. Number of nodes along the x-axis. Must be 2 or greater.
#' @param ny Integer. Number of nodes along the y-axis. Must be 2 or greater.
#' @param torus Logical. If `TRUE`, the lattice wraps around horizontally and vertically to form a toroidal structure.
#' @param display_plot Logical. If `TRUE`, plot the resulting lattice using `plot.network()` from the **network** package.
#'
#' @returns  A list with the following components:
#' \describe{
#'   \item{`adj`}{A symmetric adjacency matrix representing the lattice for hexagon.}
#'   \item{`coord`}{A data frame of node coordinates for plotting.}
#'   \item{`net`}{A `network` object from the **network** package representing the lattice.}
#' }
#'
#' @importFrom network as.network plot.network
#' @export
#'
#' @examples
#' lat <- lattice_hexagon(nx = 8, ny = 8, torus = TRUE, display_plot = FALSE)
#' lat$adj
#' plot(lat$net, coord = lat$coord, jitter = FALSE, displaylabels = TRUE)
lattice_hexagon <- function(nx = 8, ny = 8, torus = TRUE, display_plot = TRUE) {
  stopifnot(
    nx >= 2, is.numeric(nx), nx == as.integer(nx),
    ny >= 2, is.numeric(ny), ny == as.integer(ny)
  )

  # adjacency matrix ----
  nodeList <- list()
  for (i in 1:ny)
  {
    nodeList[[i]] <- (i - 1) * nx + 1:nx
  }

  adj <- matrix(0, ncol = nx * ny, nrow = nx * ny)

  for (i in 1:ny)
  {
    for (j in 1:nx)
    {
      adj[nodeList[[i]][j], nodeList[[i]][j + 1]] <- 1
    }
  }

  for (i in 1:(ny - 1))
  {
    for (j in 1:nx)
    {
      adj[nodeList[[i]][j], nodeList[[i + 1]][j]] <- 1
    }
  }

  for (i in 1:ny)
  {
    for (j in 1:(nx - 1))
    {
      if (i %% 2 == 0) {
        adj[nodeList[[i]][j], nodeList[[i - 1]][j + 1]] <- 1
        if (i != ny) {
          adj[nodeList[[i]][j], nodeList[[i + 1]][j + 1]] <- 1
        }
      }
    }
  }

  if (torus) {
    for (i in 1:ny)
    {
      adj[nodeList[[i]][nx], nodeList[[i]][1]] <- 1
      if (i %% 2 == 0) {
        adj[nodeList[[i]][nx], nodeList[[i - 1]][1]] <- 1
        if (i == ny) {
          adj[nodeList[[i]][nx], nodeList[[1]][1]] <- 1
          adj[nodeList[[i]][nx], nodeList[[1]][nx]] <- 1
          for (j in 1:(nx - 1))
          {
            adj[nodeList[[i]][j], nodeList[[1]][j]] <- 1
            adj[nodeList[[i]][j], nodeList[[1]][j + 1]] <- 1
          }
        } else {
          adj[nodeList[[i]][nx], nodeList[[i + 1]][1]] <- 1
        }
      }
    }
    if (ny %% 2 != 0) {
      for (j in 1:(nx - 1))
      {
        adj[nodeList[[ny]][j], nodeList[[1]][j]] <- 1
        adj[nodeList[[ny]][j], nodeList[[1]][j + 1]] <- 1
      }
      adj[nodeList[[ny]][nx], nodeList[[1]][nx]] <- 1
      adj[nodeList[[ny]][nx], nodeList[[1]][1]] <- 1
    }
  }

  # symmetrize adjacency matrix (undirected)
  adj <- ((adj + t(adj)) > 0) * 1


  # coordinates ----
  x <- rep(1:nx, times = ny + 1e-7) # See ?rep
  y <- rep(1:ny, times = nx + 1e-7) # See ?rep
  y <- -y[order(y)]

  is.even <- function(x) x %% 2 == 0
  x <- ifelse(is.even(y), x + 0.5, x)

  coord <- data.frame(x = x, y = y)


  # network ----
  net <- as.network(adj, directed = FALSE)


  # plot ----
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

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
