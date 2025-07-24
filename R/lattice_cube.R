#' Generate a Lattice for Cube
#'
#' Construct an adjacency matrix for a cubic shape lattice with \eqn{n_x \times n_y \times n_z} nodes, optionally wrapped as a torus.
#'
#' @param nx Integer. Number of nodes along the x-axis. Must be 2 or greater.
#' @param ny Integer. Number of nodes along the y-axis. Must be 2 or greater.
#' @param nz Integer. Number of nodes along the z-axis. Must be 2 or greater.
#' @param torus Logical. If `TRUE`, wrap the lattice around each axis to form a 3D torus.
#' @param display_plot Logical. If `TRUE`, plot the resulting lattice using `plot.network()` from the **network** package.
#'
#' @returns  A list with the following components:
#' \describe{
#'   \item{`adj`}{A symmetric adjacency matrix representing the lattice for cube.}
#'   \item{`coord`}{A data frame of node coordinates for plotting.}
#'   \item{`net`}{A `network` object from the **network** package representing the lattice.}
#' }
#'
#' @importFrom network as.network plot.network
#' @export
#'
#' @examples
#' lat <- lattice_cube(nx = 4, ny = 4, nz = 4, torus = TRUE, display_plot = FALSE)
#' lat$adj
#' plot(lat$net, coord = lat$coord, jitter = FALSE, displaylabels = TRUE)
lattice_cube <- function(nx = 4, ny = 4, nz = 4, torus = TRUE, display_plot = TRUE) {
  stopifnot(
    nx >= 2, is.numeric(nx), nx == as.integer(nx),
    ny >= 2, is.numeric(ny), ny == as.integer(ny),
    nz >= 2, is.numeric(nz), nz == as.integer(nz)
  )

  # adjacency matrix ----

  # ~~~~~~~~~~~~~~~
  # 1d
  # ~~~~~~~~~~~~~~~
  adj <- matrix(0, ncol = nx, nrow = nx)

  for (i in 1:nx)
  {
    if (i != nx) {
      adj[i, i + 1] <- 1
    }
    # wrap it
    if (torus) {
      adj[1, nx] <- 1
    }
  }

  # ~~~~~~~~~~~~~~~
  # 2d
  # ~~~~~~~~~~~~~~~
  adj <- kronecker(diag(ny), adj)

  for (j in 1:ny)
  {
    for (i in 1:nx)
    {
      if (j != ny) {
        adj[i + (j - 1) * (nx), i + (j) * (nx)] <- 1
      }
      # wrap around
      if (torus) {
        adj[i, i + (ny - 1) * (nx)] <- 1
      }
    }
  }

  # ~~~~~~~~~~~~~~~
  # 3d
  # ~~~~~~~~~~~~~~~
  adj <- kronecker(diag(nz), adj)

  for (k in 1:nz)
  {
    for (ij in 1:(nx * ny))
    {
      if (k != nz) {
        adj[ij + (k - 1) * (nx * ny), ij + (k) * (nx * ny)] <- 1
      }
      # wrap around
      if (torus) {
        adj[ij, ij + (nz - 1) * (nx * ny)] <- 1
      }
    }
  }

  # symmetrize adjacency matrix (undirected)
  adj <- ((adj + t(adj)) > 0) * 1


  # coordinates ----
  x <- rep(1:nx, times = ny * nz + 1e-7) # See ?rep
  y <- rep(1:ny, each = nx + 1e-7, times = nz + 1e-7)
  z <- rep(1:nz, each = nx * ny + 1e-7)

  x_shift <- y * 1 / ny
  y_shift <- z * (nz * 2)

  x <- ny * (x + x_shift) * 2
  y <- -y - y_shift

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
