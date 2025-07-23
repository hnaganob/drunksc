lattice_cube <- function(nx = 3, ny = 4, nz = 5, torus = TRUE, display_plot = TRUE) {
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
