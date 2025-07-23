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
