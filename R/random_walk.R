random_walk <- function(adj, n_steps, start_node = NULL) {
  # Check that adj is a square matrix
  if (!is.matrix(adj) || nrow(adj) != ncol(adj)) {
    stop("The adjacency matrix must be a square matrix.")
  }

  # Number of nodes in the lattice
  n_nodes <- nrow(adj)

  # Initialize the walk
  walk <- integer(n_steps + 1)  # To store the visited nodes

  # Set the start node
  if (is.null(start_node)) {
    walk[1] <- sample(1:n_nodes, 1)  # Start at a random node if start_node is not provided
  } else {
    if (start_node < 1 || start_node > n_nodes) {
      stop("Start node must be within the range of nodes in the adjacency matrix.")
    }
    walk[1] <- start_node
  }

  # Perform the random walk
  for (step in 2:(n_steps + 1)) {
    current_node <- walk[step - 1]

    # Find the neighbors of the current node
    neighbors <- which(adj[current_node, ] == 1)

    # Ensure correct sampling when only one neighbor exists
    if (length(neighbors) == 1) {
      walk[step] <- neighbors
    } else if (length(neighbors) > 1) {
      walk[step] <- sample(neighbors, 1)
    } else {
      # If no neighbors, stop the walk
      walk <- walk[1:(step - 1)]
      warning("Reached a node with no neighbors; ending walk early.")
      break
    }
  }

  return(walk)
}
