random_walk <- function(adj, n_steps = 1000, start_node = 1) {

  # adjacency matrix
  if (!is.matrix(adj) || nrow(adj) != ncol(adj)) {
    stop("The adjacency matrix must be a square matrix.")
  }
  # n_steps
  if (!is.numeric(n_steps) || length(n_steps) != 1 || n_steps < 1 || n_steps != as.integer(n_steps)) {
    stop("n_steps must be a positive integer.")
  }
  # start_node
  if (!is.numeric(start_node) || length(start_node) != 1 || start_node < 1 || start_node > nrow(adj)) {
    stop("start_node must be between 1 and nrow(adj).")
  }

  # prepare list of neighbors at each node
  neighbor_out_list <- list()
  for (node in 1:nrow(adj)) {
    neighbor_out_list[[node]] <- which(adj[node, ] == 1) # row is out-degree
  }

  # stop if start_node has no neighbors
  if (length(neighbor_out_list[[start_node]]) == 0) {
    stop(
      sprintf(
        "Node %d has no outgoing neighbors. Cannot proceed with the random walk.",
        start_node
      )
    )
  }


  path <- integer(n_steps)
  path[1] <- start_node
  walk_stop <- n_steps

  # random walk
  for (i in 2:n_steps) {
    current_node <- path[i - 1]
    neighbor_out <- neighbor_out_list[[current_node]]

    if (length(neighbor_out) == 0) {
      warning(
        sprintf(
          "Walk stopped at step %d: node %d has no outgoing edges.",
          i - 1, current_node
        )
      )
      walk_stop <- i - 1
      break
    }

    if (length(neighbor_out) == 1) {
      path[i] <- neighbor_out
    } else {
      path[i] <- neighbor_out[sample.int(length(neighbor_out), 1)]
    }
  }

  path <- path[seq_len(walk_stop)]

  return(path)
}
