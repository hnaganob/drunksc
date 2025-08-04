#' Random Walk on a Graph
#'
#' `drunksc::random_walk()` follows a single walker that moves from its current node to one of its outgoing neighbors at each step.
#' Rows and columns of `adj` are treated as out-edges and in-edges, respectively.
#' The walk terminates early if it reaches a sink (a node with zero out-degree).
#' The function then returns both the sequence of visited nodes and a
#' per-node visit count.
#'
#' @param adj
#' A square numeric adjacency matrix.
#' Entry \code{adj[i, j] != 0} indicates a directed edge from node \code{i} to node \code{j}.
#' Values are treated as logical.
#' @param n_steps A positive integer. Maximum number of steps to take (including the starting node). Defaults to \code{1000}.
#' @param start_node An integer index between \code{1} and \code{nrow(adj)}. Node at which the walk begins. Must have at least one outgoing edge.
#'
#' @returns An integer vector containing the sequence of visited node indices. Its length is \eqn{\le} \code{n_steps}; if the walker reaches a sink earlier, the vector is shorter and a warning is issued.
#'
#' @export
#'
#' @examples
#' set.seed(1224)
#' adj <- lattice_circle(n = 10)$adj
#' path <- random_walk(adj = adj, n_steps = 10000, start_node = 1)
#' barplot(table(path))
#'
#' adj[1, 10] <- 0  # remove outgoing edge from node 1 to node 10.
#' path <- random_walk(adj = adj, n_steps = 10000, start_node = 1)
#' barplot(table(path))
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
  walk_end <- n_steps

  # random walk
  for (i in 2:n_steps) {
    current_node <- path[i - 1]
    neighbor_out <- neighbor_out_list[[current_node]]

    # walk ends at a sink
    if (length(neighbor_out) == 0) {
      warning(
        sprintf(
          "Walk stopped at step %d: node %d has no outgoing edges.",
          i - 1, current_node
        )
      )
      walk_end <- i - 1
      break
    }

    if (length(neighbor_out) == 1) {
      path[i] <- neighbor_out
    } else {
      path[i] <- neighbor_out[sample.int(length(neighbor_out), 1)]
    }
  }

  path <- path[seq_len(walk_end)]

  # return(list(path = path, adj = adj))
  return(path)
}
