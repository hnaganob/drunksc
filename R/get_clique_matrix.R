get_clique_matrix <- function(adj_mat) {
  
  # check for symmetricity
  stopifnot(
    is.matrix(adj_mat),
            nrow(adj_mat) == ncol(adj_mat),
            all(adj_mat == t(adj_mat))
    )
  
  # make a graph (igraph)
  n <- nrow(adj_mat)
  g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", diag = FALSE)
  
  # largest clique size in the graph (igraph)
  k_max <- clique_num(g)
  
  # pre-allocate result matrix
  clq_mat <- matrix(0, n, n, dimnames = dimnames(adj_mat))
  
  
  for (k in 2:k_max) {
    clq_k <- cliques(g, min = k, max = k)
    
    if (length(clq_k) > 0) {
      edge_clq_all <- do.call(rbind, lapply(clq_k, function(x) t(combn(x, 2))))
      edge_clq <- unique(edge_clq_all)
      
      for (row_idx in seq_len(nrow(edge_clq))) {
        i <- edge_clq[row_idx, 1]
        j <- edge_clq[row_idx, 2]
        clq_mat[i, j] <- k
        clq_mat[j, i] <- k # maintain symmetry
      }
    }
  }
  
  return(clq_mat)
}
