CliqueSummary <- function (adj, 
                           displayplot = F ) {
  
  library(igraph)
  library(network)

  # format adjacency matrix compatible for {network} and {igraph}
  net <- as.network(adj, directed = F)
  adj <- as.matrix(net, matrix.type = "adjacency")
  
  # list of node names
  node_list <- colnames(adj)
  
  # igraph
  eye_graph <- graph.adjacency(adj)

  # list of all cliques in eye_graph
  clq <- cliques(eye_graph)

  # prepare clique summary table (n_node x k)
  clq_max <- length(node_list)
  clq_summary_table <- matrix(NA, clq_max, clq_max)
  rownames(clq_summary_table) <- node_list  # node names
  colnames(clq_summary_table) <- 1:clq_max  # all possible k_cique

  # prepare clique list (n_node)
  clq_list <- list()
  
  for ( i in 1:length(node_list) )
  {
    k_clq_node_i <- NULL
    clq_list_node_i <- data.frame( matrix( ncol = 2, nrow = 0 ) )
    clq_id_ij <- NULL

    for (j in 1:length(clq)) {
      
      if (sum(clq[[j]] %in% i) != 0) {
        
        # all members of nodes at clique j
        nodes_clq_j <- as.vector(clq[[j]]) # it gives a numeric vector
        nodes_clq_j <- node_list[as.numeric(nodes_clq_j)]
        
        # number of nodes at clique j
        n_nodes_clq_j <- length(nodes_clq_j)
        
        # vector to a character string
        clq_j <- paste0(nodes_clq_j, collapse = ",")
        
        # list of cliques at node i
        clq_list_node_i <- rbind(clq_list_node_i, t(c(n_nodes_clq_j, clq_j)))
        clq_id_ij <- c(clq_id_ij, j)  # clq list numbers used for rownames
        
        # number of k_cliques at node i
        k_clq_node_i <- c( k_clq_node_i , n_nodes_clq_j )
        clq_summary_table_node_i <- tabulate( k_clq_node_i , nbins = clq_max )
      }
      
    }
    
    # clique list for node i
    colnames(clq_list_node_i) <- c("k", "clique_nodes")
    rownames(clq_list_node_i) <- clq_id_ij
    clq_list_node_i <- clq_list_node_i[order(as.numeric(clq_list_node_i$k)), ]
    clq_list[[ node_list[i] ]] <- clq_list_node_i
    
    # clique summary table
    clq_summary_table[i,] <- clq_summary_table_node_i
    
  }
  
  # clique summary table
  k_drop <- which(colSums(clq_summary_table) != 0)
  clq_summary_table <- clq_summary_table[, k_drop]
  names(attributes(clq_summary_table)$dimnames) <- c("node", "k")
  
  # check {igraph} plot
  if (displayplot) plot(eye_graph)
  
  return(
    list(
      Table = clq_summary_table,
      List = clq_list
    )
  )

}


# Note
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~
# clique()
#~~~~~~~~~~~~~~~~
# clique function 
#
# clq <- cliques( eye_graph ) --- line 20
#
# [[39]]
# + 2/10 vertices, named, from dd81440:
#   [1] A B
#
# > j
# [1] 39
# > clq[[j]] 
# + 2/10 vertices, named, from dd81440:
#   [1] A B
#
# > i
# [1] 1
#
# > clq[[j]] %in% i
# [1]  TRUE FALSE
#
# > clq[[j]] %in% "A"
# [1] FALSE FALSE
#
# > clq[[j]] %in% 1
# [1]  TRUE FALSE
#
# > clq[[j]] %in% 2
# [1] FALSE  TRUE
#
# > clq[[j]] %in% 3
# [1] FALSE FALSE