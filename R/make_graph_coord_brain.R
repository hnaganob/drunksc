make_graph_coord_brain <- function(
    adjacency_matrix,
    mode = NULL,
    seed = 0L) {
  
  # number of roi
  n_roi <- nrow(adjacency_matrix)
  
  # for consistent coordinates
  set.seed(seed)
  
  # get network
  net <- network(adjacency_matrix, directed = FALSE)
  
  # get edgelist
  edgelist <- as.matrix(net, matrix.type = "edgelist")
  
  # vertex layout function with Fruchterman & Reingold algorithm as default
  coord_fr <- network.layout.fruchtermanreingold(net, NULL)
  
  # vertex layout in circle
  theta <- seq(1, n_roi, 1) * 2 * pi / n_roi - pi / n_roi
  x <- -sin(theta)
  y <- cos(theta)
  y_mirror <- rep(y[1:(n_roi / 2)], 2)  # Left: 1-34; Right: 35-68
  coord_cir <- data.frame(x = x , y = y_mirror )
  
  return(list(coord_fruchtermanreingold = coord_fr,
              coord_circle = coord_cir))
}
    
seed <- 50

# n <- n_roi
# 
# theta <- seq(1, n, 1) * 2 * pi / n - pi / n
# x <- -sin(theta)
# y <- cos(theta)
# y_mirror <- rep(y[1:(n / 2)], 2)
# 
# coordinates <- data.frame(x = x , y = y_mirror )

plot(coordinates)
text(coordinates[,1], coordinates[,2], 1:n)
abline(v=0)

plot(as.network(adj_dti, directed = FALSE), coord = coordinates)
text(coordinates[,1] *1.1, coordinates[,2] * 1.1, 
     colnames(adj_dti), cex = 0.3)
