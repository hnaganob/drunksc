# Lattice_Complete                                      ---- function ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(network)

# returns:
#
# Adj: n nodes adjacency matrix
# Crd: Adj's circular coordinates 
#
#

Lattice_Complete <- function( n = 5 )
{
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Adjacency Matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  adj <- matrix( 1 , ncol = n , nrow = n )
  diag( adj ) <- 0
  
  Net <- as.network( adj , directed = F )
  Adj <- as.matrix( Net , matrix.type = "adjacency" )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  theta <- seq( 0 , n , 1 ) * 2*pi/n
  x <- sin( theta[-(n+1)] )
  y <- cos( theta[-(n+1)] )
  
  Crd <- as.data.frame( cbind( x , y ) )
  
  plot( Net ,
        coord = Crd , jitter = F ,
        displaylabels = T ,
        label.pos = 1 ,
        edge.col = "gray" )
  
  return( list( Adj = Adj , Crd = Crd ) )
  
}

# Lat_Comp <- Lattice_Comp( 64 )
# Adj_Comp <- Lat_Comp$Adj
# Crd_Comp <- Lat_Comp$Crd
# 
# plot( as.network( Adj_Comp , directed = F) ,
#       coord = Crd_Comp , jitter = F ,
#       edge.col = "gray" ,
#       displaylabels = T ,
#       label.pos = 1 )
