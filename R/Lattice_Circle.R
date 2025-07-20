# Lattice_Circle                                        ---- function ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(network)

# returns:
#
# Adj: n nodes adjacency matrix
# Crd: Adj's circular coordinates 
#
#

Lattice_Circle <- function( n = 5 )
{
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Adjacency Matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  adj <- matrix( 0 , ncol = n , nrow = n )
  
  for( i in 1:n )
  {
    adj[ i , ifelse( i==(n-1) , n , (i+1)%%n ) ] <- 1
  }
  
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

# Lat_Cir <- Lattice_Circle( 64 )
# Adj_Cir <- Lat_Cir$Adj
# Crd_Cir <- Lat_Cir$Crd
# 
# plot( as.network( Adj_Cir , directed = F) ,
#       coord = Crd_Cir , jitter = F ,
#       edge.col = "gray" ,
#       displaylabels = T ,
#       label.pos = 1 )
