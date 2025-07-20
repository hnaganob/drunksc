# Lattice_Cube                                          ---- function ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(network)

# returns:
#
# Adj: nX x nY x nZ nodes adjacency matrix
# Crd: Adj's cubic coordinates 



Lattice_Cube <- function( nX = 3 , nY = 4 , nZ = 5 , Torus = T )
{
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Adjacency Matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~
  # 1d
  #~~~~~~~~~~~~~~~
  adj <- matrix( 0 , ncol = nX , nrow = nX )
  
  for( i in 1:nX )
  {
    if( i != nX )
    {
      adj[ i , i+1 ] <- 1
    }
    # wrap it
    if( Torus )
    {
      adj[ 1 , nX ] <- 1
    }
  }
  
  #~~~~~~~~~~~~~~~
  # 2d
  #~~~~~~~~~~~~~~~
  adj <- kronecker( diag(nY) , adj )
  
  for( j in 1:nY )
  {
    for( i in 1:nX )
    {
      if( j != nY )
      {
        adj[ i + (j-1)*(nX) ,  i + (j)*(nX) ] <- 1
      }
      # wrap around
      if( Torus )
      {
        adj[ i , i + (nY-1)*(nX) ] <- 1
      }
    }
  }
  
  #~~~~~~~~~~~~~~~
  # 3d
  #~~~~~~~~~~~~~~~
  adj <- kronecker( diag(nZ) , adj )

  for( k in 1:nZ )
  {
    for( ij in 1:(nX*nY) )
    {
      if( k != nZ )
      {
        adj[ ij + (k-1)*(nX*nY) , ij + (k)*(nX*nY) ] <- 1
      }
      # wrap around
      if( Torus )
      {
        adj[ ij , ij + (nZ-1)*(nX*nY) ] <- 1
      }
    }
  }

  Net <- as.network( adj , directed = F )
  Adj <- as.matrix( Net , matrix.type = "adjacency" )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- rep( 1:nX , times=nY*nZ+1e-7 )  # See ?rep
  y <- rep( 1:nY , each=nX+1e-7 , times=nZ+1e-7 ) 
  z <- rep( 1:nZ , each=nX*nY+1e-7 ) 
  
  xShift <- y * 1/nY
  yShift <- z * (nZ*2)
  
  x <- nY * (x + xShift) * 2
  y <- -y - yShift
  # plot( x , y )
  
  Crd <- as.data.frame( cbind( x , y ) )
  # Crd <- Crd - matrix(colMeans(Crd),ncol=2,nrow=dim(Crd)[1],byrow=T)
  
  # plot( Net ,
  #       coord = Crd , jitter = F ,
  #       displaylabels = T ,
  #       label.pos = 1 ,
  #       edge.col = "gray" )
  
  return( list( Adj = Adj , Crd = Crd ) )
  
}

# Lat_Cub <- Lattice_Cube( nX = 5 , nY = 4 , nZ = 6 , Torus = T )
# Adj_Cub <- Lat_Cub$Adj
# Crd_Cub <- Lat_Cub$Crd
# 
# plot( as.network( Adj_Cub , directed = F) ,
#       coord = Crd_Cub , jitter = F ,
#       edge.col = "gray" ,
#       displaylabels = T ,
#       label.pos = 1 )
