# HexLattice                                            ---- function ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(network)

# returns:
#
# Adj: nX x nY nodes adjacency matrix
# Crd: Adj's hexagonal coordinates 
#
# see p.438 of Cressie (1993)

Lattice_Hexagon <- function( nX = 5 , nY = 7 , Torus = T )
{
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Adjacency Matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nodeList <- list()
  for( i in 1:nY )
  {
    nodeList[[i]] <- (i-1)*nX + 1:nX
  }
  
  adj <- matrix( 0 , ncol = nX*nY , nrow = nX*nY )
  
  for( i in 1:nY )
  {
    for( j in 1:nX )
    {
      adj[ nodeList[[i]][j] , nodeList[[i]][j+1] ] <- 1
    }
  }
  
  for( i in 1:(nY-1) )
  {
    for( j in 1:nX )
    {
      adj[ nodeList[[i]][j] , nodeList[[i+1]][j] ] <- 1
    }
  }
  
  for( i in 1:nY )
  {
    for( j in 1:(nX-1) )
    {
      if( i %% 2 == 0 )
      {
        adj[ nodeList[[i]][j] , nodeList[[i-1]][j+1] ] <- 1
        if( i != nY )
        {
          adj[ nodeList[[i]][j] , nodeList[[i+1]][j+1] ] <- 1
        }
      }
    }
  }
  
  if( Torus )
  {
    for( i in 1:nY )
    {
      adj[ nodeList[[i]][nX] , nodeList[[i]][1] ] <- 1
      if( i %% 2 == 0 )
      {
        adj[ nodeList[[i]][nX] , nodeList[[i-1]][1] ] <- 1
        if( i == nY )
        {
          adj[ nodeList[[i]][nX] , nodeList[[1]][1] ] <- 1
          adj[ nodeList[[i]][nX] , nodeList[[1]][nX] ] <- 1
          for( j in 1:(nX-1) )
          {
            adj[ nodeList[[i]][j] , nodeList[[1]][j] ] <- 1
            adj[ nodeList[[i]][j] , nodeList[[1]][j+1] ] <- 1
          }
        }else
        {
          adj[ nodeList[[i]][nX] , nodeList[[i+1]][1] ] <- 1
        }
      }
    }
    if( nY %% 2 != 0 )
    {
      for( j in 1:(nX-1) )
      {
        adj[ nodeList[[nY]][j] , nodeList[[1]][j] ] <- 1
        adj[ nodeList[[nY]][j] , nodeList[[1]][j+1] ] <- 1
      }
      adj[ nodeList[[nY]][nX] , nodeList[[1]][nX] ] <- 1
      adj[ nodeList[[nY]][nX] , nodeList[[1]][1] ] <- 1
    }
  }
  
  Net <- as.network( adj , directed = F )
  Adj <- as.matrix( Net , matrix.type = "adjacency" )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- rep( 1:nX , times = nY+1e-7 )  # See ?rep
  y <- rep( 1:nY , times = nX+1e-7 )  # See ?rep
  y <- -y[ order(y) ]
  
  is.even <- function(x) x %% 2 == 0
  x <- ifelse(is.even(y),x+0.5,x)
  # plot( x , y )
  Crd <- as.data.frame( cbind( x , y ) )
  
  # plot( Net , 
  #       coord = Crd , jitter = F ,
  #       displaylabels = T ,
  #       label.pos = 1 ,
  #       edge.col = "gray" )
  
  return( list( Adj = Adj , Crd = Crd ) )
  
}

# Lat_Hex <- Lattice_Hexagon( 9 , 8 , Torus = T ) 
# Adj_Hex <- Lat_Hex$Adj
# Crd_Hex <- Lat_Hex$Crd
# 
# plot( as.network( Adj_Hex , directed = F) ,
#       coord = Crd_Hex , jitter = F ,
#       edge.col = "gray" ,
#       displaylabels = T ,
#       label.pos = 1 )
