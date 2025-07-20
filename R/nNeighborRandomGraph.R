# n-Neighbor Random Graph

nNeighborRandomGraph <- function( nNode = 64 ,
                                  nNeighbor = 6 ,
                                  Seed = 1 ,
                                  Iteration = 1000 )
{
  set.seed(Seed)
  
  # list of successful adjacency matrices to keep
  KeepAdj <- list()
  KeepIt <- NULL
  
  for( it in 1:Iteration )
  {
    Adj <- matrix(0,nNode,nNode)
    
    for( i in 1:nNode )
    {
      Adj <- as.matrix(as.network(Adj,directed=F),matrix.type="adjacency")
      Link <- (1:nNode)[ -unique(as.vector(c((1:i),which(Adj[i,]==1)))) ]
      
      if( sum(Adj[i,]) < nNeighbor )
      {
        if( length(Link) > nNeighbor-sum(Adj[i,]) )
        {
          Samp <- Link[ sample.int(length(Link),nNeighbor-sum(Adj[i,])) ]
          Adj[i,Samp] <- 1
        }
      }
    }
    Adj <- as.matrix(as.network(Adj,directed=F),matrix.type="adjacency")
    length(unique(rowSums(Adj)))
    
    if(prod(unique(rowSums(Adj))==6)==1)
    {
      KeepAdj[[as.character(it)]] <- Adj
      print(paste0(it,"! You're my bitch!"))
      KeepIt <- c(KeepIt,as.character(it))
    }
    
    if( it %% 100 == 0 ){ print(it) } 
    
  }
  
  Summary <- paste0( nNeighbor , "Neighbor" , nNode , "Node_RandomAdj" )
  return( list( KeepAdj = KeepAdj ,
                KeepIt = KeepIt ) )
}
# 
# randAdj <- nNeighborRandomGraph( nNode=16 , nNeighbor=6 , Iteration=200 )
# randAdj <- randAdj$KeepAdj[[1]]
# 
# randNet <- as.network(randAdj,directed=F)
# plot(randNet)
# 
# #----------------------
# # Adj Characteristics
# #----------------------
# randAdj <- KeepAdj[['57']]
# randAdj <- KeepAdj[['161']]
# randNet <- as.network(randAdj,directed=F)
# randNet
# 
# plot(randNet)
# 
# # Graph Energy
# #---------------
# Eigen <- eigen( randAdj )
# sum( abs(Eigen$values) )
# sum( abs(eigen(HAdj)$values) )
# sum( abs(eigen(CAdj)$values) )
