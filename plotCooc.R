plotCooc <- function(data ,n = 15, term) {
  source("calculateCoocStatistics.R")
  # Definition of a parameter for the representation of the co-occurrences of a concept
  numberOfCoocs <- n
  # Determination of the term of which co-competitors are to be measured.
  coocTerm <- term
  
  
  coocs <- calculateCoocStatistics(coocTerm, data, measure="LOGLIK")
  # Display the numberOfCoocs main terms
  print(coocs[1:numberOfCoocs])
  
  resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  
  # The structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  
  # Attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  # Iteration over the most significant numberOfCoocs co-occurrences of the search term
  for (i in 1:numberOfCoocs){
    
    # Calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, data, measure="LOGLIK")
    
    #print the co-occurrences
    coocs2[1:10]
    
    # Structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    
    #Append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }
  
  # Sample of some examples from resultGraph
  resultGraph[sample(nrow(resultGraph), 6), ]
  
  require(igraph)
  
  # set seed for graph plot
  set.seed(1)
  
  # Create the graph object as undirected graph
  graphNetwork <- graph.data.frame(resultGraph, directed = F)
  
  # Identification of all nodes with less than 2 edges
  verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
  # These edges are removed from the graph
  graphNetwork <- delete.vertices(graphNetwork, verticesToRemove) 
  
  # Assign colors to nodes (search term blue, others orange)
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'deepskyblue2', ifelse(V(graphNetwork)$name %in% names(coocs[1:numberOfCoocs]),'orange', 'peachpuff3')) 
  
  #V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 
  
  # Set edge colors
  E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
  # scale significance between 1 and 10 for edge width
  E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))
  
  # Set edges with radius
  E(graphNetwork)$curved <- 0.15 
  # Size the nodes by their degree of networking (scaled between 5 and 15)
  V(graphNetwork)$size <- scales::rescale(log(degree(graphNetwork)), to = c(5, 15))
  
  
  # Define the frame and spacing for the plot
  par(mai=c(0,0,0,0)) 
  
  # Final Plot
  plot(
    graphNetwork,             
    layout = layout.fruchterman.reingold, # Force Directed Layout 
    #main = paste(coocTerm, 'Kookkurenz-Graph'),
    vertex.label.family = "sans",
    vertex.label.cex = 0.8,
    vertex.shape = "circle",
    vertex.label.dist = 0.3,          # Labels of the nodes moved slightly
    vertex.frame.color = adjustcolor("black", alpha.f = .5),
    vertex.label.color = 'black',     # Color of node names
    vertex.label.font = 1,            # Font of node names
    vertex.label = V(graphNetwork)$name,      # node names
    vertex.label.cex = 1 # font size of node names
  )
  
}