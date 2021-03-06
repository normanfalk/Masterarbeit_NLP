#Quanted Test
library(quanteda)

data_unique <- dataset %>% 
  group_by(comment_id) %>% 
  slice(1)

sotu_corpus <- corpus(data_unique$Text, docnames = data_unique$comment_id, 
                      docvars = data.frame(title = data_unique$submission_title, id = data_unique$submission_id))
ndoc(sotu_corpus)

substr(texts(sotu_corpus)[1], 0, 200)

corpus_sentences <- corpus_reshape(sotu_corpus, to = "sentences")

ndoc(corpus_sentences)

texts(corpus_sentences)[1]



lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")

# read an extended stop word list
stopwords_extended <- stop_words$word

# Preprocessing of the corpus of sentences
corpus_tokens <- corpus_sentences %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

# calculate multi-word unit candidates
sotu_collocations <- textstat_collocations(corpus_tokens, min_count = 25)
sotu_collocations <- sotu_collocations[1:250, ]

corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)


minimumFrequency <- 10

# Create DTM, prune vocabulary and set binary values for presence/absence of types
binDTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = minimumFrequency, max_docfreq = Inf) %>% 
  dfm_weight("boolean")

# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTM

as.matrix(coocCounts[202:205, 202:205])

coocTerm <- "passion"
k <- nrow(binDTM)
ki <- sum(binDTM[, coocTerm])
kj <- colSums(binDTM)
names(kj) <- colnames(binDTM)
kij <- coocCounts[coocTerm, ]



########## MI: log(k*kij / (ki * kj) ########
mutualInformationSig <- log(k * kij / (ki * kj))
mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]

########## DICE: 2 X&Y / X + Y ##############
dicesig <- 2 * kij / (ki + kj)
dicesig <- dicesig[order(dicesig, decreasing=TRUE)]

########## Log Likelihood ###################
logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
               + (k - ki - kj + kij) * log(k - ki - kj + kij) 
               + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
               - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
logsig <- logsig[order(logsig, decreasing=T)]

# Put all significance statistics in one Data-Frame
resultOverView <- data.frame(
  names(sort(kij, decreasing=T)[1:10]), sort(kij, decreasing=T)[1:10],
  names(mutualInformationSig[1:10]), mutualInformationSig[1:10], 
  names(dicesig[1:10]), dicesig[1:10], 
  names(logsig[1:10]), logsig[1:10],
  row.names = NULL)
colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms", "MI", "Dice-Terms", "Dice", "LL-Terms", "LL")
print(resultOverView)


# Read in the source code for the co-occurrence calculation
source("calculateCoocStatistics.R")
# Definition of a parameter for the representation of the co-occurrences of a concept
numberOfCoocs <- 15
# Determination of the term of which co-competitors are to be measured.
coocTerm <- "faang"


coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
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
  coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
  
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
par(mai=c(0,0,1,0)) 

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


#Styling:
windowsFonts(gillius = windowsFont("Gill Sans MT"))


ggraph(graphNetwork, layout = "with_kk", )+
  geom_edge_arc(alpha = .5, 
                 edge_width = scales::rescale(E(graphNetwork)$sig, to = c(0.1, 1)),
                 #edge_width = 0.5,
                 show.legend = FALSE,
                strength = 0.1,
                color = "DarkGray")+
  geom_node_point(size = scales::rescale(log(degree(graphNetwork)), to = c(5, 15)),
                  color = ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') )+
  geom_node_text(label = V(graphNetwork)$name)+
  #theme_graph(text=element_text(family="Gill Sans MT"))
  #theme_graph(base_family = "Gill Sans MT",)
  set_graph_style(family = "Gill Sans MT",)+
  ggsave("testplot.pdf", device = cairo_pdf)
