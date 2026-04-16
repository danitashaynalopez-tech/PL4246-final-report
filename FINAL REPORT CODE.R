library(igraph)

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/plots", showWarnings = FALSE)

responses1 <- read.csv("class-responses_AY25Sem2.csv")
g1 <- graph_from_data_frame(d=responses1, directed = TRUE)
summary(g1) #the network has 52 nodes and 409 directed and weighted edges 
responses2 <- read.csv("class-responses_AY24Sem1.csv")
g2 <- graph_from_data_frame(d=responses2, directed = TRUE) 
summary(g2) #the network has 37 nodes and 236 directed and weighted edges


compute_metrics <- function(g){g_undirected <- as_undirected(g, mode = "collapse")
  list(
      nodes = vcount(g), 
      edges = ecount(g), 
      density = edge_density(g), 
      reciprocity = reciprocity(g), 
      clustering = transitivity(g_undirected, type = "global"), 
      indeg_cent = centr_degree(g, mode ="in")$centralization, 
      outdeg_cent = centr_degree(g, mode = "out")$centralization
  )
}

metrics1 <- compute_metrics(g1)
metrics2 <- compute_metrics(g2)
metrics_df <- rbind(
  data.frame(dataset = "AY25Sem2", metrics1),
  data.frame(dataset = "AY24Sem1", metrics2)
)
write.csv(metrics_df, "outputs/metrics.csv", row.names = FALSE)


analyse_reciprocity <- function(g){
  edges <- as_data_frame(g, what = "edges") 
  
  edges$reciprocated <- mapply(function(x, y){are_adjacent(g, y, x)
    }, edges$from, edges$to)
  
  t.test(weight ~ reciprocated, data = edges)
}

recip1 <- analyse_reciprocity(g1)
recip2 <- analyse_reciprocity(g2)
sink("outputs/reciprocity_tests.txt")
recip1
recip2
sink()


g1_u <- as_undirected(g1, mode = "collapse")
g2_u <- as_undirected(g2, mode = "collapse")
mean_distance(g1_u, directed = FALSE)
mean_distance(g2_u, directed = FALSE) 
diameter(g1_u, directed = FALSE) 
diameter(g2_u, directed = FALSE) 

get_communities <- function(g) {
  g_undirected <- as_undirected(g, mode ="collapse")
  cluster_louvain(g_undirected)
}

comm1 <- get_communities(g1)
comm2 <- get_communities(g2)
length(unique(membership(comm1)))
length(unique(membership(comm2)))


top_bridges <- function(g) {sort(betweenness(g), decreasing = TRUE)[1:10]
}
top_bridges(g1)
top_bridges(g2)

png("outputs/plots/g1.png")
plot(g1)
dev.off()
png("outputs/plots/g2.png")
plot(g2)
dev.off()

