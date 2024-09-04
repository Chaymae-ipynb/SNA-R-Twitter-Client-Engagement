install.packages(c("dplyr", "tidyr", "readxl", "igraph"))
library(dplyr)
library(tidyr)
library(readxl)
library(igraph)

tweets_data <- read_excel("C:/Users/ci/Downloads/W19310-XLS-ENG.xlsx", sheet = "Tweets Data")
retweets_data <- read_excel("C:/Users/ci/Downloads/W19310-XLS-ENG.xlsx", sheet = "Retweets Data")
user_data <- read_excel("C:/Users/ci/Downloads/W19310-XLS-ENG.xlsx", sheet = "Twitter User Data")

colnames(retweets_data)
colnames(tweets_data)
colnames(user_data)


# Checking total NA values in tweets_data
total_na_tweets <- sum(is.na(tweets_data))
# Doing the same for retweets_data
total_na_retweets <- sum(is.na(retweets_data))
# Doing the same for user_data
total_na_user <- sum(is.na(user_data))
# Getting the totals
print(total_na_tweets)
print(total_na_retweets)
print(total_na_user)

#initial visualization of the network keeping retweets only
retweets_data <- retweets_data %>%
  filter(!is.na(`Vertex 1`) & !is.na(`Vertex 2`))
retweets_data <- retweets_data %>%
  mutate(`Vertex 1` = as.character(`Vertex 1`), `Vertex 2` = as.character(`Vertex 2`))
network_graph <- graph_from_data_frame(d = retweets_data, directed = TRUE)
plot(network_graph, layout = layout_with_fr(network_graph))

install.packages("visNetwork")
library(visNetwork)

visIgraph(network_graph)

V(network_graph)$name


visIgraph(network_graph) %>%
  visNodes(title = V(network_graph)$name) %>%  
  visEdges(arrows = "to") %>% 
  visInteraction(navigationButtons = TRUE, keyboard = TRUE) 

visIgraph(network_graph) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
  visInteraction(navigationButtons = TRUE, keyboard = TRUE)


visIgraph(network_graph) %>%
  visSave("network_visualization.html")
browseURL


install.packages("htmlwidgets")
library(htmlwidgets)

communities <- cluster_walktrap(network_graph)

# Using the Walktrap communities method, now we plot the graph to identify clusters
plot(communities, network_graph)

nodes_df <- data.frame(id = V(network_graph)$name, 
                       group = membership(communities),
                       title = V(network_graph)$name,
                       label = V(network_graph)$name)

edges_df <- as_data_frame(network_graph, what = "edges")

nodes_df$group <- as.factor(membership(communities))

visNetwork(nodes_df, edges_df) %>%
  visNodes(title = "title", label = "label", group = "group") %>%
  visEdges(arrows = 'to') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 123)

visNetwork(nodes_df, edges_df) %>%
  visEdges(arrows = 'to') %>%
  visPhysics(enabled = FALSE)

degree_centrality <- degree(network_graph, mode="all")  # Total connections
in_degree <- degree(network_graph, mode="in")  # Incoming connections
out_degree <- degree(network_graph, mode="out")  # Outgoing connections
betweenness_centrality <- betweenness(network_graph, directed=TRUE)
closeness_centrality <- closeness(network_graph, mode="all")

metrics <- list(
  DegreeCentrality = list(In = in_degree, Out = out_degree),
  BetweennessCentrality = betweenness_centrality,
  ClosenessCentrality = closeness_centrality
)
print(metrics)

# network density
network_density <- edge_density(network_graph)
print(network_density)

#average_path_length
average_path_length <- average.path.length(network_graph, directed = TRUE, unconnected = TRUE)
print(average_path_length)

#Thank you for reviewing my work and your feedback is highly appreciated
