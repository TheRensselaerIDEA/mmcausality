
# Conditional install of packages
if (!require("shiny", character.only = T)) {
  install.packages("shiny")
  require("shiny", character.only = T)
}

if (!require("igraph", character.only = T)) {
  install.packages("igraph")
  require("igraph", character.only = T)
}

if (!require("visNetwork", character.only = T)) {
  install.packages("visNetwork")
  require("visNetwork", character.only = T)
}

if (!require("tidyverse", character.only = T)) {
  install.packages("tidyverse")
  require("tidyverse", character.only = T)
}


# Shiny UI: Very simple!
ui <- fluidPage(
  visNetworkOutput("network", height = "1080px")
)


# Shiny server
server <- function(input, output) {
  output$network <- renderVisNetwork({
    #### Read in the data from CSV
    SAM_graph_data_all_states_2015_2017 <- read_csv("results_SAM_graph_data_all_states_2015_2017.csv")

    # Tweak the column names  
    colnames(SAM_graph_data_all_states_2015_2017)[colnames(SAM_graph_data_all_states_2015_2017) == 'p value score'] <- 'p.value.score'
    colnames(SAM_graph_data_all_states_2015_2017)[colnames(SAM_graph_data_all_states_2015_2017) == 'Score SAM'] <- 'Score.SAM'
    colnames(SAM_graph_data_all_states_2015_2017)[colnames(SAM_graph_data_all_states_2015_2017) == 'Type edge'] <- 'Type.edge'
    
    # A little tidyverse filtering
    SAM_graph_data_filtered_2015_2017 <- SAM_graph_data_all_states_2015_2017 %>% 
      filter(Score.SAM >= 0.5) %>%
      filter(p.value.score <= 0.05 )
    
    # Construct network w/ igraph
    SAM_net <- graph_from_data_frame(SAM_graph_data_filtered_2015_2017,directed = F) %>%
      set_edge_attr("weight", value= SAM_graph_data_filtered_2015_2017$Score.SAM)

    # Some community algorithms require "simplified" network    
    SAM_net_simple <- igraph::simplify(SAM_net)
    
    # Different layout schemes; execute ?layout_name for more info
    # coords = layout_on_sphere(SAM_net_simple)
    coords = layout_with_fr(SAM_net_simple)
    # coords = layout_in_circle(SAM_net_simple)
    #coords = layout_with_sugiyama
    
    # fast greedy clustering
    c1 <- cluster_fast_greedy(SAM_net_simple)
    SAM_net_clusters <- data.frame(cbind(names(SAM_net_simple[1]), c1$membership))
    
    # "fast greedy "Walktrap" clustering (creates more)
    # c2 <- cluster_walktrap(SAM_net_simple)
    # SAM_net_clusters <- data.frame(cbind(names(SAM_net_simple[1]), c2$membership))
    
    colnames(SAM_net_clusters) <- c("nodes","group")
    
    # build a visNetwork version of the plots
    
    nodes <- NULL
    nodes_s <- unique(SAM_graph_data_filtered_2015_2017$Source)
    nodes_t <- unique(SAM_graph_data_filtered_2015_2017$Target)
    nodes <- unique(c(nodes_s,nodes_t))
    font.size <- rep(40, times=length(nodes))
    
    nodes <- data.frame(cbind(nodes,nodes,font.size))
    colnames(nodes) <- c("id","label","font.size")
    
    nodes <- merge(nodes, SAM_net_clusters, by.x="id", by.y="nodes")
    
    edges <- data.frame(from = SAM_graph_data_filtered_2015_2017$Source, 
                        to = SAM_graph_data_filtered_2015_2017$Target,
                        #                   label=SAM_graph_data_filtered_2015_2017$`Type edge`,
                        value=SAM_graph_data_filtered_2015_2017$Score.SAM,
                        arrows=rep("to",times=nrow(SAM_graph_data_filtered_2015_2017))
    )
    edges$color[SAM_graph_data_filtered_2015_2017$`Type.edge` == "undirected protective (negative impact)"] <- "green"
    edges$color[SAM_graph_data_filtered_2015_2017$`Type.edge` == "directed protective (negative impact)"] <- "blue"
    
    edges$color[SAM_graph_data_filtered_2015_2017$`Type.edge` == "undirected destructive (positive impact)"] <- "red"
    edges$color[SAM_graph_data_filtered_2015_2017$`Type.edge` == "directed destructive (positive impact)"] <- "orange"
    
    #A nicer force-direct graph layout with node selector
    
    # Finally, the plot
    visNetwork(nodes, edges) %>%
      visEdges(arrows = 'to', smooth = T) %>%
      visIgraphLayout("layout_with_kk") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T, degree=1, hideColor="#bababa0f", labelOnly=T), 
                 nodesIdSelection = T
      ) %>%
      visLegend()
    
    })
}


shinyApp(ui = ui, server = server)
