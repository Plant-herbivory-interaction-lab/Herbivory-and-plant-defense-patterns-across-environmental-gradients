library(qgraph)

semGraph<-function(fit=fit) {
edges<-summary(fit, fit=T,rsquare=T,conserve=T,standardize="none")$coefficients[,c("Response","Predictor","Estimate","P.Value")] %>% 
  filter(!grepl("~",Response)&!grepl("Loc",Predictor)) %>% 
  mutate(Response=case_when(grepl("herb", Response) ~ 'Herbivory',
                            grepl("Conc", Response) ~ 'Glycoalkaloids',
                            grepl("SLA", Response) ~ 'SLA',
                            grepl("Trichomes", Response) ~ 'Trichomes',
                            .default = Response),
         Predictor = case_when(Predictor == 'Clim_ave_PC1_sc'~ 'Climate',
                               Predictor == 'Clim_PC1_sq_sc'~ 'Climate (sq)',
                               grepl("Conc", Predictor) ~ 'Glycoalkaloids',
                               grepl("SLA", Predictor) ~ 'SLA',
                               grepl("Trichomes", Predictor) ~ 'Trichomes',
                               .default = Predictor))

# Convert to edgelist format (include all paths)
edge_list <- as.matrix(edges[, c("Predictor", "Response")])
weights <- edges$Estimate  # Path coefficients as weights
p_values <- edges$P.Value  # P-values

# Change the response and predictor labels

# Define edge colors: Blue for positive, Red for negative
edge_colors <- ifelse(weights > 0, "black", "darkred")

# Define line types: Solid for significant (p < 0.05), Dashed for non-significant
edge_lty <- ifelse(p_values < 0.05, 1, 2)  # 1 = solid, 2 = dashed

# Define edge widths based on effect size
edge_widths <- abs(weights) * 10

# Define estimates as edge labels (rounded to 2 decimals)
edge_labels <- round(weights, 3)  

# Define node order (top to bottom)
node_names <- c("Herbivory", "Glycoalkaloids", "SLA", "Trichomes", "Climate", "Climate (sq)")

# Define custom layout positions with increased spacing
layout_matrix <- matrix(c(
  0,  1,  # herb_p_t (higher top)
  -1,  0,  # Conc (middle left)
  0,  0,  # SLA (middle center)
  1,  0,  # Trichomes (middle right)
  -0.5, -1,  # Clim_ave_PC1 (bottom left)
  0.5, -1   # Clim_PC1_sq (bottom right)
), byrow = TRUE, ncol = 2)

# Assign default straight edges
curves <- rep(0, nrow(edge_list))  

# Identify only the climate → herbivory paths and set outward curves

climate1_to_herbivory <- which(edge_list[,1] %in% c("Climate") & edge_list[,2] == "Herbivory")
climate_to_herbivory <- which(edge_list[,1] %in% c("Climate (sq)") & edge_list[,2] == "Herbivory")


curves[climate1_to_herbivory] <- 5.6
curves[climate_to_herbivory] <- -5.6


# Plot with qgraph
graph<-qgraph(edge_list, layout = layout_matrix, labels = node_names, directed = TRUE,
       edge.labels = edge_labels, edge.color = edge_colors, edge.width = edge_widths,
       lty = edge_lty, curve = curves, edge.label.position = 0.7,
       edge.label.bg = "white",curveShape = -1.5,
       #filetype="jpg",
       #filename = filename,
       #height = 9, width = 9,
       shape="rectangle",vsize2=5,vsize=15,label.scale=F,border.width=2.1)  # Enlarged labels with backgroun

#as.igraph(graph)
}


semGraph_ggraph <- function(fit=Non_interaction_field) {
  library(ggraph)
  library(igraph)

  
  edges <- summary(fit, fit = TRUE, rsquare = TRUE, conserve = TRUE)$coefficients[, c("Predictor", "Response", "Std.Estimate", "P.Value")] %>% 
    filter(!grepl("~", Response) & !grepl("Loc", Predictor)) %>%
    mutate(
      Response = case_when(
        grepl("herb", Response) ~ 'Herbivory',
        grepl("Conc", Response) ~ 'Glycoalkaloids',
        grepl("SLA", Response) ~ 'SLA',
        grepl("Trichomes", Response) ~ 'Trichomes',
        .default = Response
      ),
      Predictor = case_when(
        Predictor == 'Clim_ave_PC1_sc' ~ 'Climate',
        Predictor == 'Clim_PC1_sq_sc' ~ 'Climate (sq)',
        grepl("Conc", Predictor) ~ 'Glycoalkaloids',
        grepl("SLA", Predictor) ~ 'SLA',
        grepl("Trichomes", Predictor) ~ 'Trichomes',
        .default = Predictor
      )
    )
  
graph<-graph_from_data_frame(edges, directed = TRUE)


V(graph)$name<- c("Herbivory", "SLA", "Trichomes", "Climate", "Climate (sq)", "Glycoalkaloids")

node_positions <- data.frame(
  x = c(-0.5, 0.5, 1, 0, -1, 0),   # Place Glycoalkaloids below or wherever fits
  y = c(-1, -1, 0, 0, 0, 1)
)


ggraph(graph,layout = "manual",x=node_positions[,1],y=node_positions[,2]
       ) + 
  geom_edge_link(aes(
    edge_alpha = abs(Std.Estimate),
    edge_width = abs(Std.Estimate),
    edge_linetype = P.Value < 0.05,
    edge_color = Std.Estimate > 0
  ),
  show.legend = FALSE,
  arrow = arrow(length = unit(3, "mm"))
  ) +  # Draw edges with alpha based on weight
  ggraph::geom_edge_label(aes(label = round(Std.Estimate, 2)), 
                  fontface = "bold", color = "black", label.size = NA, fill = "white") +
  geom_node_label(aes(label = name), size = 8, label.padding = unit(0.2, "lines")) +
  scale_edge_color_manual(values = c("darkred", "black")) +
  scale_edge_linetype_manual(values = c("dashed", "solid")) +
  theme_void()
}




