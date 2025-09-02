

semGraph<-function(fit=fit) {
  library(qgraph)
  library(rsvg)
  library(svglite)
  library(grid)
edges<-summary(fit, fit=T,rsquare=T,conserve=T,standardize="scale")$coefficients[,c("Response","Predictor","Std.Estimate","P.Value")] %>% 
  filter(!grepl("~",Response)&!grepl("Loc",Predictor)) %>% 
  mutate(Response=case_when(grepl("herb", Response) ~ 'Herbivory',
                            grepl("Conc", Response) ~ 'Glycoalkaloids',
                            grepl("SLA", Response) ~ 'SLA',
                            grepl("Trichomes", Response) ~ 'Trichomes',
                            .default = Response),
         Predictor = case_when(Predictor == 'Latitude_sc'~ 'Latitude',
                               Predictor == 'Latitude_sc_sq'~ 'Latitude (sq)',
                               grepl("Conc", Predictor) ~ 'Glycoalkaloids',
                               grepl("SLA", Predictor) ~ 'SLA',
                               grepl("Trichomes", Predictor) ~ 'Trichomes',
                               .default = Predictor))

# Convert to edgelist format (include all paths)
edge_list <- as.matrix(edges[, c("Predictor", "Response")])
weights <- edges$Std.Estimate  # Path coefficients as weights
p_values <- edges$P.Value  # P-values

# Change the response and predictor labels

# Define edge colors: Blue for positive, Red for negative
edge_colors <- ifelse(weights > 0, "black", "darkred")

# Define line types: Solid for significant (p < 0.05), Dashed for non-significant
edge_lty <- ifelse(p_values < 0.05, 1, 2)  # 1 = solid, 2 = dashed

# Define edge widths based on effect size
edge_widths <- ifelse(p_values < 0.05,abs(weights)*20,1)

# Define estimates as edge labels (rounded to 2 decimals)
edge_labels <- ifelse(p_values < 0.1,round(weights, 3),NA)  

# Define node order (top to bottom)
node_names <- c("Herbivory", "Glycoalkaloids", "SLA", "Trichomes", "Latitude", "Latitude (sq)")

# Define edge label locations
edge_label_locations<-c(0.5,0.5,0.5,0.5,0.5,0.5,0.7,0.4,0.4,0.7,0.5)


# Define custom layout positions with increased spacing
layout_matrix <- matrix(c(
  0,  1,  # herb_p_t (higher top)
  -0.7,  0,  # Conc (middle left)
  0,  0,  # SLA (middle center)
  0.7,  0,  # Trichomes (middle right)
  -0.5, -1,  # Clim_ave_PC1 (bottom left)
  0.5, -1   # Clim_PC1_sq (bottom right)
), byrow = TRUE, ncol = 2)

# Assign default straight edges
curves <- rep(0, nrow(edge_list))  

# Identify only the climate → herbivory paths and set outward curves

climate1_to_herbivory <- which(edge_list[,1] %in% c("Latitude") & edge_list[,2] == "Herbivory")
climate_to_herbivory <- which(edge_list[,1] %in% c("Latitude (sq)") & edge_list[,2] == "Herbivory")


curves[climate1_to_herbivory] <- 5.6
curves[climate_to_herbivory] <- -5.6


 # Enlarged labels with background

temp_file<-tempfile(fileext = ".svg")

svglite(temp_file, width = 60, height = 60)

# Plot with qgraph
  qgraph(edge_list, layout = layout_matrix, labels = node_names, directed = TRUE,
                       edge.labels = edge_labels, edge.color = edge_colors, edge.width = edge_widths,
                       edge.label.cex = 1.8,
                       lty = edge_lty, curve = curves, edge.label.position = edge_label_locations,
                       edge.label.bg = "white",curveShape = -1.5,
                       mar = c(3,7,3,7),
                       #filetype="jpg",
                       #filename = filename,
                       #height = 9, width = 9,
         label.cex = 14, 
                       shape="rectangle",vsize2=8,vsize=25,label.scale=F,border.width=2.1)

dev.off()

wrap_elements(rasterGrob(rsvg(temp_file), interpolate = TRUE))

}
