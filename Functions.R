## Code by: Jacob Herschberger
## Date: September 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Functions ----
## Compress data function ----
transform_perc <- function(percentage_vec) {
  # See Cribari-Neto & Zeileis (2010)
  (percentage_vec * (length(percentage_vec) - 1) + 0.5)/length(percentage_vec)
}

## Graph theme setup----
C_theme<-function(size=18){theme_bw(base_size = size)+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())}


## SEM graphing function ----
semGraph<-function(fit=fit,node_locs=list(),
                   edge_locs = list(),
                   curve_locs = list(),
                   H = F, marg_y = 12) {
  
  edges<-summary(fit,rsquare=T,conserve=T)$coefficients[,c("Response","Predictor","Estimate","P.Value","Std.Error")] %>% 
    filter(!grepl("~",Response)&!grepl("Loc",Predictor)) %>% 
    mutate(
      across(where(is.character), ~ sub("_s2", " (sq)", .)),
      across(where(is.character), ~ sub("Clim", "Climate ", .)),
      across(where(is.character), ~ sub("_s", "", .)),
      across(where(is.character), ~ sub("_", " ", .)),
      Response=case_when(grepl("herb", Response) ~ 'Herbivory',
                         grepl("Conc", Response) ~ 'Glycoalkaloids',
                         grepl("SLA", Response) ~ 'SLA',
                         grepl("Trichomes", Response) ~ 'Trichomes',
                         .default = Response),
      Predictor = case_when(
        grepl("Conc", Predictor) ~ 'Glycoalkaloids',
        grepl("SLA", Predictor) ~ 'SLA',
        grepl("Trichomes", Predictor) ~ 'Trichomes',
        .default = Predictor))
  
  # Convert to edgelist format (include all paths)
  edge_list <- as.matrix(edges[, c("Predictor", "Response")])
  weights <- edges$Estimate  # Path coefficients as weights
  p_values <- round(edges$P.Value,2)  # P-values
  
  # Change the response and predictor labels
  
  # Define edge colors: Blue for positive, Red for negative
  
  if (H) {edge_colors <- "black"
  } else {edge_colors <- ifelse(weights > 0, "black", "darkred")}
  
  # Define line types: Solid for significant (p < 0.05), Dashed for non-significant
  if (H) {edge_lty <- 1
  } else { edge_lty <- ifelse(p_values < 0.1, 1, 2)}  # 1 = solid, 2 = dashed
  
  
  # Define edge widths based on effect size
  if (H) {edge_widths <- 2
  } else { edge_widths <- ifelse(p_values < 0.1,abs(weights)*20,2)}
  
  
  # Define estimates as edge labels (rounded to 2 decimals)
  if (H) {edge_labels <- NULL # only significant ones
  } else {  edge_labels <- mapply(function(w, se, p) {
    if (p < 0.01) {
      bquote(
        atop(
          .(round(w, 2)) %+-% .(round(as.numeric(se), 2)),
          italic(p) < 0.01
        )
      )
    } else {
      bquote(
        atop(
          .(round(w, 2)) %+-% .(round(as.numeric(se), 2)),
          italic(p) == .(round(p, 2))
        )
      )
    }
  }, weights, edges$Std.Error, p_values, SIMPLIFY = FALSE)} 
  
  
  
  
  # Define node order (top to bottom)
  node_names <- c(unique(c(edge_list[,1],edge_list[,2])))
  
  # Define edge label locations
  pdf(NULL)
  graph<-qgraph(edge_list)
  dev.off()
  
  edge_label_locations<-graph$graphAttributes$Edges$edge.label.position
  
  get_path_num<-function(pred="NPP",resp="Herbivory"){
    which(edge_list[,1] %in% as.vector(pred) & edge_list[,2] == resp)
  }
  
  edge_loc = c(list(c("Latitude","Glycoalkaloids",0.3),
                    c("Glycoalkaloids","Herbivory",0.3),
                    c("NPP","Herbivory",0.6),
                    c("NPP","Trichomes",0.3),
                    c("SLA","Herbivory",0.6),
                    c("Trichomes","Herbivory",0.3)),edge_locs)
  
  
  
  for(i in seq_along(edge_loc)) {
    
    num<-get_path_num(edge_loc[[i]][1],edge_loc[[i]][2])
    
    if(length(num) != 0){
      edge_label_locations[num]<-as.numeric(edge_loc[[i]][3])
    }
  }
  
  #Define custom layout positions with increased spacing
  layout_matrix <- graph$layout
  
  rownames(layout_matrix)<-node_names
  
  node_loc=c(list('Herbivory'=c(0,1),
                  "Glycoalkaloids"=c(-0.7,0),
                  "SLA"=c(0,0),
                  "Trichomes"=c(0.7,0),
                  "NPP"=c(-0.7,-1),
                  "Latitude"=c(0.7,-1)),node_locs)
  
  for(i in seq_along(node_loc)) {
    node_name<-names(node_loc[i])
    if (node_name %in% rownames(layout_matrix)) {
      layout_matrix[node_name, ] <- node_loc[[i]]
    }
    
  }
  
  
  # Assign default straight edges
  curves <- graph$graphAttributes$Edges$curve  
  
  # Identify only the climate → herbivory paths and set outward curves
  curve_loc = c(list(c("Latitude","Herbivory",-5),
                     c("NPP","Herbivory",5)),curve_locs)
  
  
  for(i in seq_along(curve_loc)) {
    num<-get_path_num(curve_loc[[i]][1],curve_loc[[i]][2])
    
    if(length(num) != 0){
      curves[num]<-as.numeric(curve_loc[[i]][3])
    }
  }
  
  
  # Enlarged labels with background
  
  temp_file<-tempfile(fileext = ".svg")
  
  svglite(temp_file, width = 20, height = 20)
  
  # Plot with qgraph
  qgraph(edge_list, labels = node_names, directed = TRUE,
         edge.labels = edge_labels, edge.color = edge_colors, edge.width = edge_widths,
         edge.label.cex = 1.2,
         lty = edge_lty, curve = curves, 
         edge.label.position = edge_label_locations, layout = layout_matrix,
         edge.label.bg = "white",curveShape = -1.5,
         mar = c(3,marg_y,3,marg_y),
         label.cex = 4, 
         shape="ellipse",node.width=3.2,label.scale=F,border.width=1)
  
  dev.off()
  
  wrap_elements(rasterGrob(rsvg(temp_file), interpolate = TRUE))
  
}

## Custom biplot function ----
library(ggrepel)
PCbiplot <- function(data1=data,rot_x=1,rot_y=1,font_size=14,ext=4) {
  PC<-data1  %>% 
    drop_na() %>% 
    prcomp(.,scale. = T,center = T)
  tran<-c(rot_x,rot_y)
  data <- data.frame(sweep(PC$x[,1:2],2,tran,FUN = '*'))
  rot<-sweep(PC$rotation[,1:2],2,tran,FUN = '*')
  dev<-PC$sdev[1:2]
  
  datapc <- data.frame(varnames=rownames(rot), sweep(rot,2,dev,FUN = '*'))
  angle <- with(datapc[,2:3], (180/pi) * atan(PC2/PC1))
  hjust = with(datapc[,2:3], (1 - 1.5* sign(PC1)) / 6)
  Lab<-paste(c("PC1","PC2"), 
             sprintf('(%0.1f%%)', 
                     100 * PC$sdev[1:2]^2/sum(PC$sdev^2)))
  
  plot <- ggplot(data, aes(x=PC1, y=PC2))+geom_point() + 
    geom_segment(data=datapc, aes(x=0, y=0, xend=PC1*ext, yend=PC2*ext), 
                 arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="black")+
    geom_label_repel(data=datapc, aes(x=PC1*ext, y=PC2*ext, label=varnames), 
                     color="black",angle = angle, hjust = hjust,size=font_size,
                     force = 0.4, label.padding = unit(0.2, "lines")) + 
    labs(x=Lab[1],y=Lab[2])
  
  plot
}

# Weighted climate distance ----

weighted_cd <- function(data, origin_pc, pc_cols, weights) {
  
  # Ensure names match
  stopifnot(all(pc_cols %in% names(data)))
  stopifnot(all(pc_cols %in% names(origin_pc)))
  stopifnot(all(pc_cols %in% names(weights)))
  
  # Compute weighted Euclidean distance row-wise
  d <- sqrt(
    rowSums(
      sapply(pc_cols, function(pc) {
        weights[pc] * (data[[pc]] - origin_pc[[pc]])^2
      })
    )
  )
  
  return(d)
}

