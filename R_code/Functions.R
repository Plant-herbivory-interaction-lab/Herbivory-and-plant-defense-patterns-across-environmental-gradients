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


## Data prep function ----
Data_prep<-function(loc="Field",PopLevel=F,long=F,ClimateLong=F,
                    Treatment=F,Time.var='Mid',start_date="2023-06-15",
                    end_date="2023-07-29",byDate=F,group=NULL,...){
  
  Combined_data1<- Trait_data%>% 
    left_join(PCs)
  
  if(loc=="Field"|loc=="Garden"){Combined_data1<-Combined_data1 %>% 
    dplyr::filter(Loc==loc)}
  
  
  if(loc=="Garden"){
    
    if(byDate==T){
      Combined_data1<-Combined_data1 %>%
        filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))}
    else{
      Combined_data1<-Combined_data1 %>%
        filter(str_detect(Time,Time.var))
    }
    
    if (!is.null(group)) {
      Combined_data1 <- group_by(Combined_data1, !!!syms(group)) %>%
        summarise(
          Pop = unique(Pop),
          Loc = if (PopLevel) unique(Loc) else first(Loc),
          Time = if (PopLevel) unique(Time) else first(Time),
          Date = sample(c(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE)), size = 1),
          count = n(),
          flowers = sum(fl_m > 0, fl_h > 0, na.rm = TRUE),
          quant_herb_0.75 = as.vector(quantile(herb_p, na.rm = TRUE)[3]),
          max_herb = max(herb_p, na.rm = TRUE),
          across(where(is.numeric), ~ mean(., na.rm = TRUE))
        ) %>%
        ungroup()}
  }
  
  
  if(PopLevel==T&Treatment==T){Combined_data1<-Combined_data1 %>% 
    group_by(Pop,Loc,Treatment,Time)}
  
  if(PopLevel==T&Treatment==F){Combined_data1<-Combined_data1 %>% 
    group_by(Pop,Loc,Year)}
  
  
  if(PopLevel==T){Combined_data1<-Combined_data1 %>% 
    summarise(across(where(is.numeric), \ (x) mean(x, na.rm = TRUE)),
              Plant_N=length(Pop),
              Conc_t=log(Conc),
              SLA_t=log(SLA),
              Trichomes_t=log(Trichomes),
              herb_p_t=logit(herb_p)) %>% 
    ungroup()}
  
  
  if(long==T&ClimateLong==T){Combined_data1<-Combined_data1 %>%
    left_join(Pop_info %>% select(Pop,Latitude))%>% 
    pivot_longer(cols = c(Trichomes_t_sc,SLA_t_sc,Conc_t_sc,Clim_PC1_sq_sc,Clim_ave_PC1_sc)) 
  }
  
  if(long==T&ClimateLong==F){Combined_data1<-Combined_data1 %>%
    left_join(Pop_info %>% select(Pop,Latitude))%>% 
    pivot_longer(cols = c(Trichomes_t_sc,SLA_t_sc,Conc_t_sc))
  }
  
  if(long==T){Combined_data1<-Combined_data1 %>%
    mutate(name=case_when(
      name=="Conc_t_sc"~"Glycoalkaloids (mg/g)",
      name=='Clim_ave_PC1_sc'~'Climate',
      name=='Clim__PC1_sq_sc'~'Climate (sq)',
      name=="Trichomes_t_sc"~"Trichomes",
      name=='SLA_t_sc'~'SLA',
      .default = name))}
  
  
  Combined_data1<-Combined_data1 %>% ungroup() %>% 
    mutate(
      Conc_t=log(Conc),
      SLA_t=log(SLA),
      Trichomes_t=log(Trichomes),
      herb_p_t=logit(herb_p),
      across(any_of(c("herb_p_t","Trichomes_t","NPP_g","NPP_g_10y","SLA_t","Conc_t")), 
             ~as.numeric(scale(.,center = T)), .names = "{.col}_sc"),
      Plant=c(1:length(Conc))
    )  %>% filter(SLA_t_sc>-3&SLA_t_sc<3)
  
  Combined_data1
}

## Graph theme setup----
C_theme<-function(size=18){theme_bw(base_size = size)+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())}


## SEM function ----
SEM_results <- function(Loc = "Field", lat="",Prod = " + NPP_g", lat_prod="Latitude_sc",
                        lat_main="", lat_traits="",
                        mod_fun='glmmTMB',
                        model = c("lm1", "lm2", "lm3", "lm4"), 
                        random = "+ (1|Year/Pop)", year_ran = "",
                        Time="mid", group="Plant_ID",
                        byDate=T,start_date="2023-06-15",end_date="2023-07-29",ICC=T,corError = list(
                          quote(SLA_t_sc %~~% Trichomes_t_sc),
                          quote(SLA_t_sc %~~% Conc_t_sc))) {
  
  
  method<-get(mod_fun)
  
  formula_strings <- c(
    lm1 = paste0('herb_p_t_sc ~',lat_main,  Prod,' + Trichomes_t_sc + Conc_t_sc + SLA_t_sc', random, year_ran), 
    lm2 = paste0('Conc_t_sc ~ ',lat_traits,  Prod, random, year_ran),           
    lm3 = paste0('SLA_t_sc ~ ',lat_traits, Prod, random, year_ran),
    lm4 = paste0('Trichomes_t_sc ~ ',lat_traits, Prod, random),  
    lm5 = paste0('NPP_g_10y_sc ~', lat_prod, '+ (1|dummy)'),
    lm6 = paste0('NPP_g_sc ~', lat_prod, '+ (1|dummy)')
  )
  
  DF_short_I_field <- Data_prep(loc=Loc,Time.var=Time,byDate = byDate,
                                start_date = start_date, 
                                end_date = end_date,group=group) %>% 
    select(c(Date,unique(unlist(lapply(formula_strings, function(fstr) {
      all.vars(as.formula(fstr))
    }))))) %>% drop_na()
  
  
  
  print(min(DF_short_I_field$Date))
  print(max(DF_short_I_field$Date))
  
  
  model_list <- lapply(formula_strings, function(fstr) method(as.formula(fstr), DF_short_I_field))
  
  AICs<-sapply(model_list,FUN=AIC)
  
  print(AICs)
  
  if(ICC==T){cus_unlist<-function(x){as.vector(icc(x))}
  ICCs<-do.call(cbind,sapply(model_list,FUN=cus_unlist,simplify = F))
  
  print(ICCs)}
  
  for (m in model) {
    model_list[[m]]$call[[1]] <- as.name(mod_fun)
  }
  
  fit <- do.call(psem, c(
    model_list[model],
    corError,
    list(data = DF_short_I_field)
  ))
  
  return(fit)
}

## Mini multi-panel plot ----
make_plots <- function(data, x_var, y_vars, ncol = 2, extra_plots = NULL) {
  nplots <- length(y_vars)
  
  # Identify bottom-most plot in each column
  bottom_plots <- sapply(1:ncol, function(col) {
    idx <- seq(col, nplots, by = ncol)
    max(idx)
  })
  
  # Create main plots
  plots <- lapply(seq_along(y_vars), function(i) {
    y <- y_vars[i]
    y_lab <- if (nchar(y) > 8) str_wrap(y, width = 8) else y
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y]])) +
      geom_point() +
      C_theme(size = 15) +
      labs(y = y_lab)
    
    # Show x-axis only for bottom-most plot in each column
    if (i %in% bottom_plots) {
      p <- p + labs(x = x_var)
    } else {
      p <- p + theme(
        #axis.text.x  = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
      )
    }
    
    p
  })
  
  # Insert extra plots at specific positions if provided
  if (!is.null(extra_plots)) {
    # extra_plots should be a named list: names are indices where to insert
    positions <- as.integer(names(extra_plots))
    for (i in seq_along(extra_plots)) {
      pos <- positions[i]
      p <- extra_plots[[i]]
      
      # Wrap y-label and apply theme
      y_lab <- p$labels$y
      if (is.null(y_lab)) y_lab <- ""
      wrapped_y <- if (nchar(y_lab) > 12) str_wrap(y_lab, width = 12) else y_lab
      p <- p + labs(y = wrapped_y) + C_theme(size = 15)
      
      # Insert the plot at the desired position
      if (pos > length(plots)) {
        plots[[pos]] <- p
      } else {
        plots <- append(plots, list(p), after = pos - 1)
      }
    }
  }
  
  wrap_plots(plots, ncol = ncol)
}

## Plots with trend linds ----
Custom_ggplot<-function(loc="Field",response='Trichomes',predictor='Clim_ave_PC1',deg=2,random="+(1|Pop:Time)",family="poisson",Trend=T){
  Data<-Data_prep(loc=loc,byDate = T,group = "Plant_ID") %>% 
    mutate(Pop=as.factor(Pop))
  
  Data_pop<-Data_prep(loc=loc,PopLevel = T,byDate = T) 
  
  max_l<-max(Data[,predictor],na.rm=T)
  
  min_l<-min(Data[,predictor],na.rm=T)
  
  values<-seq(from=min_l,to=max_l,length.out=100)
  
  m<-glmmTMB(as.formula(paste0(response,'~poly(',predictor,',',deg,')', random)),Data,family = family)
  
  
  predicted<-as.data.frame(predict_response(m,
                                            terms=c(paste0(predictor,'[',paste(values, collapse = ", "),']')), margin="empirical",
  ))
  
  
  p<-ggplot(data=predicted,aes(x=x,y=predicted))
    
  if(Trend==T){p<-p+geom_ribbon(aes(x=x,y=predicted,ymin=conf.low,ymax = conf.high), fill = "grey70",alpha=0.5) + 
    geom_line(linewidth=1)}
  
    p<-p+geom_point(data=Data,aes(x=!!sym(predictor),y=!!sym(response)),alpha=0.3,shape = 16)+
    geom_point(data=Data_pop,aes(x=!!sym(predictor),y=!!sym(response)),col="darkred",size=3)
  
    return(p)
}

## SEM graphing function ----
semGraph<-function(fit=fit) {
  library(qgraph)
  library(rsvg)
  library(svglite)
  library(grid)
  edges<-summary(fit, fit=T,rsquare=T,conserve=T,standardize="scale")$coefficients[,c("Response","Predictor","Std.Estimate","P.Value")] %>% 
    filter(!grepl("~",Response)&!grepl("Loc",Predictor)) %>% 
    mutate(
      across(where(is.character), ~ sub("_sc_sq", " (sq)", .)),
      across(where(is.character), ~ sub("_.*", "", .)),
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
  weights <- edges$Std.Estimate  # Path coefficients as weights
  p_values <- edges$P.Value  # P-values
  
  # Change the response and predictor labels
  
  # Define edge colors: Blue for positive, Red for negative
  edge_colors <- ifelse(weights > 0, "black", "darkred")
  
  # Define line types: Solid for significant (p < 0.05), Dashed for non-significant
  edge_lty <- ifelse(p_values < 0.1, 1, 2)  # 1 = solid, 2 = dashed
  
  # Define edge widths based on effect size
  edge_widths <- ifelse(p_values < 0.1,abs(weights)*20,1)
  
  # Define estimates as edge labels (rounded to 2 decimals)
  edge_labels <- ifelse(p_values < 0.1,round(weights, 3),NA)  
  
  # Define node order (top to bottom)
  node_names <- c("Herbivory", "Glycoalkaloids", "SLA", "Trichomes", as.vector(edge_list[10,1]), as.vector(edge_list[11,1]))
  
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
  
  climate1_to_herbivory <- which(edge_list[,1] %in% as.vector(edge_list[10,1]) & edge_list[,2] == "Herbivory")
  climate_to_herbivory <- which(edge_list[,1] %in% as.vector(edge_list[11,1]) & edge_list[,2] == "Herbivory")
  
  
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
         shape="rectangle",vsize2=8,vsize=27,label.scale=F,border.width=2.1)
  
  dev.off()
  
  wrap_elements(rasterGrob(rsvg(temp_file), interpolate = TRUE))
  
}

## Custom biplot function ----
require(ggrepel)
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


# Function to extract data from Google earth engine ----


## see https://github.com/r-spatial/rgee/tree/master for rgee usage and installation
## you will need to run the commented commands below when first setting up rgee
#install.packages("rgee")
#install.packages("reticulate")
# you might also have to install git https://git-scm.com/downloads
#reticulate::install_python() # make sure python is installed
#reticulate::virtualenv_create("rgee")
#reticulate::py_install("numpy","rgee")
#reticulate::py_install("earthengine-api","rgee")
#reticulate::use_virtualenv("rgee")
#library(rgee)
#ee_install_upgrade()
#ee_Authenticate()

reticulate::use_virtualenv("rgee")
library(rgee)
ee_Initialize()




## Extract multiple bands and dates from google earth engine----
Extract_var_with_const_date <- function(loc,path,subpath,select,
                                        begin = 5,
                                        end=8,
                                        unit="month",
                                        buffer=1500,
                                        scale=500) {
  batch_size=round(sqrt(6000/(3.14*(buffer/scale)^2)))
  
  if(select!=T){
    terra<-ee$Image(as.character(path))
  } else if(select==T){
    terra<-ee$ImageCollection(as.character(path))$select(as.character(subpath))
  }
  
  if(nchar(unit)>1) {
    terra<-terra$filter(
      ee$Filter$calendarRange(
        begin,
        end,
        unit
      )
    )
  }
  
  
  n_images <- terra$size()$getInfo()
  
  num_iterations <- ceiling(nrow(loc) / batch_size)
  
  results<-list()
  # Loop through each batch
  for (i in 1:num_iterations) {
    # Calculate the start and end rows for the current batch
    start_row <- (i - 1) * batch_size + 1
    end_row <- min(i * batch_size, nrow(loc))
    print(paste("Processing locations", start_row,"-",end_row, "of", nrow(loc)))
    # Extract current batch
    batch <- sf_as_ee(loc[start_row:end_row, ])$map(function(pt) {                         # pt is an ee.Feature
      pt$buffer(buffer)                  
    }) 
    
    im_iter <- ceiling(n_images / batch_size)
    

    
    for (j in 1:im_iter) {
      start_im <- (j - 1) * batch_size + 1
      end_im <- min(j * batch_size, n_images)
      
      print(paste("Processing images", start_im,"-",end_im, "of", n_images))
      
      # Get the i-th image (0-indexed in EE)
      img <- ee$ImageCollection$fromImages(terra$toList(n_images)$slice(start_im,end_im))
     
    # Perform ee_extract for the current batch
    row <-img$map(
      ee_utils_pyfunc(function(img) {
        img_date <- img$date()$format("YYYY-MM-dd")
        
        sampled <- img$sampleRegions(
          collection = batch,
          scale = scale,
          geometries = FALSE
        )
        sampled$map(
          ee_utils_pyfunc(function(feature) {
            feature$set("image_date", img_date)
          })
        )
      })
    )$flatten() %>% ee_as_sf() %>% sf::st_drop_geometry() %>% 
      group_by(Pop,image_date) %>%
      mutate(pixel_num = row_number()) %>%
      ungroup()

    results[[paste(i,j)]]<-row
    }
   
    }
  results<-do.call(rbind, results)
  results
}

