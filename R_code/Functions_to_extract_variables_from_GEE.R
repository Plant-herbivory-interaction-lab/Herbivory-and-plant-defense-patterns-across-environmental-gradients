 
# Function to extract ESA data from Google earth engine ----
# This samples 1000 spots within one kilometer and then finds the proportion of landtypes.
Land_cover_porp_1KM<-function(path,pp1) {
  terraclimate<-ee$ImageCollection(path) 
  
  buffer<-sf::st_buffer(
    pp1
    ,1000)
  
  row2<-list()
  
  for(i in 1:nrow(buffer)){
    row<-table(ee_extract(
      x = terraclimate,
      y = st_sample(buffer[i,],1000),
      scale=1,
      fun = ee$Reducer$mean(),
      sf = FALSE
    )[,2])/1000
    row1<-data.frame(t(as.vector(row)))
    colnames(row1)<-names(row)
    row2[[i]]<-row1
    
    done<-round(i/nrow(buffer)*100)
    print(paste(done, "% complete"))
    
  }
  row2<-do.call(bind_rows,row2)
  return(row2)
  
}


# Function to extract image collections with set dates----
Extract_with_var_dates<-function(loc,path,subpath){

Datebegin<-as.character(as.Date(loc$surveyDate,tryFormats = "%m/%d/%Y")-30)
Dateend<-as.character(as.Date(loc$surveyDate,tryFormats = "%m/%d/%Y"))

vec<-list()

for (i in 1:nrow(loc)) {
  terraclimate <- ee$ImageCollection(as.character(path))$select(as.character(subpath)) %>%
    ee$ImageCollection$filterDate(Datebegin[i], Dateend[i])
  
  vec1 <- ee_extract(
    x = terraclimate,
    y = loc[i,],
    scale = 50,
    fun = ee$Reducer$mean(),
    sf = FALSE
  ) 
  
  
  col_1<-ncol(loc)
  col_2<-ncol(loc)+1
  
  if(col_1==ncol(vec1)){vec1[,"t"]<-NA}
  else if(col_1>ncol(vec1)){
    vec1[,"t"]<-NA
    vec1[,"r"]<-NA
  }
  
  
  names(vec1)[col_1:col_2]<-c("1st_date","2nd_date")
  
  vec[[i]]<-vec1
}
vec<-do.call(rbind, vec)
num_nas <- sum(is.na(vec[,ncol(vec)]))
print(paste("Number of missing values:", num_nas))

return(vec)
}

# Extract multiple bands and dates from google earth engine----
Extract_var_with_const_date <- function(loc,path,subpath,select,datebegin,dateend) {
  batch_size=300
  
  if(select!=T){
    terra<-ee$Image(as.character(path))
  } else if(nchar(dateend)==0){
  terra<-ee$Image(as.character(path))$select(as.character(subpath))
  } else if(nchar(dateend)>0){terra <- ee$ImageCollection(as.character(path))$select(as.character(subpath)) %>%
    ee$ImageCollection$filterDate(datebegin, dateend)}
  
  num_iterations <- ceiling(nrow(loc) / batch_size)
  
  results<-list()
  # Loop through each batch
  for (i in 1:num_iterations) {
    # Calculate the start and end rows for the current batch
    start_row <- (i - 1) * batch_size + 1
    end_row <- min(i * batch_size, nrow(loc))
    
    # Extract current batch
    batch <- loc[start_row:end_row, ] 
    
    # Perform ee_extract for the current batch
    row <-ee_extract(
        x = terra,
        y = batch,
        scale = 50,
        fun = ee$Reducer$mean(),
        sf = FALSE
      ) # Adjust column index based on actual output structure
      #col<-colnames(row[,length(loc)])
      
    
    results[[i]]<-row
  }
  results<-do.call(rbind, results)

  
  return(results)
}



# Function to Handle Missing Data with Buffering ----
na_buffer_fill <- function(var,Data,loc1,path,fun,select,datebegin,dateend) {
  print(paste(var, "raster buffer starting at", 50))

  if(fun=="collect"){
    Data<-Extract_with_var_dates(loc1,path,var)}
    else {
    Data<-Extract_var_with_const_date(loc1,path,var,select,datebegin,dateend)}
  

  for (circ1 in seq(500, 10000, 500)) {
    
    if (anyNA(Data[,ncol(loc1)])) {
      # Get Missing Lat/Lon Points
      print(paste(var, "raster buffer starting at", circ1))
      nas<-is.na(Data[,ncol(Data)])
      print(paste("Number of missing values:", length((nas[nas]+0))))
      new_points <- loc1[nas,] %>%
        sf::st_buffer(dist = circ1)
      
      # Extract Buffered Data
      if(fun=="collect"){ 
        extracted1<-Extract_with_var_dates(new_points,path,var)}
      else {
      extracted1 <- Extract_var_with_const_date(new_points,path,var,select,datebegin,dateend)
      }
      # Update Data

      if(ncol(Data)==ncol(extracted1)){
      Data[nas,] <- extracted1}
    } else {
      message(paste(var, "raster buffering completed at", circ1))
      break
    }
  }
  return(Data)
}

# Wrapper function -----
Gradient_extract<-function(GEE_path="projects/soilgrids-isric/",subpath='bdod_mean',
               Data=data_survey,lat="Lat",lon="Long",Grad_var="Soil"){
 
  pp1<-sf::st_as_sf(Data,coords=c(lon,lat),crs="+proj=longlat +datum=WGS84")
  
  if(Grad_var=="Soil"){
    
    Soil_var <- c("ocs", "cec","bdod", "cfvo", "clay", "nitrogen", "ocd",
                  "phh2o", "sand", "silt", "soc")
    list<-list()
    
    for (var in Soil_var) {
      subpath = paste0(var,"_mean")
      path<-paste0(GEE_path,subpath)
      Data1<- na_buffer_fill(subpath, Data, pp1,path,"",select=F,'','')
      
      Data1<-Data1[,ncol(pp1)]
      
      list[[var]]<-Data1
    } 
    Data1<-do.call(cbind,list)
    Data1<-cbind(pp1,Data1)
    return(Data1)
  }else if(Grad_var=="Landcover"){
   Data2=as.data.frame(Land_cover_porp_1KM(GEE_path,pp1))
   oldnames = c("10","20","30",'40','50','60','70','80','90','95','100')
   newnames = c("Tree cover","Shrubland","Grassland",
                'Cropland',"Buildings",'Bare','Snow','Water',
                'Herb Wetland',"Mangroves",'Moss')
   
   for (col in oldnames) {
     if (!col %in% colnames(Data2)) {
       Data2[[col]] <- 0
     }
   }
   
   Data2<-Data2 %>% 
     rename_with(~ newnames[match(.x, oldnames)], any_of(oldnames))
   
   Data2[is.na(Data2)]<-0
   
   data_survey=bind_cols(Data,Data2)
   return(data_survey)
  }else if(Grad_var=="NDVI"){
    data_survey=na_buffer_fill(subpath,Data,pp1,GEE_path,"collect",select=T,'','')
    data_survey<-cbind(pp1,NVDI=data_survey[,ncol(pp1)])
    
    return(data_survey)
  } else if(Grad_var=="Aridity"){
    var<-Grad_var
    data_survey<- na_buffer_fill(var, Data, pp1,GEE_path,"na",select=F,'','') %>% 
      rename(var=b1)
    data_survey<-cbind(pp1,Aridity=data_survey[,ncol(pp1)])
    return(data_survey)
  }else if(Grad_var=="Climate"){
    

    Data<- na_buffer_fill('', Data, pp1,GEE_path,"",select=F,'','')
    Data1<-cbind(pp1,Data[,grep("bio",colnames(Data))])
    return(Data1)
  }else if(Grad_var=="LeafIndex"){
    select_num<-function(df){
      test<-df[,ncol(Data):ncol(df)]
      split_dfs <- lapply(seq(1,ncol(test),12), function(i) test[, i:min(i + 12 - 1, ncol(test))])
      Data<-as.matrix(Reduce(`+`, split_dfs) / length(split_dfs))
    }
    
    
    low_max<-select_num(na_buffer_fill('leaf_area_index_low_vegetation_max', Data, pp1,GEE_path,"na",select=T,'2017-01-01','2023-12-31'))
    high_max<-select_num(na_buffer_fill('leaf_area_index_high_vegetation_max', Data, pp1,GEE_path,"na",select=T,'2017-01-01','2023-12-31'))
    low_min<-select_num(na_buffer_fill('leaf_area_index_low_vegetation_min', Data, pp1,GEE_path,"na",select=T,'2017-01-01','2023-12-31'))
    high_min<-select_num(na_buffer_fill('leaf_area_index_high_vegetation_min', Data, pp1,GEE_path,"na",select=T,'2017-01-01','2023-12-31'))
    temp2m<-select_num(na_buffer_fill('temperature_2m', Data, pp1,GEE_path,"na",select=T,'2017-01-01','2023-12-31'))
    soil_temp_surf<-select_num(na_buffer_fill('soil_temperature_level_1', Data, pp1,GEE_path,"na",select=T,'2017-01-01','2023-12-31'))
    
    soil_temp_surf<-colMeans(t(soil_temp_surf))
    
    low<-as.data.frame(biovars(temp2m,
                 low_min,
                 low_max)[,c("bio1","bio8")])
    names(low)<-c("low_veg_mean_LI_year" ,"low_veg_mean_LI_growing")
    
    
    high<-as.data.frame(biovars(temp2m,
                 high_min,
                 high_max)[,c("bio1","bio8")])
    
    names(high)<-c("high_veg_mean_LI_year" ,"high_veg_mean_LI_growing")
    
    data<-cbind(pp1,low,high,soil_temp_surf)
    #data<-do.call(rbind,bio_empt)
    return(data)
    }
  
  

  }



#terraclimate <- ee$ImageCollection('ECMWF/ERA5_LAND/MONTHLY_AGGR')
#ee_print(terraclimate)



