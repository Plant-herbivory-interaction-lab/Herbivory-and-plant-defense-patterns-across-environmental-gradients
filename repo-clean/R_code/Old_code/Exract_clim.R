# Code by: Jacob Herschberger
# Date: July 2024


Extract_clim <- function(filepath="../General/Example R folder/bioclim/30s_res/",Data=Data,Latt="Lat",Long="Long"){
files<-list.files(filepath)[grep("tif",list.files(filepath))]
Data<-as.data.frame(Data)

pp1<-sf::st_as_sf(Data,coords=c(Long,Latt),crs="+proj=longlat +datum=WGS84")

for (var1 in files) {
  
 
  ras1<-raster::raster(paste(filepath,var1,sep=""))
  extracted<-raster::extract(ras1,pp1)
  
  var<-gsub('wc2.1_','',gsub('_mean','',gsub('.tif', '', var1)))
  
  Data[,var]<-extracted
  
  
  
  for (circ in seq(50,3000,50)) {
    
    if(length(Data[is.na(Data[,var]),Latt])>0){
      Lat<-Data[is.na(Data[,var]),Latt]
      Lon<-Data[is.na(Data[,var]),Long]
      
      new.points<-na.omit(as.matrix(cbind(Lon,Lat)))
      extracted1<-raster::extract(ras1,buffer=circ,new.points,fun=mean,na.rm=T)
      
      Data[is.na(Data[,var]),var]<-extracted1
    }else{print(paste(var,"completed",'at',circ,sep=" "))
      break
      }
  }
  }

pp2<-sf::st_transform(pp1,'+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs')# transform to the soil projection (in meters)

Soil_var<-c("ocs", "cec","bdod", "cfvo", "clay", "nitrogen", "ocd",
            
            "phh2o", "sand", "silt", "soc")

source("R_code/Soils_world_vis_mod.R") # load customized function from the geodata package

fun1<-function(var){
  extracted<-raster::extract(soil_world_vsi_mod(var, depth=5,stat = "mean"),pp2)
  Data[,var]<-extracted
  return(Data)}

fun2<-function(var){
  for (circ1 in seq(50,10000,50)) {
    circ<-circ1
    if(length(Data[is.na(Data[,var]),Latt])>0){
      Lat<-Data[is.na(Data[,var]),Latt]
      Lon<-Data[is.na(Data[,var]),Long]
      
      new.points2<-as.data.frame(na.omit(as.matrix(cbind(Lon,Lat))))
      new.points1<-sf::st_as_sf(new.points2,coords=c("Lon","Lat"),crs="+proj=longlat +datum=WGS84")
      new.points3<-sf::st_transform(new.points1,'+proj=igh')# transform to the soil projection (in meters)
      new.points<-sf::st_buffer(new.points3,circ)
      
      ras<-soil_world_vsi_mod(var, depth=5,stat = "mean")
      extracted1<-raster::extract(ras,new.points,fun=mean,na.rm=T)
      
      Data[is.na(Data[,var]),var]<-extracted1
      
      
    }else{print(paste(var,"raster buffering completed",'at',circ,sep=" "))
      break
      
    } 
    
  }
  return(Data)}


for (var1 in Soil_var) {
  if(length(Data[,grep(var1,colnames(Data))])==0){
    Data<-fun1(var1)
    Data<-fun2(var1)
    
  }else{
    print(paste(var1,"column exists",sep=" "))
    Data<-fun2(var1)}
  
 
}


return(Data)
}
