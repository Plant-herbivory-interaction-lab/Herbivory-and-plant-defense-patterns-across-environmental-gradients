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
  hjust = with(datapc[,2:3], (1 - 1.5* sign(PC1)) / 2)
  Lab<-paste(c("PC1","PC2"), 
             sprintf('(%0.1f%% explained variance)', 
                     100 * PC$sdev[1:2]^2/sum(PC$sdev^2)))
  
  plot <- ggplot(data, aes(x=PC1, y=PC2))+geom_point(alpha=0.2)
  plot <- plot + geom_label_repel(data=datapc, aes(x=PC1*ext, y=PC2*ext, label=varnames), color="black",angle = angle, hjust = hjust)
  plot <- plot + 
    geom_segment(data=datapc, aes(x=0, y=0, xend=PC1*ext, yend=PC2*ext), 
                 arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="black")+
    labs(x=Lab[1],y=Lab[2])+
    theme_bw(base_size = font_size)+theme(panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank())
  
  plot
}


