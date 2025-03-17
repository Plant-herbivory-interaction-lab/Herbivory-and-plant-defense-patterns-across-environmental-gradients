# Load libraries
library(flextable)

Table<-function(data1=data,col='p_value',output="output_table.pdf"){
data1<-data1 %>% 
  mutate_if(is.numeric,round,digits=3)
  
  
  # Step 3: Create a flextable
ft <- flextable(data1) %>%
  theme_booktabs(bold_header = T) %>%
  bold(j = col, i = 0.05>data1[,col]) # Bold significant p-values
ft<-autofit(ft)

tmp<-tempfile("table", fileext = c(".png"))

# Step 4: Save flextable as an image
save_as_image(ft, path = tmp)

# Step 5: Embed the image in a PDF
pdf(output, width = 8, height = 4) # Open PDF device
grid::grid.raster(png::readPNG(tmp)) # Insert table image
dev.off() # Close PDF device
}
