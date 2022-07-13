require(magick)

setwd("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/cox_regression")
files = list.files(pattern="*.tif")

tiff("balanded_1.tiff", units="in", width=21, height=26, res=300, compression = 'lzw')
par(mfrow=c(5,3)) 

for (i in 1:15) {
  name <- files[i] 
  lag <- strsplit(name, "_")[[1]][2] #get the name right for the image
  match <- strsplit(name, "_")[[1]][4]
  lead <- strsplit(name, "_")[[1]][6]
  lead <- strsplit(lead, ".tiff")[[1]][1]
  name <- paste("   Lag=",lag,", Match=1:",match,", Lead=",lead, sep = "") #put the name together
  img <- image_annotate(image_read(files[i]), name, font = 'Times', size = 120) #read the image and add the name
  img <- image_crop(img, "1500x1050")
  plot(img) 
}
dev.off() #save figure



require(raster)

setwd("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/cox_regression")
files = list.files(pattern="*.tif")

tiff("balanded_1.tiff", units="in", width=21, height=26, res=300, compression = 'lzw') #for saving

par(mfrow=c(5,3)) 

for (i in 1:15) {
  plotRGB(brick(files[i]))
}

dev.off() #save figure