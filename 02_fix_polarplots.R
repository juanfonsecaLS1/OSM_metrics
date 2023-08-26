library(magick)


my_images = list.files(path = "02_network_metrics/",pattern = "^plot_.*\\.png",full.names = T)

lapply(my_images,function(x){
  my_tmp=magick::image_read(x)

  my_tmp_cr=image_crop(my_tmp,geometry = "1229x1029+100+200")

  image_write(my_tmp_cr,path = x)
})
