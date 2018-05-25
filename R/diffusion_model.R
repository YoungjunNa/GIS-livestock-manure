# map load
theme_set(theme_gray(base_family="NanumGothic"))
pacman::p_load("rgdal","ggmap","ggplot2","maps","plyr","dplyr")

sig_shp <- readOGR("/Users/youngjunna/Github/diffusion_model/R","전국_법정구역정보(시군구)_201510")
sig_shp <- readOGR("/Users/youngjun/Github/diffusion_model/R","전국_법정구역정보(시군구)_201510")

sig_shp@data$id <- rownames(sig_shp@data) #idx 부여 
sig_shp_fortify <- fortify(sig_shp,by="id") #fortify함수 region =  id로
sig_shp_fortify_join <- join(sig_shp_fortify,sig_shp@data, by="id") #id로 조인!

simple <- rmapshaper::ms_simplify(sig_shp,keep=0.005)
simple@data$id <- rownames(simple@data) #idx 부여 
simple_fortify <- fortify(simple,by="id") #fortify함수 region =  id로
simple_join <- join(simple_fortify,simple@data, by="id") #id로 조인!

# write.csv(simple_join,"simple.txt",row.names=FALSE)
