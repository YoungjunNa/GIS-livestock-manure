pacman::p_load("rgdal","sp","ggmap","ggplot2","maps","plyr","dplyr","reshape2")

# sig_shp <- readOGR("/Users/youngjunna/Github/diffusion_model/R","전국_법정구역정보(시군구)_201510")

sig_shp@proj4string #origin
prj_longlat <- CRS("+proj=longlat +datum=WGS84") #경위도 wgs84타원체
sig_shp_after<-spTransform(sig_shp,prj_longlat)

sig_shp_after@data$id <- rownames(sig_shp_after@data) #idx 부여 
sig_shp_fortify <- fortify(sig_shp_after,by="id") #fortify함수 region =  id로
sig_shp_fortify_join <- join(sig_shp_fortify,sig_shp_after@data, by="id") #id로 조인!

# 시군기준 사육두수 ####
# animal <- read.csv("animal-2016.txt",stringsAsFactors = FALSE)
animal <- dcast(animal, 시군~축종명, value.var="규모",sum)
animal <- animal[c("시군","한우","육우","젖소","산란계","육계","돼지")]
colnames(animal) <- c("시군","한우.두수","육우.두수","젖소.두수","산란계.두수","육계.두수","돼지.두수")
merge <- merge(sig_shp_fortify_join, animal, by.x="A2",by.y="시군",all=TRUE)
merge <- arrange(merge, group, order)
a <- merge[-c(1:11)]
a[is.na(a)] <- 0
merge <- cbind(merge[1:11],a)

p1 <- ggplot(merge, aes(x=long,y=lat,group=group,fill=젖소.두수)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','orange','red'))+ expand_limits(x=merge$long,y=merge$lat)+ ggtitle("시군별 젖소두수") + labs(fill="두수") 

print(p1)
