# 확산모델 시각화
pacman::p_load("ggplot2","dplyr")
options(scipen=100) #global
theme_set(theme_bw(base_family="AppleGothic")) #한글깨짐 문제 해결
load("simple.Rdata")

# p <- ggplot(test1, aes(x=long, y=lat, group=group, fill=한우)) + geom_polygon(colour="black") + coord_map()
# p <- ggplot(merge, aes(x=long,y=lat,group=group,fill=한우)) + geom_polygon() + scale_fill_gradientn(colours=c('white','orange','red')) + expand_limits(x=merge$long,y=merge$lat)+ ggtitle("한우두수") + labs(fill="한우 두수")

p1 <- ggplot(merge, aes(x=long,y=lat,group=group,fill=고상.분뇨)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','orange','red'))+ expand_limits(x=merge$long,y=merge$lat)+ ggtitle("고상가축분뇨발생(톤/년)") + labs(fill="발생(톤/년)") 

# + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
print(p1)

p2 <- ggplot(merge, aes(x=long,y=lat,group=group,fill=고상.합계.차이)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('blue','white','red'))+ expand_limits(x=merge$long,y=merge$lat)+ ggtitle("고상가축분뇨발생-처리(톤/년)") + labs(fill="Difference")

print(p2)

p3 <- ggplot(merge, aes(x=long,y=lat,group=group,fill=고상.처리)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','#67C8FF','blue'))+ expand_limits(x=merge$long,y=merge$lat)+ ggtitle("고상가축분뇨처리(톤/년)") + labs(fill="처리(톤/년)") 
print(p3)

p4 <- ggplot(merge, aes(x=long,y=lat,group=group,fill=계)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','#58D68D','#186A3B'))+ expand_limits(x=merge$long,y=merge$lat)+ ggtitle("경작지 면적") + labs(fill="경작지 면적") + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
print(p4)

p5 <- ggplot(merge, aes(x=long,y=lat,group=group,fill=논)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','#58D68D','#186A3B'))+ expand_limits(x=merge$long,y=merge$lat)+ ggtitle("경작지 면적(논)") + labs(fill="경작지 면적") + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
print(p5)

p6 <- ggplot(merge, aes(x=long,y=lat,group=group,fill=밭)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','#58D68D','#186A3B'))+ expand_limits(x=merge$long,y=merge$lat)+ ggtitle("경작지 면적(밭)") + labs(fill="경작지 면적") + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
print(p6)

ggsave("generation.png",plot=p1,dpi=300)
ggsave("diff.png",plot=p2,dpi=300)
ggsave("treated.png",plot=p3,dpi=300)

ggsave("논.png",plot=p5,dpi=300)
ggsave("밭.png",plot=p6,dpi=300)

group <- group_by(merge, A2) %>% summarise(발생량=round(mean(고상.분뇨),0),처리량=round(mean(고상.처리),0))
result <- mutate(group, 차이=발생량-처리량)
result[,2:4] <- result[,2:4]/1000
result$단위 <- "만톤/년"
result <- filter(result, 발생량!=0 | 처리량!=0)
colnames(result) <- c("시군구","발생량","처리량","차이","단위")

DT::datatable(result)


sum(result$차이, na.rm=TRUE)

sum(group$발생량,na.rm=TRUE)/10000

merge$output <- merge$output %>% round(0)
merge$diff <- merge$diff %>% round(0)
DT::datatable(merge)
