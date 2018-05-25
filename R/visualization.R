# 확산모델 시각화
pacman::p_load("ggplot2","dplyr","plotly","extrafont")
farm <- read.csv("merge.txt")

options(scipen=100) #global
font_import()
theme_set(theme_bw(base_family="AppleGothic")) #한글깨짐 문제 해결

p <- ggplot(test1, aes(x=long, y=lat, group=group, fill=한우)) + geom_polygon(colour="black") + coord_map()
p <- ggplot(farm, aes(x=long,y=lat,group=group,fill=한우)) + geom_polygon() + scale_fill_gradientn(colours=c('white','orange','red')) + expand_limits(x=farm$long,y=farm$lat)+ ggtitle("한우두수") + labs(fill="한우 두수")

p1 <- ggplot(farm, aes(x=long,y=lat,group=group,fill=고상.분뇨)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','orange','red'))+ expand_limits(x=farm$long,y=farm$lat)+ ggtitle("고상가축분뇨발생(톤/년)") + labs(fill="발생(톤/년)") + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
print(p1)

p2 <- ggplot(farm, aes(x=long,y=lat,group=group,fill=고상.합계.차이)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('blue','white','red'))+ expand_limits(x=farm$long,y=farm$lat)+ ggtitle("고상가축분뇨발생-처리(톤/년)") + labs(fill="Difference") + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
print(p2)

p3 <- ggplot(farm, aes(x=long,y=lat,group=group,fill=고상.처리)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','#67C8FF','blue'))+ expand_limits(x=farm$long,y=farm$lat)+ ggtitle("고상가축분뇨처리(톤/년)") + labs(fill="처리(톤/년)") + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
print(p3)

# p4 <- ggplot(farm, aes(x=long,y=lat,group=group,fill=고상.처리)) + geom_polygon(colour="grey") + scale_fill_gradientn(colours=c('white','#58D68D','#186A3B'))+ expand_limits(x=farm$long,y=farm$lat)+ ggtitle("경작지 면적") + labs(fill="경작지 면적") + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
# print(p4)

ggsave("generation.png",plot=p1,dpi=300)
ggsave("diff.png",plot=p2,dpi=300)
ggsave("treated.png",plot=p3,dpi=300)

group <- group_by(farm, A2) %>% summarise(발생량=round(mean(고상.분뇨),0))
sum(group$합계.차이,na.rm=TRUE) 

farm$output <- farm$output %>% round(0)
farm$diff <- farm$diff %>% round(0)
DT::datatable(farm)
