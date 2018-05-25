pacman::p_load("dplyr","kormaps2014","reshape2")
simple_join <- read.csv("simple.txt")

# 가축분뇨 배출원단위(kg/두/일) ####
hanwoo <- data.frame(bw=350,feces=8,urine=5.7,total=13.7,output=13.7)
holstein <- data.frame(bw=450,feces=19.2,urine=10.9,total=30.1,output=37.7)
swine <- data.frame(bw=60,feces=0.87,urine=1.74,total=2.49,output=5.1)
layer <- data.frame(bw=1.7,feces=0.1247,urine=NA,total=0.1247,output=0.1247)
broiler <- data.frame(bw=1.3,feces=0.0855,urine=NA,total=0.0855,output=0.0855)
manure_out <- list(hanwoo=hanwoo,holstein=holstein,swine=swine,layer=layer,broiler=broiler)

# 시군기준 사육두수 ####
animal <- read.csv("animal-2016.txt",stringsAsFactors = FALSE)
animal <- dcast(animal, 시군~축종명, value.var="규모",sum)
animal <- animal[c("시군","한우","육우","젖소","산란계","육계","돼지")]
colnames(animal) <- c("시군","한우.두수","육우.두수","젖소.두수","산란계.두수","육계.두수","돼지.두수")
merge <- merge(simple_join, animal, by.x="A2",by.y="시군",all=TRUE)
merge <- arrange(merge, group, order)
a <- merge[12:17]
a[is.na(a)] <- 0
merge <- cbind(merge[1:10],a)

# 분뇨 발생량 ####
merge <- mutate(merge, 한육우.분뇨=(한우.두수+육우.두수)*13.7*365/1000)
merge <- mutate(merge, 젖소.분뇨=젖소.두수*37.7*365/1000)
merge <- mutate(merge, 돼지.분뇨=돼지.두수*5.1*365/1000)
merge <- mutate(merge, 육계.분뇨=육계.두수*0.0855*365/1000)
merge <- mutate(merge, 산란계.분뇨=산란계.두수*0.1247*365/1000)
merge <- mutate(merge, 고상.분뇨=한육우.분뇨+젖소.분뇨+육계.분뇨+산란계.분뇨)
merge <- mutate(merge, 합계.분뇨=한육우.분뇨+젖소.분뇨+돼지.분뇨+육계.분뇨+산란계.분뇨)

# 분뇨 처리량 ####
manure2 <- read.csv("manure2.txt")
manure2 <- dcast(manure2, 시군~축종, value.var="발생량",sum)
colnames(manure2) <- c("시군","가금.처리","돼지.처리","젖소.처리","한육우.처리","합계.처리")
merge <- merge(merge,manure2,by.x="A2",by.y="시군",all=TRUE)
merge <- arrange(merge, group, order)
a <- merge[-c(1:16)]
a[is.na(a)] <- 0
merge <- cbind(merge[1:16],a)

# 발생-처리 ####
merge <- mutate(merge, 고상.처리=젖소.처리+한육우.처리)
merge <- mutate(merge, 고상.합계.차이=고상.분뇨-고상.처리)
merge <- mutate(merge, 합계.차이=합계.분뇨-합계.처리)

# 경지면적 ####
farmland <- readxl::read_excel("farmland.xlsx")
write.csv(farmland,"farmland.txt",row.names=FALSE)
farmland <- read.csv("farmland.txt")
# farmland$시군구 <- gsub(" ","",farmland$시군구)
farmland$시군구 <- trimws(farmland$시군구,which = "both")
str(farmland)

write.csv(merge,"merge.txt",row.names=FALSE)
