
##########Figure 1A Sample collection distribution map

rm(list=ls())

library("maptools")
library(maps)
library(mapdata)
map("china")


mydat = readShapePoly("bou2_4p.shp")

plot(mydat)


getColor=function(mapdata,provname,provcol,othercol)
{
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(mapdata@data$NAME,f,provname);
  col=c(othercol,provcol)[colIndex+1];
  return(col);
}



provname=c("西藏自治区","新疆维吾尔自治区",
           "青海省","陕西省","安徽省","北京市"
           ,"福建省","广东省","广西省"
           ,"海南省","河南省","湖北省","江苏省",
           "山东省","上海市","四川省","云南省","浙江省")

pop=c(276,96,69,103,1,5,2,10,4,3,
      4,43,30,9,7,4,1,1);

provcol=rgb(red=1-pop/max(pop)/2,green=1-pop/max(pop)/2,blue=0);

plot(mydat,col=getColor(mydat,provname,provcol,"white"),xlab="",ylab="")

