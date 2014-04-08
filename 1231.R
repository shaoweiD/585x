
library(reshape)
library(ggplot2)
age.sub$name<-1:74
remove(dfr)
head(age.sub)
age.sub[,1] <- as.Date(x=paste("1-",age.sub[,1], sep=""), format="%d-%b-%y")
age.sub[,2] <- as.Date(x=paste("1-",age.sub[,2], sep=""), format="%d-%b-%y")
age.sub$"# of case by date of confirmation"<-as.numeric(age.sub$"# of case by date of confirmation")
mdfr <- melt(age.sub, measure.vars = c("start", "end"))

new<-melt(mdfr[,c(4:7)],id=c("variable","value"))
colnames(new)<-c("variable","value","type","data")
#mdfr$"# of case by date of confirmation"<-as.numeric(mdfr$"# of case by date of confirmation")
ggplot(mdfr, aes(mdfr$value,as.factor(mdfr$"# of case by date of confirmation"))) + 
  geom_line(size = 6) +
  xlab("period") + ylab("number of cases by date of restriction") +
  theme_bw()

pd <- position_dodge(0.1)
ggplot(data = new, aes(x = value, y = as.numeric(data), colour = type)) + geom_line(position = pd, aes(group = variable))



p+ facet_grid(. ~ new$type)
sapply(try, function(x) !all(is.na(as.Date(as.character(x),format="%b-%y"))))
try[,1] <- as.Date(x=paste("1-",try[,1], sep=""), format="%d-%b-%y")
try[,2] <- as.Date(x=paste("1-",try[,2], sep=""), format="%d-%b-%y")
p <- ggplot(new, aes(new$value,as.numeric(new[,2]))) + geom_point()
# With one variable
p + facet_grid(. ~as.numeric(mdfr[,1]))
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p + facet_grid(. ~ cyl)
ggplot()+ facet_grid(data=new,aes(value,type))
ggplot(mdfr, aes(as.Date(value, "%d-%b-%y"), name)) + 
  geom_line(size = 6) +
  xlab("") + ylab("") +
  theme_bw()
mdfr$value[order(sort(as.numeric(mdfr$"# of case by date of confirmation")))]
a<-order(mdfr$value[order(sort(as.numeric(mdfr$"# of case by date of confirmation")))])
#trend of passive surveillance




###############distribution of passive surveillance######################################################################################

uk<-readShapeSpatial('E:/585x/uk/map.shp')


xxx <- thinnedSpatialPoly(as(uk, "SpatialPolygons"), tolerance = 0.1, minarea = 0.001, topologyPreserve = TRUE)
#Read polygons
extractPolygons <- function(shapes) {
  require(plyr)
  
  dframe <- ldply(1:length(shapes@polygons), function(i) {
    ob <- shapes@polygons[[i]]@Polygons
    dframe <- ldply(1:length(ob), function(j) {
      x <- ob[[j]]
      co <- x@coords
      data.frame(co, order = 1:nrow(co), group = j)
    })
    dframe$region <- i
    dframe
  })
  
  # construct a group variable from both group and polygon:
  dframe$group <- interaction(dframe$region, dframe$group)
  
  dframe
}
oz <- extractPolygons(xxx)
ggplot(oz, aes(x = x, y = y, group = group)) + 
  geom_polygon(colour='black', fill='white')
uk@data$MM_UID<-1:192
oz.new<- merge(oz, uk@data, by.x = "region", by.y = "MM_UID")
sort(city$COUNTY)
sort(unique(oz.new$NAME2))
ggplot(oz.new, aes(x = x, y = y, group = group)) + 
  geom_polygon(colour='black', fill='white')

#Clean the data
city$COUNTY<-gsub("&","and",city$COUNTY)
city$COUNTY[city$COUNTY=="Ayrshire" ]<-"South Ayrshire"           
city$COUNTY[city$COUNTY=="Cornwall and Isles of Scilly" ]<-"Cornwall"    
city$COUNTY[city$COUNTY=="East Riding and Northern Lincolnshire" ]<-"North Lincolnshire" 
city$COUNTY[city$COUNTY=="Eileanan an lar"]<-"Eilean Siar"  
city$COUNTY[city$COUNTY=="Greater London"]<-"London" 
city$COUNTY[city$COUNTY=="Greater Manchester"]<-"Manchester"
city$COUNTY[city$COUNTY=="Gloucestershire excl South"]<-"South Gloucestershire"
city$COUNTY[city$COUNTY=="Leicestershire and Rutland"]<-"Leicestershire"
city$COUNTY[city$COUNTY=="Lincolnshire excl North"]<-"North Lincolnshire"
city$COUNTY[city$COUNTY=="North-East Scotland"]<-"Arberdeen"
city$COUNTY[city$COUNTY=="North-East Wales"]<-"Herefordnshire"
city$COUNTY[city$COUNTY=="North-West Wales"]<-"Ceredigion"
city$COUNTY[city$COUNTY=="Northern Somerset and South Glouceste"]<-"Northern Somerset"
city$COUNTY[city$COUNTY=="Somerset excl North"]<-"North Somerset"
city$COUNTY[city$COUNTY=="South Wales"]<-"Cardiff"
colnames(city)<-c("region","farms","cases")
#merge two files 
distribution<-merge(oz.subset,city,by="region")
oz.subset<-oz.new[,c(2:5,7,8,9)]
colnames(oz.subset)<-c("x","y","order","group","area","region","division")
#distribution of farms and BSE cases 
distribution$cases<-as.numeric(distribution$cases)
ggplot()+geom_polygon(data=oz.new, aes(x = x, y = y, order = order, group = group))+ 
  geom_polygon(data = distribution, aes(x = x,y = y,order = order, group = group,fill =distribution$cases))

distribution$farms<-as.numeric(distribution$farms)
ggplot()+geom_polygon(data=oz.new, aes(x = x, y = y, order = order, group = group))+ 
  geom_polygon(data = distribution, aes(x = x,y = y,order = order, group = group,fill =distribution$farms))
#percent reduction
head(trend)
red<-trend[,c(1,2,8)]
red$"percent reduction year on year(suspected)"<-NA
red$"percent reduction year on year(confirmed)"<-NA
i<-NA
j<-NA
per<-NA
red[1:28,2:3]<-as.matrix(sapply(red[1:28,2:3], as.numeric))  
percent <- function(x, digits = 2, format = "f", ...)
{
  paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
}

for (i in 3:(length(red$SUSPECTS.RESTRICTED))){
  
  red$"percent reduction year on year(suspected)"[i]<-as.numeric((-red$SUSPECTS.RESTRICTED[i-1]+red$SUSPECTS.RESTRICTED[i])/red$SUSPECTS.RESTRICTED[i-1])
  red$"percent reduction year on year(confirmed)"[i]<-as.numeric((-red$SLAUGHTERED.SUSPECTS.IN.WHICH.BSE.CONFIRMED[i-1]+red$SLAUGHTERED.SUSPECTS.IN.WHICH.BSE.CONFIRMED[i])/red$SLAUGHTERED.SUSPECTS.IN.WHICH.BSE.CONFIRMED[i-1])
  if (red$"SLAUGHTERED.SUSPECTS.IN.WHICH.BSE.CONFIRMED"[i-1]==0){red$"percent reduction year on year(confirmed)"[i]<-0}
}

red.melt<-melt(red[,c(1,4,5)],id=c("YEAR"))
red.melt$value<-as.numeric(red.melt$value)
red.melt$value[is.na(red.melt$value)] <- 0
pd <- position_dodge(0.1)
ggplot(data = red.melt, aes(x = YEAR, y = value, colour = variable)) + geom_line(position = pd, aes(group = variable))

#age in great britain
age<-read.csv("C:/Users/sding/Documents/GitHub/585xproject/age.gb.csv")
age<-age[-1,-2]
age$"Birth Period"<-1982:2006
colnames(age)<-c("Birth Period","1 year old","2 year old","3 year old","4 year old","5 year old","6 year old","7 year old","8 year old","9 year old","10 year old")
age.melt<-melt(age,id="Birth Period")
age.melt$value<-as.numeric(age.melt$value)
age.melt$"Birth Period"<-as.numeric(age.melt$"Birth Period")
age.melt$"variable"<-as.factor(age.melt$"variable")
qplot()
pd <- position_dodge(0.1)
ggplot(data =age.melt, aes(x =age.melt$"Birth Period", y =value, colour = variable)) + geom_line(position = pd, aes(group = variable))
```

# In recent two years
recent<-read.csv("C:/Users/sding/Documents/GitHub/585xproject/recent.csv")
a<-recent[-1,c(1,2,3,4)]
recent[,c(2,3,4)]<-as.matrix(sapply(recent[,c(2,3,4)], as.numeric))  
a$cases<-rowSums(recent[-1,c(2,3,4)])
a$County<-gsub("&","and",a$County)
a<-a[,-c(2,3,4)]
a$County[a$County=="North-West Wales"]<-"Ceredigion"
colnames(a)<-c("region","cases")

rec.dis<-merge(oz.subset,a,by="region")
rec.dis$cases<-as.numeric(rec.dis$cases)
ggplot()+geom_polygon(data=oz.subset, aes(x = x, y = y, order = order, group = group))+ 
  geom_polygon(data =rec.dis, aes(x = x,y = y,order = order, group = group,fill =rec.dis$cases))