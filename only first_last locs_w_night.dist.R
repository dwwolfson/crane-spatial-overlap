#get to dataset of just night/morning locations
library(gdata)
library(ggplot2)
library(dplyr)
#Import newdat from the dataframe folder
newdat<-read.csv(file="./data/newdat.csv", header=T)   #this assumes that you're under the CTMM R project and that 
# the CTMM folder is your working directory

unique(newdat$id)  #31 cranes
#change from factor to character
newdat$loctime<-as.character(newdat$loctime)

#switch to posix form
newdat$loctime<-as.POSIXct(newdat$loctime, tz="UTC")

summary(newdat$loctime)  #dates range from 4/1/16 to 7/15/16

#subset to just colts
newdat<-newdat[newdat$age=="colt",]
wtf<-newdat[newdat$id=="5K (Helliksen July adult 2015)",]  #miscoded as a colt
newdat<-newdat[!newdat$id=="5K (Helliksen July adult 2015)",]
newdat<-droplevels(newdat)
unique(newdat$id) #just 13 colts

crane.names<-as.list(unique(newdat$id))
testdat<-newdat
hist(newdat$Hour)

temp.crane<-NULL
temp.crane.day<-NULL
dat.summary<-NULL
tempdat<-NULL
firstday<-NULL
lastday<-NULL
dat.summary<-NULL
i<-NULL
j<-NULL
for(i in 1:length(unique(testdat$id))){
  temp.crane<-testdat[testdat$id==crane.names[[i]],]
  if(nrow(temp.crane)>0){
    current.crane<-as.character(crane.names[[i]])
    temp.days<-as.list(unique(temp.crane$day))
    days.vec<-unlist(temp.days)
    firstday<-days.vec[1]
    lastday<-last(days.vec)
    for(j in seq.int(from=firstday, to=lastday)){
        tempdat<-temp.crane[which(temp.crane$day==j),]    
        firstrow<-tempdat[1,]
        lastrow<-tempdat[nrow(tempdat),]
        daylocs<-rbind(firstrow, lastrow)
        
        if((j-1)<firstday)
        {daylocs$night.dist<-NA}
        else{
        prevdat<-temp.crane[which(temp.crane$day==j-1),]    
        firstrow<-prevdat[1,]
        lastrow<-prevdat[nrow(prevdat),]
        prevdaylocs<-rbind(firstrow, lastrow)
        
        daylocs$night.dist<-sqrt((daylocs$x[1]-prevdaylocs$x[2])^2+(daylocs$y[1]-prevdaylocs$y[2])^2)
        }
        
        dat.summary<-rbind(dat.summary, daylocs)
      
      }
      }
     }
  

head(dat.summary)
dat.summary[1]
hist(dat.summary$night.dist)
summary(dat.summary$night.dist)   #there are some rows where the first and last locations weren't actually
                                  # at sunrise or sunset, so I'll subset to only keep data in the right time frame

table(dat.summary$Hour)           #this shows it as well

library(dplyr)
dat.test<-filter(dat.summary, Hour)
dat.test<-dat.summary[which(dat.summary$Hour>=5 &dat.summary$Hour<=7|dat.summary$Hour>=19&dat.summary$Hour<=21),]
table(dat.test$Hour)

summary(dat.test$Hour)
hist(dat.test$night.dist, ylim=c(0,10000))
summary(dat.test$night.dist)
plot(dat.test$day, dat.test$night.dist, ylim=c(0,15000))
length(which(dat.test$night.dist>1000))



str(dat.summary)
morn.night<-ggplot(dat.summary, aes(x=x, y=y, colour=id))+geom_point()
morn.night+geom_path()+facet_wrap(~id)

#need to convert from utms to 


library(ggmap)
library
map<-get_map("Minnesota", zoom=5)
plot(map)
attr(map, "bb")
#extent for data points
bbox = c(-101.7, 41.7, -87.6, 51.3)
map2<-get_stamenmap(bbox, maptype="terrain", zoom=6)
map2

p<-ggmap(map)
r<-p+geom_point(data=dat.summary, aes(x=x, y=y, colour=id))
r

