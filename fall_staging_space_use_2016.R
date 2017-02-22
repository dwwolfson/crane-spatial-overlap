#Fall Staging Space Use 2016

library(dplyr)
library(gdata)
library(sp)
?gBuffer
library(rgeos)
#import data from movebank
#these are locations from all active cranes from August 1-October 1st 2016
fall16<-read.csv("~/R/CraneMovement/scripts/post-December 2016/clustered roosting locs fall 2016/Grus canadensis Minnesota (2).csv")
alldat<-fall16  #so that I can use script already set up for a different name

#' drop unnecesary columns
alldat<-alldat[,c(4,5,17,23)]
head(alldat)
str(alldat)

#change name to make simpler
colnames(alldat)[4]<-"loctime"

#change from factor to character
alldat$loctime<-as.character(alldat$loctime)

#convert datetime into POSIX.ct
alldat$loctime<-as.POSIXct(x = alldat$loctime,format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")

#check for NA's
apply(is.na(alldat), 2, sum)
#remove records with NA's
alldat<-na.omit(alldat)

colnames(alldat)[3]<-"id"
str(alldat)

library(lubridate)
alldat$Hour<-hour(alldat$loctime)
alldat$day<-yday(alldat$loctime)
alldat$Week<-week(alldat$loctime)
alldat$Month<-month(alldat$loctime)
table(alldat$Hour)   

#extract coordinates
coordinates(alldat)<-c("location.long", "location.lat")

#define coordinate ref system
proj4string(alldat)<-CRS("+proj=longlat +datum=WGS84")

#project all coordinates into "USA Contiguous Albers Equal Area Conic"  (EPSG:102003)
#this seems like a good projection because 1)it has equal area if I want to calculate distance,
# 2) I've been using it already, so there is continuity, and 3) I don't have to mess around with
# overlapping UTM zones (of which I have 3 I think)
alldat<-spTransform(alldat, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
+lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))

#change colname from lat/long to x/y because it's now transformed
colnames(alldat@coords)<-c("x", "y")

#convert back to dataframe from spatial points
alldatdf<-as.data.frame(alldat)

crane.names<-as.list(unique(alldat$id))
testdat<-alldatdf

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
dat.summary<-na.omit(dat.summary)
table(dat.summary$id)  #this didn't work at all for just Bagley for some reason
apply(is.na(dat.summary), 2, sum)
#############################################################################################
#I calculated the roost locations for Bagley separately and I'll just take out the NA's and 
#rbind the correct obs back to the database (I ran the loop just for Bagley and it worked, ?!?!)

#take out pop and gender from bag.df
str(bag.df)
bag.df<-bag.df[,c(1:8,11)]
dat.summary<-rbind(dat.summary, bag.df)  #back in business with bagley working

#After looking at the raw observation data for Bagley, there is another (the same) issue:
# It took locations continuously, so everything gets filtered out in the next step because
#I took out the hours during the middle of the night
############################################################################################
#there are some rows where the first and last locations weren't actually
# at sunrise or sunset, so I'll subset to only keep data in the right time frame

table(dat.summary$Hour)           #this shows it as well

# I looked up when the possible sunrise/sunset times are for Bemidji, MN and cut out the times that aren't 
# close to the actual sunrise/sunset times
dat.test<-dat.summary[which(dat.summary$Hour>=5 &dat.summary$Hour<=7|dat.summary$Hour>=19&dat.summary$Hour<=21),]
table(dat.test$Hour)
table(bag.df$Hour)
table(dat.test$id)
dat.test<-rbind(dat.test, bag.df)  #had to add back in because of (see above)
########################################################################
#need to convert from utms to lat lon to map with google maps
library(rgdal)

#This is assuming you start with a dataframe that you have projected cartesian coordinates for
#First project to a spatialpoints data frame
dat.sp<-dat.test
coordinates(dat.sp)<-c("x","y")

#Then define the projection to the one that was used
proj4string(dat.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                           +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

#This was check that it worked
proj4string(dat.sp)

#Then reproject to WGS84 so you can get the lat/lon
test.sp<-spTransform(dat.sp, CRS("+proj=longlat +datum=WGS84"))

#Convert to dataframe to get the coordinates in a dataframe
test.df<-as.data.frame(test.sp)
str(test.df)  #looks ok
fall.df<-test.df
colnames(fall.df)[8:9]<-c("lon", "lat")
head(fall.df)

#add utms back as well
fall.df<-cbind(fall.df, dat.test[,c("x","y")])

#save this dataframe to file
write.csv(x=fall.df, file="fall16roosts.csv")
####################################################################################################
#Buffer around each point, overlay polygons, determine overlap of polygons from each population?
library(sp)
library(rgeos)
fall.sp<-fall.df

#create SPDF
coordinates(fall.sp)<-c("x","y")
#Then define the projection to the one that was used
proj4string(fall.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                           +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
#Buffer each point
buff5km<-gBuffer(fall.sp, width=5000)  #buffer 5km around each point
plot(buff5km)

#I'll split up by population first and then buffer and overlap to only get intersection on staging areas
str(fall.df)

############################# Add the population, gender, and age to the dataframe##########
str(fall16roosts)
fall.df<-fall16roosts
sort(unique(fall.df$id))
#assign age
fall.df$age<-NULL
adult.list<-c("0J (Waubun adult #2)", "0K Deglman #1","1J (Santiago #3)" ,
              "2C (Melby adult)" , "2M (Bagley)" , "3C (Larson adult #2)" ,
              "3E (Wambach adult)" , "3K (Rasmussen adult)",  "3M  (Callaway adult)" ,
              "4E Larson adult #1" , "4J (Santiago #1)",  "4K (Waubun #1)" ,  "4M (Ogema adult)" ,
              "5J (Deglman #3)" , "5M (Beaulieu adult)" , "6E (Linden-Kologi)", "8K (Deglman #2)" ,
              "8M (Stockyard adult)", "9A  (Santer)" , "9C (Helliksen adult April 2015)",
              "9K (Santiago #2)" ,"9M (Inman adult)" )

fall.df<-mutate(fall.df, age=ifelse(id%in%adult.list, "adult", "colt"))  

#assign population
MCP.list<-c("2A (Helliksen July 2015 colt)" ,"2E Beaulieu colt" ,"3E (Wambach adult)",
            "4K (Waubun #1)" ,"4M (Ogema adult)" ,"5K (Helliksen July adult 2015)",
            "5M (Beaulieu adult)","6E (Linden-Kologi)" ,
            "7C (Helliksen colt 2014)" ,"9C (Helliksen adult April 2015)","9J  (Waubun colt)")
fall.df$pop<-NULL
fall.df<-mutate(fall.df, pop=ifelse(id%in%MCP.list, "MCP", "EP"))

#assign gender
male.list<-c("3A  (Barb Wire)","2E Beaulieu colt" ,"3M  (Callaway adult)",
             "7M (E of Mud Lake colt)", "7A  (Hackensack)", "7C (Helliksen colt 2014)",
             "2A (Helliksen July 2015 colt)" ,"9C (Helliksen adult April 2015)",
             "5K (Helliksen July adult 2015)", "9M (Inman adult)","1A (Inman colt #2)" ,
             "3C (Larson adult #2)" ,  "7K (Larson colt #1)" , "6E (Linden-Kologi)",
             "2C (Melby adult)", "1K (Melby colt #2)","1M (Min/Maint Rd)",
             "7E (Nora Township colt)" ,"6A (Pelican Rapids colt)","3K (Rasmussen adult)",
             "6C (Rice Lake colt 2014)" , "3E (Wambach adult)" ,"9J  (Waubun colt)", "0J (Waubun adult #2)")
fall.df$gender<-NULL
fall.df<-mutate(fall.df, gender=ifelse(id%in%male.list, "male", "female"))

#save again
write.csv(x=fall.df, file="fall.df.csv")

#export as a shapefile of points as well
fall.sp<-fall.df
coordinates(fall.sp)<-c("x", "y")
proj4string(fall.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
# writeOGR(obj=fall.sp, dsn="overlap_polys16", layer="fall16_roost_pts_all", driver="ESRI Shapefile")

############################################################################
#split df into 2 for each pop, buffer each roost site locations,
#find overlap (intersection of polygons), look at the result?

ep.fall<-fall.df[fall.df$pop=="EP",]
mcp.fall<-fall.df[fall.df$pop=="MCP",]
ep.sp<-ep.fall
mcp.sp<-mcp.fall
#create SPDF
coordinates(ep.sp)<-c("x","y")
coordinates(mcp.sp)<-c("x","y")
#Then define the projection to the one that was used
proj4string(ep.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
proj4string(mcp.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
#Buffer each point
ep.buff<-gBuffer(ep.sp, width=5000)  #5km
mcp.buff<-gBuffer(mcp.sp, width=5000) #5km
plot(ep.buff, col="red")
plot(mcp.buff, col="blue", add=T)

both.pop<-gIntersection(ep.buff, mcp.buff)
plot(both.pop, col="brown", add=T)

#turn SpatialPolygons to SpatialPolygonsDataFrame so it can be exported as a shapefile
#Create a dataframe and display default rownames
buff.df<-data.frame(ID=1:length(both.pop))
both.pop<- SpatialPolygonsDataFrame(both.pop, buff.df) 

#export as shapefile
# writeOGR(obj=both.pop, dsn="overlap_polys16", layer="fall_pop_overlap16_no_2flyway_cranes", driver="ESRI Shapefile")
#this is the same as the previous shapefile (before I corrected to have Bagley in there), so
#I wont rerun everything after this as well.
######################################################################################
#Now change pop affiliation to reflect the 4 cranes that used both flyways
t<-fall.df                      #don't want to mess up df until I know it worked
sort(unique(fall.df$id))
both.list<-c("4E Larson adult #1", "4M (Ogema adult)",
             "5C (Stockyard colt)", "8M (Stockyard adult)")
ind<-which(t$id%in%both.list)         #index of which row #'s correspond to the 4 "both" cranes
t$pop[ind]<-"Both"                    #assign different population value to these cranes observations
fall.df<-t                            #it worked, so change back to real dataframe

#Buffer and export shapefile for cranes that used both flyways
fly2<-fall.df[fall.df$pop=="Both",]
coordinates(fly2)<-c("x","y")
proj4string(fly2)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
fly2.buff<-gBuffer(fly2, width = 5000)
plot(fly2.buff, col="green", add=T)
fly2.df<-data.frame(ID=1:length(fly2.buff))

fly2.buff<-SpatialPolygonsDataFrame(fly2.buff, fly2.df)  #error (row.names of df and polygon ID's don't match)
#extract polygon ID
fly2id<-sapply(slot(fly2.buff, "polygons"), function(x)slot(x, "ID"))
fly2.df<-data.frame(ID=1:length(fly2.buff), row.names=fly2id)
fly2.buff<-SpatialPolygonsDataFrame(fly2.buff, fly2.df)  #now it works
writeOGR(obj=fly2.buff, dsn="overlap_polys16", layer="fall_16_2flyway_cranes", driver="ESRI Shapefile")

######################################################################################################
#since I'm at it, might as well do colts v adults, and males vs females
#colt/adult split
colt.df<-fall.df[fall.df$age=="colt",]
colt.sp<-colt.df
adult.df<-fall.df[fall.df$age=="adult",]
adult.sp<-adult.df

#convert and project to spatial
coordinates(colt.sp)<-c("x","y")
coordinates(adult.sp)<-c("x","y")
proj4string(colt.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
proj4string(adult.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
#buffer 
colt.buff<-gBuffer(colt.sp, width=5000)
adult.buff<-gBuffer(adult.sp, width=5000)
plot(colt.buff, col="red")
plot(adult.buff, col="blue", add=T)

#intersection
age.int<-gIntersection(colt.buff, adult.buff)
plot(age.int, col="green", add=T)

#convert to SpatialPolygonDataFrames and export as shapefiles
#colts
coltid<-sapply(slot(colt.buff, "polygons"), function(x)slot(x, "ID"))
colt.sp.df<-data.frame(ID=1:length(colt.buff), row.names=coltid)
colt.buff<-SpatialPolygonsDataFrame(colt.buff, colt.sp.df)
writeOGR(obj=colt.buff, dsn="overlap_polys16/2016 colt vs adult polygons", layer="colt_roosts16", driver="ESRI Shapefile")
#adults
adultid<-sapply(slot(adult.buff, "polygons"), function(x)slot(x, "ID"))
adult.sp.df<-data.frame(ID=1:length(adult.buff), row.names=adultid)
adult.buff<-SpatialPolygonsDataFrame(adult.buff, adult.sp.df)
writeOGR(obj=adult.buff, dsn="overlap_polys16/2016 male vs adult polygons", layer="adult_roosts16", driver="ESRI Shapefile")
###############################################################
#Now for males vs females
male.df<-fall.df[fall.df$gender=="male",]
male.sp<-male.df
female.df<-fall.df[fall.df$gender=="female",]
female.sp<-female.df

#convert and project to spatial
coordinates(male.sp)<-c("x","y")
coordinates(female.sp)<-c("x","y")
proj4string(male.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
proj4string(female.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                           +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
#buffer 
male.buff<-gBuffer(male.sp, width=5000)
female.buff<-gBuffer(female.sp, width=5000)
plot(male.buff, col="red")
plot(female.buff, col="blue", add=T)

#intersection
gender.int<-gIntersection(male.buff, female.buff)
plot(gender.int, col="pink", add=T)

#convert to SpatialPolygonDataFrames and export as shapefiles
#males
maleid<-sapply(slot(male.buff, "polygons"), function(x)slot(x, "ID"))
male.sp.df<-data.frame(ID=1:length(male.buff), row.names=maleid)
male.buff<-SpatialPolygonsDataFrame(male.buff, male.sp.df)
writeOGR(obj=male.buff, dsn="overlap_polys16/2016 male vs female polygons", layer="male_roosts16", driver="ESRI Shapefile")
#females
femaleid<-sapply(slot(female.buff, "polygons"), function(x)slot(x, "ID"))
female.sp.df<-data.frame(ID=1:length(female.buff), row.names=femaleid)
female.buff<-SpatialPolygonsDataFrame(female.buff, female.sp.df)
writeOGR(obj=female.buff, dsn="overlap_polys16/2016 male vs female polygons", layer="female_roosts16", driver="ESRI Shapefile")



#Check some maps
library(ggmap)
map<-get_map("Minnesota", zoom=6)
ggmap(map)

p<-ggmap(map)
r<-p+geom_point(data=fall.df, aes(x=lon, y=lat, colour=pop))+
  geom_path(data=fall.df, aes(x=lon, y=lat, colour=pop))
r+ggtitle("Movement by Population from Aug1-Oct1 2016")

#probably just easier to make maps in ArcMap instead of R

####################################################################################
#Now extract points that fall into the overlap polygons to see how many cranes used them and for how long
library(rgdal)
library(sp)
library(rgeos)
#read back in the points dataframe and the polygons
# fall.df <- read.csv("~/R/CraneMovement/scripts/post-December 2016/2016 overlap/fall.df.csv")
# both.pop<-readOGR(dsn ="overlap_polys16", layer = "fall_pop_overlap16_no_2flyway_cranes")

#turn points layer into spatial first
fall.sp<-fall.df
coordinates(fall.sp)<-c("x", "y")
proj2string(fall.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=25.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

#project the polygon layer into the same projection
both.pop<-spTransform(both.pop, CRS("+proj=aea +lat_1=29.5 +lat_2=25.5 +lat_0=37.5
                                    +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
# proj2string(fall.sp)    #check them
# proj2string(both.pop)   #check them

#extract points within the overlap polygons
over.pts<-over(fall.sp, both.pop)
ind<-which(!is.na(over.pts))
tot.overlap<-fall.df[ind,]  #this is a df of all the points that fall into any of the 3 overlap regions

#split overlap SpatialPolygonsDataFrame into each polygon and then extract points that fall in each one
test<-SpatialPolygons(both.pop@polygons, proj2string = both.pop@proj2string) #from spdf to spatialPolygon
ptest<-disaggregate(test)   #from list of 1 to list of 2
poly.list<-both.pop@polygons[[1]]@Polygons  #changed from spdf to list of polygons

# I can't figure out how to spit the spatialpolygonsdataframe into four separate ones, so I'll just
# write out to arcmap and do it there and bring it back to R
# singles<-readOGR(dsn ="overlap_polys16/2016 pops single part features", layer = "pop_overlap_singles")
#not sure this helps

#I exported 2 separate shapefiles to address the issue:
#The 2016 staing areas are named 1-2 as: 1) waubun, 2) carpenters corner, 3) red lake res, 2) agassiz
stage1<-readOGR(dsn ="overlap_polys16/separate_staging_shps", layer = "stage1")
stage2<-readOGR(dsn ="overlap_polys16/separate_staging_shps", layer = "stage2")
stage3<-readOGR(dsn ="overlap_polys16/separate_staging_shps", layer = "stage3")
stage2<-readOGR(dsn ="overlap_polys16/separate_staging_shps", layer = "stage2")

#Now extract the points that fell in each staging area
fall.sp<-spTransform(fall.sp, CRS("+proj=aea +lat_1=29.5 +lat_2=25.5 +lat_0=37.5
                                  +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs82=0,0,0"))

#extract crane observations from periods of overlap
st1<-over(fall.sp, stage1)
ind1<-which(!is.na(st1$ID))
st1.pts<-fall.df[ind1,]
st1.pts<-droplevels(st1.pts)
summary(st1.pts)
#this gives overlap, but doesn't explicity say when cranes from diff pop's were there at the same time...
#I'll run a loop to determine which days have multiple cranes, and 
#also which days have cranes from different populations present simultaneously
library(gdata)
dat.summary.1<-NULL
tempdat<-NULL
pops<-c("EP", "MCP")

st1alt<-st1.pts[order(st1.pts$day, st1.pts$id),]
st1days<-as.list(unique(st1alt$day))
days.vec<-sort(unlist(st1days))
firstday<-days.vec[1]
lastday<-last(days.vec)
for(i in firstday:lastday){
  tempdat<-st1alt[which(st1alt$day==i),]
  if(length(unique(tempdat$id))>1)
  {tempdat$mult<-"T"}else{tempdat$mult<-"F"}
  ifelse(pops%in%tempdat$pop,tempdat$pop.over<-"T",tempdat$pop.over<-"F")
  dat.summary.1<-rbind(dat.summary.1, tempdat)
}

#might want to calculate the duration per crane
aggregate(dat.summary.1$day, by=list(id=dat.summary.1$id), FUN=length)


#df of days when multiple cranes were present on a staging area
st1mult<-dat.summary.1[dat.summary.1$mult=="T",]
length(unique(st1mult$day))                           #56 days

#df of days that multiple cranes from different population were there
st1pov<-dat.summary.1[dat.summary.1$pop.over=="T",]
length(unique(st1pov$day))                             #53 days


#for the second staging area
st2<-over(fall.sp, stage2)
ind2<-which(!is.na(st2$ID))
st2.pts<-fall.df[ind2,]
st2.pts<-droplevels(st2.pts)
summary(st2.pts)

dat.summary.2<-NULL
tempdat<-NULL

st2alt<-st2.pts[order(st2.pts$day, st2.pts$id),]
st2days<-as.list(unique(st2alt$day))
days.vec<-sort(unlist(st2days))
firstday<-days.vec[1]
lastday<-last(days.vec)
for(i in firstday:lastday){
  tempdat<-st2alt[which(st2alt$day==i),]
  if(length(unique(tempdat$id))>1)
  {tempdat$mult<-"T"}else{tempdat$mult<-"F"}
  ifelse(pops%in%tempdat$pop,tempdat$pop.over<-"T",tempdat$pop.over<-"F")
  dat.summary.2<-rbind(dat.summary.2, tempdat)
}

#might want to calculate the duration per crane
aggregate(dat.summary.2$day, by=list(id=dat.summary.2$id), FUN=length)

#df of days when multiple cranes were present on a staging area
st2mult<-dat.summary.2[dat.summary.2$mult=="T",]
length(unique(st2mult$day))                           #29 days

#df of days that multiple cranes from different population were there
st2pov<-dat.summary.2[dat.summary.2$pop.over=="T",]
length(unique(st2pov$day))                             #21 days


#the third
st3<-over(fall.sp, stage3)
ind3<-which(!is.na(st3$ID))
st3.pts<-fall.df[ind3,]
st3.pts<-droplevels(st3.pts)
summary(st3.pts)

dat.summary.3<-NULL
tempdat<-NULL

st3alt<-st3.pts[order(st3.pts$day, st3.pts$id),]
st3days<-as.list(unique(st3alt$day))
days.vec<-sort(unlist(st3days))
firstday<-days.vec[1]
lastday<-last(days.vec)
for(i in firstday:lastday){
  tempdat<-st3alt[which(st3alt$day==i),]
  if(nrow(tempdat)>0){
  tempdat$mult<-ifelse(length(unique(tempdat$id))>1,"T","F")
  if(length(unique(tempdat$id))>1)
  {tempdat$mult<-"T"}else{tempdat$mult<-"F"}
  ifelse(pops%in%tempdat$pop,tempdat$pop.over<-"T",tempdat$pop.over<-"F")
    dat.summary.3<-rbind(dat.summary.3, tempdat)
}
}

#might want to calculate the duration per crane
aggregate(dat.summary.3$day, by=list(id=dat.summary.3$id), FUN=length)

#df of days when multiple cranes were present on a staging area
st3mult<-dat.summary.3[dat.summary.3$mult=="T",]
length(unique(st3mult$day))                           #24 days

#df of days that multiple cranes from different population were there
st3pov<-dat.summary.3[dat.summary.3$pop.over=="T",]
length(unique(st3pov$day))                             #13 days


#the fourth
st4<-over(fall.sp, stage4)
ind4<-which(!is.na(st4$ID))
st4.pts<-fall.df[ind4,]
st4.pts<-droplevels(st4.pts)
summary(st4.pts)

dat.summary.4<-NULL
tempdat<-NULL

st4alt<-st4.pts[order(st4.pts$day, st4.pts$id),]
st4days<-as.list(unique(st4alt$day))
days.vec<-sort(unlist(st4days))
firstday<-days.vec[1]
lastday<-last(days.vec)
for(i in firstday:lastday){
  tempdat<-st4alt[which(st4alt$day==i),]
  if(nrow(tempdat)>0){
  if(length(unique(tempdat$id))>1)
  {tempdat$mult<-"T"}else{tempdat$mult<-"F"}
  ifelse(pops%in%tempdat$pop,tempdat$pop.over<-"T",tempdat$pop.over<-"F")
  dat.summary.4<-rbind(dat.summary.4, tempdat)
}
}


#might want to calculate the duration per crane
aggregate(dat.summary.4$day, by=list(id=dat.summary.4$id), FUN=length)

#df of days when multiple cranes were present on a staging area
st4mult<-dat.summary.4[dat.summary.4$mult=="T",]
length(unique(st4mult$day))                           #0 days

#df of days that multiple cranes from different population were there
st4pov<-dat.summary.4[dat.summary.4$pop.over=="T",]
length(unique(st4pov$day))                             #0 days

#Combine all dataframes together for export
dat.summary.1$area<-"1"
dat.summary.2$area<-"2"
dat.summary.3$area<-"3"
dat.summary.4$area<-"4"
overlap.df<-rbind(dat.summary.1, dat.summary.2, dat.summary.3, dat.summary.4)

stage.summary<-aggregate(overlap.df$day, by=list(id=overlap.df$id, pop=overlap.df$pop, age=overlap.df$age, gender=overlap.df$gender), FUN=length)
stage.summary[order(stage.summary$pop, stage.summary$x),]    #**run this**##
#so 14 cranes were in the overlapped staging areas, and here are the number of days each spend in any one of them.
#see other number above for the temporal overlap summary statistics


# write.csv(x=overlap.df, file="overlap.df.csv")


?aggregate







