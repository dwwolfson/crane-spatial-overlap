#Fall Staging Space Use 2015

library(dplyr)
library(gdata)
library(sp)

#import data from movebank
#these are locations from all active cranes from August 1-October 1st 2016
fall15<-read.csv("~/R/CraneMovement/scripts/post-December 2016/2015 overlap/raw_aug1_oct1_2015.csv")
alldat<-fall15  #so that I can use script already set up for a different name

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
table(dat.summary$id)  #looks good
apply(is.na(dat.summary), 2, sum)

############################################################################################
#there are some rows where the first and last locations weren't actually
# at sunrise or sunset, so I'll subset to only keep data in the right time frame
table(dat.summary$Hour)           #this shows it as well
# I looked up when the possible sunrise/sunset times are for Bemidji, MN and cut out the times that aren't 
# close to the actual sunrise/sunset times 
#I also left in the loc's taken at hour 23 and 0/24, because those are almost certainly from cranes 
#that were taking locations all night (thanks a lot Andrew :( ) and so are actually reflecting roost sites

dat.test<-dat.summary[which(dat.summary$Hour>=5 &dat.summary$Hour<=7|dat.summary$Hour>=19&dat.summary$Hour<=21|
                              dat.summary$Hour==23|dat.summary$Hour==0),]
table(dat.test$Hour)
table(dat.test$id)
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
####################################################################################################
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
write.csv(x=fall.df, file="fall15.df.csv")

#export as a shapefile of points as well
fall.sp<-fall.df
coordinates(fall.sp)<-c("x", "y")
proj4string(fall.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
# writeOGR(obj=fall.sp, dsn="overlap_polys15", layer="fall15_roost_pts_all", driver="ESRI Shapefile")
##############################################################################################################
#split df into 2 for each pop, buffer each roost site locations,
#find overlap (intersection of polygons), look at the result

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
library(rgeos)
#Buffer each point
ep.buff<-gBuffer(ep.sp, width=3000)  #3km
mcp.buff<-gBuffer(mcp.sp, width=3000) #3km
plot(ep.buff, col="red" )
plot(mcp.buff, col="blue", add=T)

both.pop<-gIntersection(ep.buff, mcp.buff)
plot(both.pop, col="brown")

#turn SpatialPolygons to SpatialPolygonsDataFrame so it can be exported as a shapefile
#Create a dataframe and display default rownames
buff.df<-data.frame(ID=1:length(both.pop))
both.pop<- SpatialPolygonsDataFrame(both.pop, buff.df)

#export as shapefile
writeOGR(obj=both.pop, dsn="2015 overlap/overlap_polys15/pops", layer="fall_pop_overlap15_no_2flyway_cranes", driver="ESRI Shapefile")
############################################################################################Now extract points that fall into the overlap polygons to see how many cranes used them and for how long
library(rgdal)
library(sp)
library(rgeos)
#read back in the points dataframe and the polygons
# fall.df <- read.csv("~/R/CraneMovement/scripts/post-December 2016/2016 overlap/fall.df.csv")
# both.pop<-readOGR(dsn ="overlap_polys16", layer = "fall_pop_overlap16_no_2flyway_cranes")

#turn points layer into spatial first
fall.sp<-fall15.df
fall.df<-fall15.df
coordinates(fall.sp)<-c("x", "y")
proj4string(fall.sp)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                          +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

#project the polygon layer into the same projection
both.pop<-spTransform(both.pop, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                                    +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
# proj4string(fall.sp)    #check them
# proj4string(both.pop)   #check them

#extract points within the overlap polygons
over.pts<-over(fall.sp, both.pop)
ind<-which(!is.na(over.pts))
tot.overlap<-fall.df[ind,]  #this is a df of all the points that fall into any of the 3 overlap regions

#split overlap SpatialPolygonsDataFrame into each polygon and then extract points that fall in each one
test<-SpatialPolygons(both.pop@polygons, proj4string = both.pop@proj4string) #from spdf to spatialPolygon
ptest<-disaggregate(test)   #from list of 1 to list of 4
poly.list<-both.pop@polygons[[1]]@Polygons  #changed from spdf to list of polygons

# I can't figure out how to spit the spatialpolygonsdataframe into four separate ones, so I'll just
# write out to arcmap and do it there and bring it back to R
# singles<-readOGR(dsn ="overlap_polys16/2016 pops single part features", layer = "pop_overlap_singles")
#not sure this helps

#I exported 4 separate shapefiles to address the issue:
#The 2016 staing areas are named 1-4 as: 1) waubun, 2) carpenters corner, 3) red lake res, 4) agassiz
2015 overlap\overlap_polys15\separate_staging_shps
stage1<-readOGR(dsn ="2015 overlap/overlap_polys15/separate_staging_shps",
                layer = "stage1_waubun")
stage2<-readOGR(dsn ="2015 overlap/overlap_polys15/separate_staging_shps", layer = "stage2_red_lake")
stage3<-readOGR(dsn ="2015 overlap/overlap_polys15/separate_staging_shps", layer = "stage3_agassiz")

#Now extract the points that fell in each staging area
fall.sp<-spTransform(fall.sp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                                  +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#extract crane observations from periods of overlap
st1<-over(fall.sp, stage1)
ind1<-which(!is.na(st1$ID))
st1.pts<-fall.df[ind1,]
st1.pts<-droplevels(st1.pts)
summary(st1.pts)
#this gives overlap, but doesn't explicity say when cranes from diff pop's were there at the same time...
#I'll run a loop to determine which days have multiple cranes, and 
#also which days have cranes from different populations present simulataneously
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
aggregate(dat.summary.1$day, by=list(id=dat.summary.1$id, 
        pop=dat.summary.1$pop), FUN=function(x) (length(unique(x))))


#df of days when multiple cranes were present on waubun
st1mult<-dat.summary.1[dat.summary.1$mult=="T",]
length(unique(st1mult$day))                           #52 days

#df of days that multiple cranes from different population were on waubun
st1pov<-dat.summary.1[dat.summary.1$pop.over=="T",]
length(unique(st1pov$day))                             #50 days
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#the following two measures are a bit misleading because the two waubun adults 
#were both still on their breeding territory during this time   
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#pull out waubun adults
st1sub<-dat.summary.1[dat.summary.1$id!="0J (Waubun adult #2)"&
                           dat.summary.1$id!="4K (Waubun #1)",]

aggregate(st1sub$day, by=list(id=st1sub$id, 
                  pop=st1sub$pop), FUN=function(x) (length(unique(x))))

#df of days when multiple cranes were present on waubun
sub1mult<-st1sub[st1sub$mult=="T",]
length(unique(sub1mult$day))                           #19 days

#df of days that multiple cranes from different population were on waubun
sub1pov<-st1sub[st1sub$pop.over=="T",]
length(unique(sub1pov$day)) 

##############################################################################
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
  if(nrow(tempdat)>0){
  if(length(unique(tempdat$id))>1)
  {tempdat$mult<-"T"}else{tempdat$mult<-"F"}
  ifelse(pops%in%tempdat$pop,tempdat$pop.over<-"T",tempdat$pop.over<-"F")
  dat.summary.2<-rbind(dat.summary.2, tempdat)
  }
}

#might want to calculate the duration per crane at red lake
aggregate(dat.summary.2$day, by=list(id=dat.summary.2$id,
        pop=dat.summary.2$pop), FUN=function(x) (length(unique(x))))

#df of days when multiple cranes were present at red lake
st2mult<-dat.summary.2[dat.summary.2$mult=="T",]
length(unique(st2mult$day))                           #20 days

#df of days that multiple cranes from different population were there
st2pov<-dat.summary.2[dat.summary.2$pop.over=="T",]
length(unique(st2pov$day))                             #18 days


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

#might want to calculate the duration per crane at aggasiz
aggregate(dat.summary.3$day, by=list(id=dat.summary.3$id,
            pop=dat.summary.3$pop), FUN=function(x) (length(unique(x))))

#df of days when multiple cranes were present on a staging area
st3mult<-dat.summary.3[dat.summary.3$mult=="T",]
length(unique(st3mult$day))                           #9 days

#df of days that multiple cranes from different population were there
st3pov<-dat.summary.3[dat.summary.3$pop.over=="T",]
length(unique(st3pov$day))                             #9 days

#Combine all dataframes together for export
dat.summary.1$area<-"1"
dat.summary.2$area<-"2"
dat.summary.3$area<-"3"
overlap15.df<-rbind(dat.summary.1, dat.summary.2, dat.summary.3)

stage.summary15<-aggregate(overlap15.df$day, by=list(id=overlap15.df$id, pop=overlap15.df$pop, age=overlap15.df$age, gender=overlap15.df$gender), FUN=function(x) (length(unique(x))))

stage.summary15[order(stage.summary15$pop, stage.summary15$x),]    

#maybe pull out both waubun adults because they weren't traveling to a
#separate staging area
over15.sub<-overlap15.df[overlap15.df$id!="0J (Waubun adult #2)"&
                           overlap15.df$id!="4K (Waubun #1)",]

aggregate(over15.sub$day, by=list(id=over15.sub))



stage.sub15<-aggregate(over15.sub$day, by=list(id=over15.sub$id,pop=over15.sub$pop, 
  age=over15.sub$age, gender=over15.sub$gender), FUN=function(x) (length(unique(x))))

write.csv(x=stage.sub15, file="stage.sub15.csv")
write.csv(x=overlap15.df, file="overlap15.df.csv")






