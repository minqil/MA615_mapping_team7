library(leaflet)
library(maps)
library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(sf)
library(tmap)

#fips match location
uscounty=st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2]=colnames(uscounty)[1]
uscounty=left_join(uscounty,county.fips,by='ID')

#floyd rain
rain_floyd=rain %>% filter(storm_id=='Floyd-1999')
total_rain_floyd = rain_floyd %>% group_by(fips) %>% summarise(storm_id=storm_id[1],precip=sum(precip)) 
total_rain_floyd = total_rain_floyd %>% mutate(fips=as.numeric(fips))
total_rain_floyd= right_join(uscounty,total_rain_floyd,'fips')

#floyd track line
hurr_floyd=hurr_tracks%>%filter(storm_id=="Floyd-1999")
track_floyd=cbind(longitude=hurr_floyd$longitude,latitude=hurr_floyd$latitude)
#the following codes refer to other's 
track_floyd=SpatialLines(list(Lines(Line(track_floyd),ID='Floyd-1999')))


#plot
tm_shape(total_rain_floyd)+
  tm_polygons('precip',title="Rainfall")+
  tm_layout(main.title="Floyd-1999 Rainfall",
            main.title.position="center") +
tm_shape(track_floyd)+
  tm_lines(col = "red",lwd = 3)+
  tmap_options(col)

  
