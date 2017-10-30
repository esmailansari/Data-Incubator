# time series analysis of voluem and seismic data

rm(list=ls())
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(reshape2)
require(zoo)
require(xts)
require(hydroTSM)
require(lubridate)

#----------------------------------------------
#   Earthquake data for oklahoma and kansas
#----------------------------------------------
seismic_data <- read.csv("./Oklahoma_Kansas_USGS.csv")
seis_data <- seismic_data %>% filter(str_detect(place, 'Kansas|Oklahoma')) %>%
  mutate(date=as.Date(time))%>%
  mutate(year=format(date,'%Y')) %>%
  select(c('date','mag','depth'))
  #%>% filter(date >= as.Date("2010-01-01"))

seis_zoo <- read.zoo(seis_data)

#because the index is per day, it takes the mean of the seismicity in day
seis_zoo <- aggregate(seis_zoo, index(seis_zoo), mean)

seis_xts <- xts(seis_zoo)

seis_r      <- izoo2rzoo(seis_zoo, tstep= "days")
#seis_filled <- na.approx(seis_r)
seis_filled <- na.fill(seis_r, fill=0.00)
seis_ts  <- ts(seis_filled,start=c(year(start(seis_zoo)),as.numeric(format(start(seis_zoo),"%j"))),
               end  =c(year(end(seis_zoo)),as.numeric(format(end(seis_zoo),"%j"))),frequency=365)


#----------------------------------------
# counts of the seismic data per months
#----------------------------------------
seismic_data <- read.csv("./Oklahoma_Kansas_USGS.csv")
seis_data <- seismic_data %>% filter(str_detect(place, 'Kansas|Oklahoma')) %>%
  mutate(date=as.Date(time))%>%
  mutate(year=format(date,'%Y')) %>%
  select(c('date','mag','depth'))

seis_counts <- table(format(seis_data$date,"%Y-%m-01"))
counts_df  <- as.data.frame(seis_counts)
counts_df <- counts_df %>% mutate(date=Var1) %>% select(c('date','Freq'))
#counts_df should have a column named "date" to recognize it. 
counts_zoo <-  read.zoo(counts_df)
counts_r  <- izoo2rzoo(counts_zoo, tstep='months')
counts_filled <- na.fill(counts_r, fill=0.00)
counts_xts <- xts(counts_filled)
counts_ts <- ts(counts_filled,start=c(year(start(counts_zoo)),month(start(counts_zoo))),
                end  =c(year(end(counts_zoo)),month(end(counts_zoo))),deltat=1/12)


#-------------------------------------------------------------------
#     showing the seismic counts and injection rate on one plot
#------------------------------------------------------------------
pdf('seismicity_increase.pdf')
plot(counts_zoo,las=1,ylab='Number of Earthquakes',xlab='Year')
dev.off()

# # seismic data
seis_data <- read.csv("./Oklahoma_Kansas_USGS.csv")
seis_data <- seis_data %>% filter(str_detect(place, 'Oklahoma') | 
                                    str_detect(place, 'Kansas'))
usa    <- map_data("usa") # we already did this, but we can do it again
states <- map_data("state")
mid_us <- subset(states, region %in% c("kansas", "oklahoma"))
counties <- map_data("county")
KS_OK_county <- subset(counties, region %in% c("kansas","oklahoma"))


m_base <- ggplot()
m_base <- m_base + geom_polygon(data=mid_us, aes(x = long, y = lat, group = group),
                                colour='red',fill='gray')+ 
  coord_fixed(1.05)

m_base <- m_base + geom_polygon(data=KS_OK_county, 
                                aes(x=long, y=lat, group = group), 
                                colour="white",fill=NA) +
  geom_polygon(data=mid_us, aes(x = long, y = lat, 
                                group = group)
               ,color = "black" 
               ,fill = NA)


m_base <- m_base + geom_jitter(data=seis_data,  aes(x=longitude, y=latitude, 
                                                    size = seis_data$mag, 
                                                    colour=seis_data$mag),
                               label=rownames(seis_data)) + 
  scale_size(range=c(0.05,2),name='magnitude')+
  labs(x='lon', y='lat', 
       size = "magnitude", 
       fill = "magnitude") +
  ggtitle('earthquakes')+theme_nothing()
#+theme(legend.position="none")
print(m_base)

sbbox  <- make_bbox(lon = mid_us$long, lat = mid_us$lat, f = .1)
sq_map <- get_map(location = sbbox,
                  maptype = "terrain", source = "google")
sq_map <- ggmap(sq_map) + geom_jitter(data = seis_data, 
                                      mapping = aes(x = longitude, y = latitude, size = seis_data$mag, 
                                                    colour=seis_data$mag))+ scale_size(range=c(0.05,2),name='magnitude')+
  labs(size='magnitude', fill='magnitude')+ 
  coord_fixed(1.05)
#theme(legend.position="none")
print(sq_map)
pdf('Seismicity_KS_OK.pdf')
print(sq_map)
print(m_base)
dev.off()


