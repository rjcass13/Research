dt <- read.csv('agg_1x1_2015_2024_temp_precip.csv')

provo_lat <- 40.5
provo_lon <- 248.5

cin_lat <- 39.5
cin_lon <- 360 - 84.5

quito_lat <- -.5
quito_lon <- 360 - 78.5

sydney_lat <- -33.5
sydney_lon <- 151.5

provo <- dt[which(dt$Lat == provo_lat & dt$Lon == provo_lon), ]
cincinnati <- dt[which(dt$Lat == cin_lat & dt$Lon == cin_lon), ]
quito <- dt[which(dt$Lat == quito_lat & dt$Lon == quito_lon), ]
sydney <- dt[which(dt$Lat == sydney_lat & dt$Lon == sydney_lon), ]

provo$week <- sub(".*-", "", provo$Year_Week)
cincinnati$week <- sub(".*-", "", cincinnati$Year_Week)
quito$week <- sub(".*-", "", quito$Year_Week)
sydney$week <- sub(".*-", "", sydney$Year_Week)

plot(provo$week, provo$Temp)
plot(cincinnati$week, cincinnati$Temp)
plot(quito$week, quito$Temp)
plot(sydney$week, sydney$Temp)

# Look at raw prior to agg: check temperature start/end dates per year
# Check provo temps in raw data: do they look as expected?
# Look for ways to plot things that give me the globe in the back, etc.
### image.plot function from fields library
### library(fields)
### Can then overlay a world map (packages: rworldmap, maps)

# Research question: 
# Model temperature as a function of Lat/Elev/Time
# Look at different splits of the dependents, etc.
# after accounting for latitude and time, is the effect of elevation linear or not
# What is the explanation of why temperatures are colder at higher elevations?
# Think about what might be the physical interpretations of WHY things interact the way they do
### Do a little write-up about all the actual physical features that appear to be important
### Do a little write-up about all the statistical features that appear to be important

