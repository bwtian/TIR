source('0_settings.R')

library(plyr)
library(lubridate)

load('1_all_scenes.RData')

# Count scenes
lt20 <- scenes[(scenes$Cloud.Cover <= 20) &
               (scenes$Date.Acquired >= as.Date('1995/1/1')) &
               (scenes$Date.Acquired <= as.Date('2015/12/31')),]
lt20$year <- year(lt20$Date.Acquired)
lt20$year_cat <- cut(lt20$year, c(1995, 2000, 2005, 2010, 2015), include.lowest=TRUE)
lt20$pathrow <- paste(lt20$WRS.Path, lt20$WRS.Row, sep='-')

lt20bysite <- ddply(lt20, .(sitecode, year_cat), summarize,
                    n=length(sitecode),
                    mean_cloud_cover=mean(Cloud.Cover, na.rm=TRUE))

lt20bypathrow <- ddply(lt20, .(year_cat, pathrow), summarize,
                    n=length(sitecode),
                    mean_cloud_cover=mean(Cloud.Cover, na.rm=TRUE), .drop=FALSE)
nrow(lt20)
nrow(lt20bypathrow)
median(lt20bypathrow$n)
table(lt20bypathrow$n)
max(lt20bypathrow$n)
mean(lt20bypathrow$n)
hist(lt20bypathrow$n)


lt20bypathrow[lt20bypathrow$n < 4, ]
