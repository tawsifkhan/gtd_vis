library(dplyr)

df <- read.csv('./gtd.csv')
cols <- c('iyear','imonth','iday','longitude','latitude','country_txt','country','region_txt','region','nkill','nwound','weaptype1_txt','weaptype1')
df <- df[cols]
df <- subset(df,df$nkill>0 | df$nwound>0)

#x_1<-df %>% group_by(iyear,country_txt,country,longitude,latitude,region_txt,region) %>% summarise(nkill=sum(nkill,na.rm=TRUE))
#x_2<-df %>% group_by(iyear,country_txt,country,longitude,latitude,region_txt,region) %>% summarise(wound=sum(nwound,na.rm=TRUE))
#x_3<-df %>% group_by(iyear,country_txt,country,longitude,latitude,region_txt,region) %>% summarise(weaptype1=max(weaptype1,na.rm=TRUE))

weaps<-unique(df[c('weaptype1_txt','weaptype1')])
#x_3 <- merge(x_3,weaps)

df<-Reduce(function(...) merge(..., all=TRUE), list(x_1, x_2, x_3))

df$weaptype1_txt <- as.character(df$weaptype1_txt)
df$weaptype1_txt[df$weaptype1==10] <- "Vehicle"
df$weaptype1_txt[df$weaptype1==6] <- "Explosives"
df$weaptype1_txt[df$weaptype1==11] <- "Sabotage"


unique(df[c('country_txt','country')])
df$country_txt <- as.character(df$country_txt)
df$country_txt[df$country==229 | df$country==47] <- "Congo"

df$country_txt[df$longitude<(-16.62912) & df$longitude>(-16.62915) & df$latitude<28.29157 & df$latitude > 28.29156)] <- 'Spain'

write.csv(df,file='./mygtd.csv')
