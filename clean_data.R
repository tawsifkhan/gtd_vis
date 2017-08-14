library(dplyr)

df <- read.csv('./gtd.csv')
df2 <- df
df<-df2
cols <- c('iyear','imonth','iday','longitude','latitude','country_txt','country','region_txt','region','nkill','nwound','weaptype1_txt','weaptype1')
df <- df[cols]
df <- subset(df,df$nkill>0 | df$nwound>0)

unique(df[c('weaptype1_txt','weaptype1')])

df$weaptype1_txt <- as.character(df$weaptype1_txt)
df$weaptype1_txt[df$weaptype1==10] <- "Vehicle"
df$weaptype1_txt[df$weaptype1==6] <- "Explosives"
df$weaptype1_txt[df$weaptype1==11] <- "Sabotage"


unique(df[c('country_txt','country')])
df$country_txt <- as.character(df$country_txt)
df$country_txt[df$country==229 | df$country==47] <- "Congo"

write.csv(df,file='./mygtd.csv')
