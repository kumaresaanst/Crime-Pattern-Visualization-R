getwd()
setwd("C:/Users/HP/Documents/R Files")
sfcrime=read.csv(file="train1.csv")
dept = read.csv(file="2018 Batch DB.csv")
View(dept)
View(sfcrime)
factor(sfcrime$Address)
tab1=table(sfcrime$Address)
tab1
df=as.data.frame(tab1)
df
df= tbl_df(df)
tb= arrange(df,desc(Freq))
tab2= table(sfcrime$Category)
df1 = as.data.frame(tab2)
df1=tbl_df(df1)
tb1 = arrange(df1,desc(Freq))
tb1
install.packages("dplyr")
library(dplyr)
fil=filter(sfcrime,sfcrime$Address=="800 Block of BRYANT ST")
View(fil)
install.packages("ggmap")
library(ggmap)
sanfran=get_map(location = 'San Francisco',zoom = 4)
ggmap(sanfran)
locationCrimes <- as.data.frame(table(sfcrime$Longitude, sfcrime$Latitude))
locationCrimes
larceny=filter(sfcrime,sfcrime$Category=="LARCENY/THEFT")
View(larceny)
tab_lar= table(larceny$DayOfWeek)
df_lar_day=as.data.frame(tab_lar)
df_lar_day
lar_fri= filter(sfcrime,sfcrime$Category=="LARCENY/THEFT" & sfcrime$DayOfWeek=="Friday")
fac_address= factor(lar_fri$Address)
other=filter(sfcrime,sfcrime$Category=="OTHER OFFENSES")
View(other)
noarr=filter(sfcrime,sfcrime$Resolution=="NONE")
factor(sfcrime$Category)
tab_cat=table(sfcrime$Category)
df_tab_cat=as.data.frame(tab_cat)
df_tab_cat
none=filter(sfcrime,sfcrime$Resolution=="NONE")
table(none$DayOfWeek)
frid = filter(sfcrime, sfcrime$DayOfWeek=='Friday')
tab=table(frid$Category)
tab1=as.data.frame(tab)
tab1 = tbl_df(tab1)
df2 = arrange(tab1,desc(Freq))
df2
head(df2,10)
drum=filter(sfcrime,sfcrime$Address=="0 Block of DRUMM ST")
drum
drug=filter(sfcrime,sfcrime$Category=="DRUG/NARCOTIC")
arrest= filter(sfcrime,sfcrime$Resolution=="ARREST- BOOKED")
table(sfcrime$DayOfWeek)
install.packages("ggmap")
library(ggmap)
sanfran=get_map(location = 'San Francisco',zoom = 12)
ggmap(sanfran)
locationcrime=as.data.frame(table(lar_fri$X,lar_fri$Y))
names(locationcrime)<-c('long','lat','freq')
locationcrime$long=as.numeric(as.character(locationcrime$long))
locationcrime$lat=as.numeric(as.character(locationcrime$lat))
locationcrime=subset(locationcrime,freq>0)
ggmap(sanfran) + geom_tile(data = locationcrime,aes(x=long,y=lat,alpha =freq),fill='red') + theme(axis.title.y = element_blank(),axis.title.x = element_blank()) 
ggmap(sanfran) +
  geom_point(data=locationcrime, aes(x=long, y=lat, color=factor(drug)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Type of Crime")) +
  scale_colour_brewer(type="qual",palette="Paired") + 
  ggtitle("Top Crimes in San Francisco") +
  theme_light(base_size=20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
install.packages("ggplot2")
library(ggplot2)
ggsave("sf_top_crimes_map.png", p, width=14, height=10, units="in")
coltypes <-list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))
install.packages("lubridate")
library(lubridate)
install.packages("readr")
library(readr)
train <-read_csv(file="train1.csv",col_types=coltypes)
View(train)
mapdata= filter(sfcrime,sfcrime$Category=="LARCENY/THEFT")
mapdata %>%group_by(Category) %>%summarise(n=n())
ggmap(sanfran, extent='device') +
  geom_point(aes(x=X, y=Y, colour=Category), data=lar_fri) +  
  ggtitle('Resolution-Arrested')
s<-sfcrime %>% group_by(Category) %>% group_by(DayOfWeek) %>% summarise(dat = n())
View(s)
sf= larcen %>% group_by(Category)
sf1 = sfcrime %>% group_by(DayOfWeek)
summarise(sf1)
dc = as.table.default(sf)
View(dc)
?POSIXct
sfcrime[,3:4]


