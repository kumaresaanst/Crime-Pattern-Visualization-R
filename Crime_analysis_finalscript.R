# crime analysis project

getwd()
setwd("C:/Users/HP/Documents/R Files")
sfcrime=read.csv(file="train1.csv")
View(sfcrime)
install.packages("dplyr")
library(dplyr)

# Filtering crimes with no resolution

noarr=filter(sfcrime,sfcrime$Resolution=="NONE")
View(noarr)

# MAP Function

install.packages("ggmap")
library(ggmap)
sanfran=get_map(location = 'San Francisco',zoom = 12)
ggmap(sanfran)

# To view visualization of no resolution crimes on San Francisco Map

ggmap(sanfran, extent='device') +
  geom_point(aes(x=X, y=Y, colour=Category), data=noarr) +  
  ggtitle('Resolution-None')


# Finding Top 10 Crimes with no Resolution

tabno= table(noarr$Category)
tabno
df1=as.data.frame(tabno)
df1
tb1 = tbl_df(df1)
tb1
arr1=arrange(tb1,desc(tb1$Freq))
arr1


