library(tigris)
library(GISTools)
library(dplyr)
setwd("C:/Users/Steve/Dropbox/MyCourses/BST_5930_DataVisualization/Maps")

###############################################################
####  tigris package
###############################################################

pa = counties("Pennsylvania")    ### tigris package

plot( pa )

PA_FIPS = pa[[4]]
counties_list_tigris = pa[[5]]
pa[[4]]
lat_cent = pa[[16]]
long_cent = pa[[17]]

plot( pa )
points(long_cent,lat_cent)

seed(12345)
out = rnorm(67,0,1)
choropleth( pa , out )

PA_Unemployment = read.csv("./Homework5/PA_Unemployment.csv")

counties_list_tigris
PA_Unemployment$CountyName

windows( 12 , 8 )
shades = shading( c(4.0,5.0,6.0) , cols=brewer.pal(4,"Blues"))
choropleth( pa , PA_Unemployment$UERate , shades , main="WRONG")  ## WRONG!
choro.legend( -79 , 42.6 , shades , title="Unemployment Rate" , cex=0.9 )

CoFIPS = PA_Unemployment$CountyFIPS
CoName = PA_Unemployment$CountyName
UERate = PA_Unemployment$UERate

df1 = data.frame( CoFIPS , CoName , UERate , counties_list_tigris )
df2 = arrange( df1 , counties_list_tigris )
df3 = data.frame( df2 , UERate )
df4 = arrange( df3 , CoFIPS )

UERate_Corrected = df4$UERate.1

windows( 12 , 8 )
shades = shading( c(4.0,5.0,6.0) , cols=brewer.pal(4,"Blues"))
choropleth( pa , UERate_Corrected , shades , main="Correct")  ## RIGHT!
choro.legend( -79 , 42.6 , shades , title="Unemployment Rate" , cex=0.9 )



# Miao's revised code
t = PA_Unemployment %>%
  dplyr::select(CountyFIPS, CountyName, UERate) %>%
  mutate(counties_list_tigris = counties_list_tigris) %>%
  arrange(counties_list_tigris) %>%
  mutate(UE_corrected = PA_Unemployment$UERate) %>%
  arrange(CountyFIPS) %>%
  dplyr::select(UE_corrected) %>%
  unlist()

choropleth( pa , UERate_Corrected , shades , main="zz")  ## RIGHT!
choropleth( pa , t , shades , main="z111z")  ## RIGHT!



