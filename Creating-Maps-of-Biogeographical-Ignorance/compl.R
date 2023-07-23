## Measuring inventory completeness 

# Measuring inventory completeness in 0.5 x 0.5 cells in Namibia territory
## Load libraries
library (dplyr)
library(KnowBR)

## Extract coordinates from sf object
coords = st_coordinates(my_sf2) 

## Convert sf into a df
df <- my_sf2 %>% st_drop_geometry()
df <- cbind(df,coords)

df <- df %>% 
  dplyr::rename(decimalLongitude = X,
                decimalLatitude = Y)

## Put data into knowBr format
df$count<-1 #create a column of frequency

df1<-df[,c("species","decimalLongitude","decimalLatitude","count")]

## Load databases
data("adworld")

## Run the function that calculates inventory completeness
KnowB(df1, cell=30, jpg = T, 
      minLon = 10, maxLon = 27, minLat = -30, maxLat = -14, extent = F)


# Load Namibia shapefile
shp = shapefile ("C:/Users/Utilizador/Downloads/geoBoundaries-NAM-ADM1-all/geoBoundaries-NAM-ADM1_simplified.shp")

# Load world map
data(adworld)

#
KnowBPolygon(data=df1, 
             shape=shp, 
             admAreas=TRUE, 
             shapenames="shapeName", 
             colscale=rev(heat.colors(100)), 
             jpg=FALSE)
 