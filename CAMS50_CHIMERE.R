
library(RNetCDF)
library(fields)
library(maptools)
library(rgdal)

library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(raster)   
library(rgeos)
library(rgdal)
library(plyr)
library(dplyr)
library(leaflet)
library(sp)
library(htmltools)
library(ncdf4)
library(htmlwidgets)
library(spatialEco)
library(pracma)

setwd ("C:/CAMS_95/R_sample") 
# setwd ("C:/RICARDO-AEA/CAMS_95/R_sample") 
# dir <- "C:/RICARDO-AEA/CAMS_95/R_sample/EU_shp"

# dir <- "C:/CAMS_95/R_sample/EU_shp"
dir <- "C:/CAMS_95/R_sample/France_shp"

### shapefile for local authorities in England
# shp <- readOGR(dsn = dir, layer = "EN_Wales")
# shp <- readOGR(dsn = dir, layer = "Europe_Countries")
shp <- readOGR(dsn = dir, layer = "france_all_10")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)
# plot(shp)

PM25_CHIMERE <- "W_fr-meteofrance,MODEL,CHIMERE+ANALYSIS+SURFACE+PM25+-24H-1H_C_LFPW_20160906000000.nc" ## (ug/m3)

PM25_CHIMERE <- open.nc(PM25_CHIMERE)

#Read data
PM25_CHIMERE <- read.nc(PM25_CHIMERE)

LAT_CHIMERE <- as.vector(PM25_CHIMERE$latitude)  ### get Lat
LON_CHIMERE <- as.vector(PM25_CHIMERE$longitude)  ### get Lon
LAT <- repmat(LAT_CHIMERE,1,700)
LAT <- t(LAT)
LAT <- as.data.frame(LAT)
LON = repmat(LON_CHIMERE,400,1)
LON = c(LON)

LON_west <-subset(LON, LON < 360 & LON > 335)
max(LON_west)
min(LON_west)


LON <- as.data.frame(LON)
LON_east <- LON %>%
  filter(!(LON < 360 & LON > 335))

LON_west <- LON_west - 360
LON_west <- as.data.frame(LON_west)
colnames(LON_west) <- "LON"
LON <- rbind(LON_west, LON_east)

  
# data, lat, long

# Jan <- CH4$gridded_data[1, , ]
# Jan[is.na(Jan)] <- 0 
# 
# Feb <- CH4$gridded_data[2, , ]
# Feb[is.na(Feb)] <- 0 


Data_CHIMERE <- PM25_CHIMERE$pm2p5_conc[, ,12]  ### lat , lon ,hour
Data_CHIMERE[is.na(Data_CHIMERE)] <- 0 
Data_CHIMERE <- t(Data_CHIMERE)
Data_CHIMERE <- c(Data_CHIMERE)
Data_CHIMERE <- as.data.frame(Data_CHIMERE)

CHIMERE_PM25 <- cbind(LON, LAT, Data_CHIMERE)
colnames(CHIMERE_PM25) <- c("LON", "LAT", "Data_CHIMERE")
# write.csv(CHIMERE_PM25, "CHIMERE_PM25.csv")

# CH4_YEAR <- Jan + Feb + Mar + Apr+ May+ Jun+ July+ Augu+ Sep+ Oct+ Nov+ Dec
# AVG_CH4 <- CH4_YEAR/12


# AVG_CH4 <- c(AVG_CH4)
# CHIMERE_PM25 <-  subset(CHIMERE_PM25, !is.na(CHIMERE_PM25) & CHIMERE_PM25>0)

crs <- projection(shp) ### get projections from shp file

CHIMERE_PM25 <- SpatialPointsDataFrame(CHIMERE_PM25[,1:2], CHIMERE_PM25, proj4string=CRS(crs)) 
pixels <- SpatialPixelsDataFrame(CHIMERE_PM25, tolerance = 0.916421, CHIMERE_PM25@data)
raster_CHIMERE_PM25 <- raster(pixels[,'Data_CHIMERE'])
plot(raster_CHIMERE_PM25)
plot(shp, add=TRUE, lwd=1)

# write a nc file
CHIMERE_PM25_nc <- writeRaster(raster_CHIMERE_PM25,
                                 filename="raster_CHIMERE_PM25.nc",
                                 format="CDF", overwrite=TRUE) 
CHIMERE_PM25_nc <- raster("raster_CHIMERE_PM25.nc")

CHIMERE_PM25_nc <- crop(CHIMERE_PM25_nc, extent(shp))
CHIMERE_PM25_nc <- mask(CHIMERE_PM25_nc, shp)
plot(CHIMERE_PM25_nc)

# convert nc file into .tiff

library(tiff)
## Output: a GeoTIFF file
file.tiff_CHIMERE_PM25 <- 'CHIMERE_PM25.tif'
CHIMERE_PM25_tiff <- writeRaster(CHIMERE_PM25_nc, filename = file.tiff_CHIMERE_PM25, format = 'GTiff', overwrite = T)
plot(CHIMERE_PM25_tiff)
CHIMERE_PM25_tiff <- raster("CHIMERE_PM25.tif")
plot(CHIMERE_PM25_tiff)

# land_cover_UK <- read.csv("Land_Cover_London.csv")
# plot(land_cover_UK)


####### Map data with Leaflet #############################################

# define color palette
pal_CHIMERE_PM25 <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                                         getValues(CHIMERE_PM25_tiff),na.color = "transparent")

map <- leaflet() %>%
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addRasterImage(CHIMERE_PM25_tiff, colors = pal_CHIMERE_PM25, opacity = 0.5,
                 group = "CHIMERE_PM2.5_10km") %>%
  addLegend("bottomright",pal = pal_CHIMERE_PM25, values = values(CHIMERE_PM25_tiff),
            title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%

addLayersControl(
  baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
  overlayGroups = c("CHIMERE_PM2.5_10km"),
  options = layersControlOptions(collapsed = TRUE)) 
#  hideGroup("CHIMERE_PM2.5_10km") 

map



#################################################
#################################################

# load Donkelaar Satellite-Derived PM2.5, 2014, at 35% RH [ug/m3] (with Geographical regression adjustment)


PM25_2014_GRW <- raster("GlobalGWR_PM25_GL_201401_201412-RH35.nc")
projection(PM25_2014_GRW) <- CRS("+proj=longlat +datum=WGS84")

### crop raster over the EUrope shp file  ###############################
PM25_2014_GRW <- crop(PM25_2014_GRW, extent(shp))
PM25_2014_GRW <- mask(PM25_2014_GRW, shp)
# PM25_2014_nc_cropped <- crop(PM25_2014_nc, extent(CHIMERE_PM25_nc))

file.tiff_GWR_PM25_1km <- 'GWR_PM25_1km.tif'
GWR_PM25_1km_tiff <- writeRaster(PM25_2014_GRW, filename = file.tiff_GWR_PM25_1km, format = 'GTiff', overwrite = T)
plot(GWR_PM25_1km_tiff)

plot(PM25_2014_GRW)
plot(shp, add=TRUE, lwd=1)

# check resoltuion 
res(PM25_2014_GRW)
res(CHIMERE_PM25_nc)



### Exctract points from raster ######################################
# PM25_2014_GWR <- rasterToPoints(PM25_2014_nc_cropped)
# head(PM25_2014_GWR)
# colnames(PM25_2014_GWR) <- c("Lon", "Lat", "PM25_2014")
# PM25_2014_GWR <- as.data.frame (PM25_2014_GWR)
# PM25_2014_GWR <- subset(PM25_2014_GWR, !is.na(PM25_2014) & PM25_2014>0)
# head(PM25_2014_GWR)
# write.csv(PM25_2014_GWR, file = "PM25_2014_GWR_Donkelaar.csv", row.names=FALSE)

#############################################################################################

## make resolution of CHIMERE as the one of GWR-------------------------------
CHIMERE_1km = projectRaster(CHIMERE_PM25_nc, PM25_2014_GRW) 
plot(CHIMERE_1km)

# write a nc file
CHIMERE_1km_nc <- writeRaster(CHIMERE_1km,
                               filename="CHIMERE_1km.nc",
                               format="CDF", overwrite=TRUE) 

file.tiff_CHIMERE_PM25_1km <- 'CHIMERE_PM25_1km.tif'
CHIMERE_PM25_1km_tiff <- writeRaster(CHIMERE_1km_nc, filename = file.tiff_CHIMERE_PM25_1km, format = 'GTiff', overwrite = T)
plot(CHIMERE_PM25_1km_tiff)

####### Map data with Leaflet #############################################

# define color palette
pal_CHIMERE_PM25_1km <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                                 getValues(CHIMERE_PM25_1km_tiff),na.color = "transparent")

map <- leaflet() %>%
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(CHIMERE_PM25_1km_tiff, colors = pal_CHIMERE_PM25_1km, opacity = 0.5,
                 group = "CHIMERE_PM2.5_1km") %>%
  addRasterImage(CHIMERE_PM25_tiff, colors = pal_CHIMERE_PM25, opacity = 0.5,
                 group = "CHIMERE_PM2.5_10km") %>%
  
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("CHIMERE_PM2.5_1km", "CHIMERE_PM2.5_10km"),
    options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup("CHIMERE_PM2.5_1km") 

map


## -------------------------------########################################
### Exctract poitns from TIFF files ######################################
# same rsolution 1km as from Donkelaar

CHIMERE <- raster("CHIMERE_PM25_1km.tif")
plot (CHIMERE)
GWR <- raster("GWR_PM25_1km.tif")
plot(GWR)


### Exctract poitns from raster ######################################
CHIMERE_pt <- rasterToPoints(CHIMERE)
head(CHIMERE_pt)
colnames(CHIMERE_pt) <- c("Lon", "Lat", "PM25_2014_CHIMERE")
CHIMERE_pt <- as.data.frame (CHIMERE_pt)
CHIMERE_pt <- subset(CHIMERE_pt, !is.na(PM25_2014_CHIMERE) & PM25_2014_CHIMERE>0)
head(CHIMERE_pt)
# write.csv(CHIMERE_pt, file = "CHIMERE_PM2.5_pts.csv", row.names=FALSE)

GWR_pt <- rasterToPoints(GWR)
head(GWR_pt)
colnames(GWR_pt) <- c("Lon", "Lat", "PM25_2014_GWR")
GWR_pt <- as.data.frame (GWR_pt)
head(GWR_pt)
# write.csv(GWR_pt, file = "GWR_PM2.5_pts.csv", row.names=FALSE)

joint_data <- CHIMERE_pt%>%
  join(GWR_pt, c("Lon", "Lat"))

head(joint_data)


# rescale CHIMERE data with the GWR_PM2.5 data----------------------------------------
# normalise GWR data first....


joint_data <- na.omit(joint_data)  # remove NA values
max(joint_data$PM25_2014_GWR)

joint_data$PM25_2014_GWR <- (joint_data$PM25_2014_GWR)/max(joint_data$PM25_2014_GWR) # normalise GWR PM2.5 data with the maximum value
joint_data <- joint_data %>% mutate(PM25_CHIMERE_scaled = PM25_2014_CHIMERE * PM25_2014_GWR)

# go back to a spatial object

crs <- projection(shp) ### get projections from shp file

joint_data <- SpatialPointsDataFrame(joint_data[,1:2], joint_data, proj4string=CRS(crs)) 
pix <- SpatialPixelsDataFrame(joint_data, tolerance = 0.916421, joint_data@data)
raster_rescaled <- raster(pix[,'PM25_CHIMERE_scaled'])
plot(raster_rescaled)
plot(shp, add=TRUE, lwd=1)



  










###########################################################################################################################
###########################################################################################################################

# landcover data
land_cover_EU <- raster("C:/CAMS_95/R_sample/Land_cover_USGS_2001_2012/MCD12Q1.tiff")
plot(land_cover_EU)


### crop raster over the EUrope shp file  ###############################
land_cover_EU <- crop(land_cover_EU, extent(shp))
land_cover_EU <- mask(land_cover_EU, shp)
# PM25_2014_nc_cropped <- crop(PM25_2014_nc, extent(CHIMERE_PM25_nc))


# write a nc file
land_cover_EU_nc <- writeRaster(land_cover_EU,
                              filename="land_cover_EU.nc",
                              format="CDF", overwrite=TRUE) 

land_cover_EU_nc <- raster("land_cover_EU.nc")
plot(land_cover_EU_nc)

# write a tiff file
file.tiff_land_cover_EU_500m <- 'land_cover_EU_500m.tif'
land_cover_EU_500m_tiff <- writeRaster(land_cover_EU, filename = file.tiff_land_cover_EU_500m, format = 'GTiff', overwrite = T)
plot(land_cover_EU_500m_tiff)

plot(shp, add=TRUE, lwd=1)

# check resoltuion 
res(land_cover_EU_nc)
res(CHIMERE_PM25_nc)

# exctract data
land_cover_EU_pt <- rasterToPoints(land_cover_EU)
head(land_cover_EU_pt)
colnames(land_cover_EU_pt) <- c("Lon", "Lat", "class")
land_cover_EU_pt <- as.data.frame (land_cover_EU_pt)


## make resolution of CHIMERE as the one of land cover at 500m-------------------------------
CHIMERE_1km_nc <- raster("CHIMERE_1km.nc")
plot(CHIMERE_1km_nc)
CHIMERE_500m = projectRaster(CHIMERE_1km_nc, land_cover_EU_nc) 
plot(CHIMERE_500m)
res(CHIMERE_500m)


### Exctract poitns from raster ######################################
CHIMERE_pt <- rasterToPoints(CHIMERE_500m)
head(CHIMERE_pt)
colnames(CHIMERE_pt) <- c("Lon", "Lat", "PM25_2014_CHIMERE")
CHIMERE_pt <- as.data.frame (CHIMERE_pt)
# CHIMERE_pt <- subset(CHIMERE_pt, !is.na(PM25_2014_CHIMERE) & PM25_2014_CHIMERE>0)
head(CHIMERE_pt)


joint_data <- CHIMERE_pt%>%
  join(land_cover_EU_pt, c("Lon", "Lat"))
# remove missing values
joint_data <- subset(joint_data, !is.na(class) & class>0)


##### only keep urban fraction  Lancover == 13 #########

LC <- as.data.frame(joint_data$class)

LC[LC < 13] <- 0
LC[LC > 13] <- 0

LC_URBAN_FR <- cbind(joint_data$Lon, joint_data$Lat, (LC/13), joint_data$PM25_2014_CHIMERE )  ### urban fraction(%)
colnames(LC_URBAN_FR) <- c("Lon", "Lat", "Land_URB", "PM25_2014_CHIMERE")

## Geographical regression model -----------------------------------------------------------------------------

library(spgwr)
library(AICcmodavg)
library(usdm)
library(nortest)

# mydata[is.na(mydata)] <- 0

nn <- 100/nrow(LC_URBAN_FR)
memory.limit()

gwr100 <- gwr(PM25_2014_CHIMERE ~ Land_URB, data  = LC_URBAN_FR,
              coords = cbind(LC_URBAN_FR$Lon, LC_URBAN_FR$Lat), 
              adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)

gwr100


