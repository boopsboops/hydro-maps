#!/usr/bin/env Rscript

# load libs
library("here")
library("tidyverse")
library("sf")
library("elevatr")
library("terra")
library("marmap")
library("giscoR")
library("rayshader")

### BASEMAP ###

# set lonlat and proj
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

# test a country
#giscoR::gisco_get_countries(country="Myanmar") |> sf::st_geometry() |> plot(col = "seagreen2")

# fun to get country
get_sf <- function(country) {
    country.sf <- giscoR::gisco_get_countries(resolution = "10",country = country)
    country.transformed <- sf::st_transform(country.sf, crs = crsLONGLAT)
    return(country.transformed)
}

country.focus <- get_sf(country="Myanmar")
country.neighbours <- get_sf(country=c("Myanmar","Thailand","India","Nepal","Laos","Bangladesh","China"))

country.region <- sf::st_crop(country.neighbours, country.focus)


### RIVERS ###

# get rivers
#download.file(url="https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip",destfile=here("assets/HydroRIVERS_v10_as_shp.zip"))

# unzip
#unzip(zipfile=here("assets/HydroRIVERS_v10_as_shp.zip"),exdir=here("assets"))

# load shapefile
rivers <- sf::st_read("assets/HydroRIVERS_v10_as_shp/HydroRIVERS_v10_as.shp")

# crop to country
rivers.crop <- sf::st_crop(rivers,country.region)

# filter large rivers and make thickness col
rivers.filt <- rivers.crop |> dplyr::filter(ORD_STRA >= 4 & ORD_FLOW <= 6) |> 
    mutate(width=as.numeric(ORD_FLOW), 
        width=case_when(width == 1 ~ 6, 
            width == 2 ~ 5, 
            width == 3 ~ 4, 
            width == 4 ~ 3, 
            width == 5 ~ 2,
            width == 6 ~ 1)
        )

rivers.filt <- rivers.crop |> dplyr::filter(ORD_STRA >= 4 & ORD_FLOW <= 6) |> 
    mutate(width=as.numeric(ORD_FLOW), 
        width=flipper(width)
        )




# flip fun
flipper <- function(x){
    y <- (max(x) - x) + min(x)
    return(y)
}



### TOPOGRAPHY ###

#  get raster
get_elevation_data <- function(country,res) {
    country.elevation <- elevatr::get_elev_raster(locations = country, z = res, clip = "bbox") 
    country.elevation.df <- as.data.frame(country.elevation, xy = T) |> na.omit()
    colnames(country.elevation.df)[3] <- "elevation"
    return(country.elevation.df)
}

country.elevation.df <- get_elevation_data(country=country.region,res=5)


### SHADE ###

country.matrix <-elevatr::get_elev_raster(locations = country.region, z = 5, clip = "bbox") |> rayshader::raster_to_matrix()

ambmat <- ambient_shade(country.matrix, zscale = 30)
raymat <- ray_shade(country.matrix, zscale = 30, lambert = TRUE)
watermap <- detect_water(country.matrix)

country.matrix %>%
  sphere_shade() %>%
  add_water(detect_water(country.matrix)) %>%
  plot_map()

p |> rayshader::plot_gg(preview=TRUE)

# PLOT

p <- ggplot() +
    geom_tile(data = country.elevation.df, aes(x=x,y=y,fill=elevation)) + #, alpha=0.95
    scale_fill_etopo() +
    geom_sf(data=rivers.filt,color="#0d4a70",aes(linewidth=width,alpha=width)) +
    scale_linewidth(range=c(0.1,0.6)) +
    scale_alpha(range=c(0.3,0.8)) +
    geom_sf(data=country.region,alpha=0,color="white",linewidth=0.2) +
    #geom_sf(data=lakes,fill="blue",color="blue") +
    #coord_sf(xlim = c(90,102), ylim = c(9,30), expand = FALSE, crs=crsLONGLAT) +
    theme_bw() +
    theme(legend.position="none",panel.grid = element_blank())#

plot(p)

ggsave(filename=here("temp/topo_map.png"), width=9, height=9, dpi = 300, device='png', plot=p)
