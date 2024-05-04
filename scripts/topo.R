#!/usr/bin/env Rscript

# load libs
library("here")
library("tidyverse")
library("sf")
library("elevatr")
library("terra")
library("marmap")
library("giscoR")
library("terra")
library("tidyterra")
library("scales")
#library("rayshader")

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


# flip fun
flipper <- function(x){
    y <- (max(x) - x) + min(x)
    return(y)
}


rivers.filt <- rivers.crop |> dplyr::filter(ORD_STRA >= 4 & ORD_FLOW <= 6) |> 
    mutate(width=as.numeric(ORD_FLOW), 
        width=flipper(width)
        )






### TOPOGRAPHY ###

#  get raster
get_elevation_data <- function(country,res) {
    country.elevation <- elevatr::get_elev_raster(locations = country, z = res, clip = "bbox") 
    return(country.elevation)
}

country.elevation <- get_elevation_data(country=country.region,res=5)

# turn raster into df 
country.elevation.df <- as.data.frame(country.elevation, xy = T) |> na.omit()
colnames(country.elevation.df)[3] <- "elevation"



### SHADE ###
# https://dieghernan.github.io/202210_tidyterra-hillshade/
# https://dominicroye.github.io/en/2022/hillshade-effects/

country.elevation.spat <- terra::rast(country.elevation)


## Create hillshade effect

slope <- terra::terrain(country.elevation.spat, "slope", unit = "radians")
aspect <- terra::terrain(country.elevation.spat, "aspect", unit = "radians")
hill <- terra::shade(slope, aspect, 30, 270)#

# normalize names
names(hill) <- "shades"

# Hillshading, but we need a palette
pal_greys <- hcl.colors(1000, "Grays")
#show_col(pal_greys)


addAlpha <- function(COLORS, ALPHA){
    if(missing(ALPHA)) stop("provide a value for alpha between 0 and 1")
    RGB <- col2rgb(COLORS, alpha=TRUE)
    RGB[4,] <- round(RGB[4,]*ALPHA)
    NEW.COLORS <- rgb(RGB[1,], RGB[2,], RGB[3,], RGB[4,], maxColorValue = 255)
    return(NEW.COLORS)
}


pal.greys.alph <- addAlpha(pal_greys,seq.int(1, 0, length.out=1000))
#show_col(pal.greys.alph)

index <- hill %>%
  mutate(index_col = scales::rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)


# Get cols
vector_cols <- pal_greys[index]

# Get cols
vector_cols <- pal.greys.alph[index]


ggplot() +
    geom_spatraster(data = country.elevation.spat, maxcell = Inf, alpha=1) +
    scale_fill_etopo() +
    geom_sf(data=rivers.filt,color="#0d4a70",aes(linewidth=width,alpha=width)) +
    scale_linewidth(range=c(0.1,0.8)) +
    geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf, alpha = 0.3) + 
    geom_sf(data=country.region,alpha=0,color="white",linewidth=0.2) +
    theme_minimal()
    



# PLOT

p <- ggplot() +
    geom_tile(data = country.elevation.df, aes(x=x,y=y,fill=elevation)) + #, alpha=0.95
    #tidyterra::scale_fill_hypso_tint_c() +
    scale_fill_etopo() +
    #geom_sf(data=rivers.filt,color="#0d4a70",aes(linewidth=width,alpha=width)) +
    #scale_linewidth(range=c(0.1,0.6)) +
    #scale_alpha(range=c(0.3,0.8)) +
    geom_sf(data=country.region,alpha=0,color="white",linewidth=0.2) +
    #geom_sf(data=lakes,fill="blue",color="blue") +
    #coord_sf(xlim = c(90,102), ylim = c(9,30), expand = FALSE, crs=crsLONGLAT) +
    theme_bw() +
    theme(legend.position="none",panel.grid = element_blank())#
plot(p)

ggsave(filename=here("temp/topo_map.png"), width=9, height=9, dpi = 300, device='png', plot=p)
