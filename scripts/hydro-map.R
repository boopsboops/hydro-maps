#!/usr/bin/env Rscript

# load libs
library("here")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("sf") 

# tutorial from:
# https://blog.benthies.de/blog/mapping-streams-and-rivers-with-ggplot-sf/

# download rivers shapefile 
download.file(url="https://data.hydrosheds.org/file/hydrosheds-associated/gloric/GloRiC_v10_shapefile.zip",destfile=here("assets/GloRiC_v10_shapefile.zip"))

# unzip
unzip(zipfile=here("assets/GloRiC_v10_shapefile.zip"),exdir=here("assets"))

# load shapefile
rivers <- sf::st_read("assets/GloRiC_v10_shapefile/GloRiC_v10.shp")

# Load and manipulate maps
world <- rnaturalearth::ne_countries(scale="medium",returnclass="sf")
world$geometry |> plot()

# crop map to Africa and plot
africa <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf")
africa |> ggplot() + geom_sf()

#africa.box <- sf::st_bbox(africa)
#africa <- sf::st_crop(world,africa.box)
#africa |> ggplot() + geom_sf()

# filter river data to only contain streams in Africa / Arabia
rivers.africa <- sf::st_crop(rivers,africa)
saveRDS(rivers.africa, here("assets/GloRiC_v10_shapefile/rivers-africa.rds")


        
