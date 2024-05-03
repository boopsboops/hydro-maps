#!/usr/bin/env Rscript

# load libs
library("here")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("sf") 

# download africa river shapefile 
download.file(url="https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_af_shp.zip",destfile=here("assets/HydroRIVERS_v10_af_shp.zip"))

# unzip
unzip(zipfile=here("assets/HydroRIVERS_v10_af_shp.zip"),exdir=here("assets"))

# load shapefile
rivers <- sf::st_read("assets/HydroRIVERS_v10_af_shp/HydroRIVERS_v10_af.shp")
str(rivers)

# basemap
africa <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf")
#africa |> ggplot() + geom_sf()


rivers |> slice_head(n=1000) |> select(ORD_STRA) |> summarise(m=max(ORD_STRA))
rivers.filt <- rivers |> dplyr::filter(ORD_FLOW < 5)


rivers.filt <- rivers |> dplyr::filter(ORD_STRA >= 5 & ORD_FLOW <= 5) |> # & ENDORHEIC==1
    mutate(width=as.numeric(ORD_FLOW), width=case_when(width == 1 ~ 5, width == 2 ~ 4, width == 3 ~ 3, width == 4 ~ 2, width == 5 ~ 1))

#str(rivers.filt)

rivers.filt |> slice_sample(n=1000) |> distinct(ORD_FLOW)

p <- ggplot(data = africa) +
    geom_sf(fill="#FFFFFF", color="#AAAAAA") +
    #geom_sf(data=rivers.filt,linewidth=0.1) + #
    geom_sf(data=rivers.filt,color="blue",aes(linewidth=width)) + #
    #scale_linewidth(breaks=c(0.5,0.4,0.3,0.2,0.1)) +
    scale_linewidth(range=c(0.1,0.6)) +
    theme_minimal()

plot(p)