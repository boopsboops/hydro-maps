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
library("ggspatial")


# fun to get base country/region
# res: One of "60" (1:60million), "20" (1:20million), "10" (1:10million), "03" (1:3million) or "01" (1:1million).
# year: One of "2001", "2006", "2010", "2013", "2016" or "2020"
get_country_region <- function(country,resolution,year) {
    # set lonlat and proj
    crs <- "+proj=longlat +datum=WGS84 +no_defs"
    #TEST# giscoR::gisco_get_countries(country="Myanmar") |> sf::st_geometry() |> plot(col = "seagreen2")
    if(country[1] %in% c("Africa","Americas","Asia","Europe","Oceania")) {
    	country.sf <- giscoR::gisco_get_countries(resolution=resolution,region=country,year=year)
    } else {
    	country.sf <- giscoR::gisco_get_countries(resolution=resolution,country=country,year=year)
    }
    country.transformed <- sf::st_transform(country.sf,crs=crs)
    return(country.transformed)
}


# fun to get river data from hydrosheds
get_hydrosheds_rivers <- function(url) {
    shp.zip <- str_split_fixed("https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip","/",6)[,6]
    shp.dir <- str_replace_all(shp.zip,".zip","")
    shp.file <- str_replace_all(shp.zip,"_shp.zip",".shp")
    shp.path <- here("assets",shp.dir,shp.file)
    if(file.exists(shp.path)) {
        rivers <- sf::st_read(shp.path)
    } else {
        download.file(url=url,destfile=here("assets",shp.zip))
        unzip(zipfile=here("assets",shp.zip),exdir=here("assets"))
        rivers <- sf::st_read(shp.path)
    }
    return(rivers)
}


# flipper fun
flipper <- function(x){
    y <- (max(x) - x) + min(x)
    return(y)
}
