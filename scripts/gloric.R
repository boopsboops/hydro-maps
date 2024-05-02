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
#world$geometry |> plot()

# crop map to Africa and plot
africa <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf")
#africa |> ggplot() + geom_sf()

africa.box <- sf::st_bbox(africa)
africa <- sf::st_crop(world,africa.box)
#africa |> ggplot() + geom_sf()

# filter river data to only contain streams in Africa / Arabia
rivers.africa <- sf::st_crop(rivers,africa)
rm(rivers)
readr::write_rds(rivers.africa, here("assets/GloRiC_v10_shapefile/rivers-africa.rds"))
rivers.africa <- readr::read_rds(here("assets/GloRiC_v10_shapefile/rivers-africa.rds"))


# Manipulate River Data for plotting
rivers.small <- rivers.africa |>
    filter(Reach_type != 0) |> 
    mutate(class_hydr_discharge = str_sub(Class_hydr, 1, 1),variability = str_sub(Class_hydr, 2, 2), CMI = str_sub(Class_phys, 2, 2)) |> 
    rowwise() |>
    mutate(red_hydr_class = ifelse(Reach_type < 1000, str_sub(Reach_type, 2, 2), str_sub(Reach_type, 3, 3))) |>
    ungroup()|>
    mutate(width = as.numeric(class_hydr_discharge), width = case_when(width == 5 ~ 1, width == 4 ~ 0.8, width == 3 ~ 0.6, width == 2 ~ 0.4, width == 1 ~ 0.2, TRUE ~ 0)) |>
    rowwise() |>
    mutate(stream_power = ifelse(Reach_type < 1000, str_sub(Reach_type, 3, 3), str_sub(Reach_type, 4, 4))) |>
    ungroup()

# Filter river data 
rivers.plot <- rivers.small |>
  dplyr::filter(CMI != 1 | Class_geom == 12 | variability <= 1 | class_hydr_discharge > 1 | stream_power >= 2) |> 
  dplyr::select(width, Reach_type, geometry, red_hydr_class) |> 
  sf::st_as_sf()

nrow(rivers.small)

# Read in colors for plotting
colors <- read_tsv(here("assets/colors.tsv"))
colors_gg_names <- sort(rivers.plot$Reach_type) |> unique()
colors_gg <- colors$color[which(colors$classNumber %in% colors_gg_names)]
names(colors_gg) <- colors_gg_names


# Plot and save Africa
g <- ggplot(data = africa) +
    geom_sf(fill="#FFFFFF", color="#AAAAAA") +
    geom_sf(data=rivers.plot, aes(color=factor(Reach_type), size=width, alpha=factor(red_hydr_class))) +
    scale_size_continuous(range = c(0,0.3)) +
    scale_color_manual(values = colors_gg) +
    scale_alpha_manual(values=c("1" = 0.1, "2" = 0.4, "3" = 0.7, "4" = 1, "5" = 1)) +
    theme_minimal() + 
    theme(legend.position="none", panel.grid = element_blank())
#plot(g)

g <- ggplot(data = africa) +
    geom_sf(fill="#FFFFFF", color="#AAAAAA") +
    geom_sf(data=rivers.plot, color="blue", aes(size=width)) +
    scale_size_continuous(range = c(0,0.3)) +
    #scale_color_manual(values = colors_gg) +
    theme_minimal() + 
    theme(legend.position="none", panel.grid = element_blank())



ggsave(here("temp/africa.png"),g,width=30,height=30,units="cm")
