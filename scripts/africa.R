#!/usr/bin/env Rscript

# load libs and funs
source(here::here("scripts/load-libs.R"))



### BASEMAP ###

# get basemap using gisco_get_countries() and crop to focus country
# res: One of "60" (low res), "20" , "10", "03" or "01" (high res).
# year: One of "2001", "2006", "2010", "2013", "2016" or "2020"
country.focus <- get_country_region(country="Africa",resolution="03",year="2020")
#giscoR::gisco_get_countries(region="Africa") |> sf::st_geometry() |> plot(col = "seagreen2")
#rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf") |> sf::st_geometry() |> plot(col = "seagreen2")
country.focus <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf")
country.region <- country.focus



### RIVERS ###

# download (or load from file) from hydrosheds
rivers <- get_hydrosheds_rivers(url="https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_af_shp.zip")

# crop to country
rivers.crop <- sf::st_crop(rivers,country.region)

# filter on ORD_STRA (stream order) and ORD_FLOW (river flow)
rivers.crop |> st_drop_geometry() |> tibble() |> group_by(ORD_STRA) |> count()
rivers.crop |> st_drop_geometry() |> tibble() |> group_by(ORD_FLOW) |> count()

# filter
rivers.filt <- rivers.crop |> 
    dplyr::filter(ORD_STRA >= 6 & ORD_FLOW <= 5) |> 
    dplyr::mutate(width=as.numeric(ORD_FLOW), width=flipper(width))


### LAKES ###

download.file(url="https://datacatalogfiles.worldbank.org/ddh-published-v2/0040797/1/DR0050937/africawaterbody.zip",destfile=here("assets/africawaterbody.zip"))
unzip(zipfile=here("assets/africawaterbody.zip"),exdir=here("assets"))
lakes <- sf::st_read(here("assets/Africa_waterbody.shp"))


### TOPOGRAPHY ###

# get topo from elevatr
# z = zoom level to return, ranges from 1 (low) to 14 (high) 
country.elevation <- elevatr::get_elev_raster(locations=country.region,clip="bbox",z=5)# rough 5, final 8



### SHADE ###
# https://dieghernan.github.io/202210_tidyterra-hillshade/
# https://dominicroye.github.io/en/2022/hillshade-effects/

# convert to spatrast 
country.elevation.spat <- terra::rast(country.elevation) 

# and remove bathymetry
names(country.elevation.spat) <- "alt"
country.elevation.spat <- country.elevation.spat |> dplyr::mutate(alt=pmax(-10,alt))

## Create hillshade effect
slope <- terra::terrain(country.elevation.spat,"slope",unit="radians")
aspect <- terra::terrain(country.elevation.spat,"aspect",unit ="radians")
hill <- terra::shade(slope,aspect,30,270)

# normalize names
names(hill) <- "shades"

# hillshading palette
pal.greys <- hcl.colors(1000, "Grays")
#show_col(pal_greys,labels=FALSE)

# rescale shades
index <- hill |>
    dplyr::mutate(index_col=scales::rescale(shades,to=c(1,length(pal.greys)))) |>
    dplyr::mutate(index_col=round(index_col)) |>
    dplyr::pull(index_col)

# Get cols
vector.cols <- pal.greys[index]



### ADD DATA ###

# gbif
# load data from 
library("rgbif")

ent.raw <- rgbif::occ_search(scientificName="Enteromius paludinosus",limit=5000) 
ent.tib <- ent.raw$data |> tibble()

#ent.tib |> filter(grepl("BOLD",scientificName)) |> glimpse()
ent.filt <- ent.tib |>
    #filter(grepl("BOLD",scientificName)) |>
    distinct(scientificName,decimalLatitude,decimalLongitude) |> 
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) |>
    sf::st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=4326)


### PLOT ###

# make baseplot
baseplot <- ggplot() +
    geom_spatraster(data=country.elevation.spat,maxcell=Inf,alpha=1) +
    scale_fill_etopo() +
    geom_sf(data=country.region,alpha=0,color="white",linewidth=0.1) +
    geom_sf(data=rivers.filt,color="#0d4a70",aes(linewidth=width,alpha=width),lineend="round",linejoin="round") +
    scale_alpha(range=c(0.1,1)) +  
    scale_linewidth(range=c(0.05,0.3)) +
    geom_sf(data=lakes,color="#0d4a70",fill="#0d4a70",linewidth=0.1) +
    geom_spatraster(data=hill,fill=vector.cols,maxcell=Inf,alpha=0.3) + 
    geom_sf(data=ent.filt,shape=24,color="grey30",size=2,fill="white")


#plot(baseplot)
ggsave(filename=here("temp/africa-base.png"),width=210,height=297,units="mm",dpi=300,device="png",plot=baseplot)

# pretty plot
prettyplot <- baseplot + 
    theme_minimal() +
    #xlim(c(93,100)) +
    theme(legend.position="none",
        panel.grid=element_line(color="grey30",linewidth=0.05),
        plot.background=element_rect("grey97",colour=NA)
        ) +
    scale_y_continuous(expand=c(0.01,0.01)) +
    scale_x_continuous(expand=c(0.01,0.01)) +
    ggspatial::annotation_scale()

#plot(prettyplot)
ggsave(filename=here("temp/africa-pretty.png"),width=210,height=297,units="mm",dpi=300,device="png",plot=prettyplot)
