#!/usr/bin/env Rscript

# load libs and funs
source(here::here("scripts/load-libs.R"))



### BASEMAP ###

# get basemap using gisco_get_countries() and crop to focus country
# res: One of "60" (low res), "20" , "10", "03" or "01" (high res).
# year: One of "2001", "2006", "2010", "2013", "2016" or "2020"
country.focus <- get_country_region(country="Myanmar",resolution="03",year="2020")
country.neighbours <- get_country_region(country=c("Myanmar","Thailand","India","Nepal","Laos","Bangladesh","China"),resolution="03",year="2020")
country.region <- sf::st_crop(country.neighbours, country.focus)



### RIVERS ###

# download (or load from file) from hydrosheds
rivers <- get_hydrosheds_rivers(url="https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip") 

# crop to country
rivers.crop <- sf::st_crop(rivers,country.region)

# filter on ORD_STRA (stream order) and ORD_FLOW (river flow)
rivers.crop |> st_drop_geometry() |> tibble() |> group_by(ORD_STRA) |> count()
rivers.crop |> st_drop_geometry() |> tibble() |> group_by(ORD_FLOW) |> count()

# filter
rivers.filt <- rivers.crop |> 
    dplyr::filter(ORD_STRA >= 4 & ORD_FLOW <= 6) |> 
    dplyr::mutate(width=as.numeric(ORD_FLOW), width=flipper(width))



### TOPOGRAPHY ###

# get topo from elevatr
# z = zoom level to return, ranges from 1 (low) to 14 (high) 
country.elevation <- elevatr::get_elev_raster(locations=country.region,clip="bbox",z=8)# rough 5, final 8



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

# load  and filter
myanmar.locations <- read_csv(here("assets/myanmar-locations.csv"),show_col_types=FALSE)

myanmar.locations.distinct <- myanmar.locations |> 
    distinct(decimalLatitude,decimalLongitude) |> 
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) |>
    filter(decimalLongitude > 90 & decimalLatitude > 11) |>
    st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=4326)



### PLOT ###

# make baseplot
baseplot <- ggplot() +
    geom_spatraster(data=country.elevation.spat,maxcell=Inf,alpha=1) +
    scale_fill_etopo() +
    geom_sf(data=rivers.filt,color="#0d4a70",aes(linewidth=width,alpha=width),lineend="round",linejoin="round") +
    scale_linewidth(range=c(0.1,0.6)) +
    geom_spatraster(data=hill,fill=vector.cols,maxcell=Inf,alpha=0.2) + 
    geom_sf(data=country.region,alpha=0,color="white",linewidth=0.1) +
    geom_sf(data=myanmar.locations.distinct,shape=24,fill="grey90",color="grey30",size=3.5)

#plot(baseplot)
#ggsave(filename=here("temp/myanmar.png"),width=210,height=297,units="mm",dpi=300,device="png",plot=baseplot)

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
ggsave(filename=here("temp/myanmar.png"),width=210,height=297,units="mm",dpi=600,device="png",plot=prettyplot)
