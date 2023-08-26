library(osmextract)
library(sf)
library(tmap)
library(tidyverse)
library(concaveman)
library(units)
library(readxl)

source("00_main_functions.R")

# Leeds
Cam_Locations <- read_csv("https://datamillnorth.org/download/leeds-annual-traffic-growth/9bc51361-d98e-47d3-9963-aeeca3fa0afc/Camera%20Locations.csv",
                          col_types = cols(`Site Name` = col_character(),
                                           `Site ID` = col_integer(),
                                           Description = col_character(),
                                           Grid = col_double(),
                                           X = col_integer(),
                                           Y = col_integer(),
                                           Orientation = col_character()))

sf_locations = st_as_sf(Cam_Locations,coords = c("X","Y"),crs = 27700)

calc_road_metrics(sf_locations,data_name = "Leeds")

# Hull
library(httr)

Cam_Locations = GET("https://opendata.hullcc.gov.uk/dataset/30fd3969-556d-4eae-ae4c-f3f9d2cfa9e3/resource/90e1cce0-295e-4fa7-aa21-ebc4f3e8e8d4/download/scoot_loop_resources_full.json")

my_response = rjson::fromJSON(content(Cam_Locations,'text',encoding = "UTF-8"))

my_data = data.frame(do.call(rbind,
                             lapply(my_response,
                                    rbind))) |>
  unnest(cols = everything()) |> filter(longitude != 0)

sf_cameras = st_as_sf(my_data,coords = c("longitude","latitude"),crs = 4326)

sf_locations = st_transform(sf_cameras,crs = 27700)

calc_road_metrics(sf_locations,"Hull")

# Bristol

Cam_Locations=geojsonsf::geojson_sf("01_data_sets/Bristol/fact-traffic-counts.geojson")

sf_locations = Cam_Locations |> select(link) |> unique() |> drop_na() |> st_transform(crs = 27700)

calc_road_metrics(sf_locations,"Bristol")

# Calderdale

Cam_Locations=geojsonsf::geojson_sf("01_data_sets/Calderdale/Camera Count Locations.geojson")

sf_locations = Cam_Locations |> st_transform(crs = 27700)

calc_road_metrics(sf_locations,"Calderdale")

# Cambridge
Cam_Locations = read_excel("01_data_sets/Cambridge/Location List_4.xlsx",
                           sheet = "Location List")

sf_locations = st_as_sf(Cam_Locations,coords = c("Easting","Northing"),crs = 27700)

calc_road_metrics(sf_locations,"Cambridge")

# City of Bradford
Cam_Locations=geojsonsf::geojson_sf("01_data_sets/Bradford/2018_Traffic_Count_Sites.geojson")

sf_locations = Cam_Locations |> st_transform(crs = 27700)

calc_road_metrics(sf_locations,"Bradford")


# North Yorkshire data
Cam_Locations = read_csv("01_data_sets/North York/northyorkshireroadcount.csv") |>
  select(CP, Region, LocalAuthority, Road, RoadCategory, Easting, Northing)

sf_locations = Cam_Locations |>st_as_sf(coords = c("Easting","Northing"),crs = 27700)

calc_road_metrics(sf_locations,"North Yorkshire")

# Oxfordshire
Cam_Locations = read_excel("01_data_sets/Oxfordshire/mappedaadt2019.xlsx")

sf_locations = Cam_Locations |>st_as_sf(coords = c("Easting","Northing"),crs = 27700)

calc_road_metrics(sf_locations,"Oxfordshire")

# Brighton and Hove
Cam_Locations = st_read("01_data_sets/Brighton & Hove/Brighton & Hove Automatic Traffic Counters.kml")

sf_locations = Cam_Locations |> st_transform(crs=27700)

calc_road_metrics(sf_locations,data_name = "Brighton & Hove")

# West Midlands
Cam_Locations = geojsonsf::geojson_sf("01_data_sets/West Midlands/westmids_traffic_counting_classification_rtem_geom.json")

sf_locations = Cam_Locations |> st_transform(crs=27700)

calc_road_metrics(sf_locations,data_name = "West Midlands")

# webTRIS

install.packages('devtools')
devtools::install_github('RACFoundation/webTRISr')
library(webTRISr)

sf_locations = webtris_sites(sf = T) |>
  filter(Status == "Active") |>
  st_transform(crs = 27700)

calc_road_metrics(sf_locations,"National Highways")

