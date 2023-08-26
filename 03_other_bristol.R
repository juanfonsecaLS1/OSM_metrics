library(osmextract)
library(sf)
library(tmap)
library(tidyverse)
library(concaveman)
library(units)
library(readxl)


Cam_Locations=geojsonsf::geojson_sf("01_data_sets/Bristol/fact-traffic-counts.geojson")

sf_locations = Cam_Locations |> select(sk_dim_countdeviceid) |> filter(!st_is_empty(Cam_Locations)) |> unique() |> st_transform(crs = 27700)



# Producing the concave polygon
polygon= concaveman(sf_locations,concavity = 2)|> st_buffer(dist = 500)


my_types = c("residential","unclassified","tertiary","secondary","primary","trunk","motorway")


my_roads = read_rds("my_england_roads.rds")

# Subset the roads
my_roads_reprojected = my_roads |>
  st_transform(crs = st_crs(polygon))

my_selected_roads = my_roads_reprojected |>
  filter(st_intersects(my_roads_reprojected,polygon,sparse = F)) |>
  mutate(highway = factor(highway,
                          levels = rev(my_types),
                          labels = str_to_title(rev(my_types)),
                          ordered = T))

sf_locations$type = st_drop_geometry(my_selected_roads)[st_nearest_feature(sf_locations,my_selected_roads),
                                                        "highway"]

write_rds(sf_locations,file = "bristol.rds")


my_data = Cam_Locations |>
  left_join(sf_locations |>
              st_drop_geometry(),
            by = "sk_dim_countdeviceid") |>
  drop_na(type) |>
  mutate(timest = as_datetime(rollupdatetime)) |>
  mutate(dayw = wday(timest,week_start = 1),
         date = date(timest),
         Hr = hour(timest))


my_data |>
  summarise(
    ndays = n(),.by=c(sk_dim_countdeviceid)
  )



ggplot(my_data,
       aes(x=Hr,y=hourlyflow,group=sk_dim_countdeviceid))+
  geom_line(alpha=0.3,col="darkblue")+
  facet_wrap(~type)

