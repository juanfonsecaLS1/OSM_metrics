
calc_road_metrics = function(sf_locations,
                             data_name){

  st_write(sf_locations,
           dsn = paste0("03_maps/points_",
                          data_name,
                          ".geojson"))

  # Producing the concave polygon
  polygon= concaveman(sf_locations,concavity = 2)|> st_buffer(dist = 500)

  polygon$name = data_name

  st_write(polygon,
           dsn = paste0("03_maps/polygon_",
                        data_name,
                        ".geojson"))

  # tm_shape(polygon)+tm_fill()

  my_types = c("residential","unclassified","tertiary","secondary","primary","trunk","motorway")

  ## Obtaining the network layer
  # my_roads=
  #   oe_get("england",
  #          query = "SELECT * FROM 'lines' WHERE highway IS NOT NULL",
  #          extra_tags = "ref"
  #          )|>
  #   filter(highway %in% my_types)
  #
  # write_rds(my_roads,file = "my_england_roads.rds",compress = "gz")

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

  # st_write(my_selected_roads,
  #          dsn = paste0("03_maps/roads_",
  #                       data_name,
  #                       ".geojson"))

  # tmap_mode("view")

  # Summary
  summary_lenght = my_selected_roads |>
    mutate(length=st_length(geometry)) |>
    st_drop_geometry() |>
    summarise(Total_lenght = sum(length),.by=highway)




  sf_locations$type = st_drop_geometry(my_selected_roads)[st_nearest_feature(sf_locations,my_selected_roads),
                                                          "highway"]


  # Plot for checking
  # mymap = tm_shape(polygon)+
  #   tm_fill(alpha = 0.3,col = "darkblue")+
  #   tm_shape(my_selected_roads)+
  #   tm_lines(title.col = "Road type:",
  #            col = "highway",
  #            palette = rev(RColorBrewer::brewer.pal(7, "YlOrBr")))+
  #   tm_shape(sf_locations)+
  #   tm_markers()
  #
  #
  # write_rds(mymap,file = paste0("03_maps/",
  #                               data_name,
  #                               ".rds"))

  summary_lenght = my_selected_roads |>
    mutate(length=st_length(geometry)) |>
    st_drop_geometry() |>
    summarise(Total_length = sum(length),.by=highway)

  summary_sensors = sf_locations |> st_drop_geometry() |> summarise(Total=n(),.by=type)

  summary_lenght |>
    left_join(summary_sensors,
              by=c("highway"="type")) |>
    mutate(Total_length = set_units(Total_length,"km")) |>
    mutate(Ratio = ifelse(is.na(Total_length),0,Total/(Total_length))) |>
    write_csv(paste0("02_network_metrics/sensors_",
                     data_name,
                     ".csv"))
  write_lines(set_units(st_area(polygon),"km^2"),
              file = paste0("02_network_metrics/area_",data_name,".csv"))

  return(TRUE)
}
