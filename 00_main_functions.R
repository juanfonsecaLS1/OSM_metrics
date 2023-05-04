
calc_road_metrics = function(sf_locations,
                             data_name){

  # Producing the concave polygon
  polygon = concaveman(sf_locations,concavity = 10)|> st_buffer(dist = 500)

  # tm_shape(polygon)+tm_fill()

  # my_types = c("residential","tertiary","secondary","primary","trunk","motorway","unclassified")

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
    filter(st_intersects(my_roads_reprojected,polygon,sparse = F))

  tmap_mode("view")

  # Summary
  summary_lenght = my_selected_roads |>
    mutate(length=st_length(geometry)) |>
    st_drop_geometry() |>
    summarise(Total_lenght = sum(length),.by=highway)




  sf_locations$type = st_drop_geometry(my_selected_roads)[st_nearest_feature(sf_locations,my_selected_roads),
                                                          "highway"]


  # Plot for checking
  mymap = tm_shape(polygon)+
    tm_fill(alpha = 0.3,col = "blue")+
    tm_shape(my_selected_roads)+
    tm_lines(col = "highway")+
    tm_shape(sf_locations)+
    tm_dots()

  write_rds(mymap,file = paste0("03_maps/",
                                data_name,
                                ".rds"))

  summary_lenght = my_selected_roads |>
    mutate(length=st_length(geometry)) |>
    st_drop_geometry() |>
    summarise(Total_lenght = sum(length),.by=highway)

  summary_sensors = sf_locations |> st_drop_geometry() |> summarise(Total=n(),.by=type)

  summary_lenght |>
    left_join(summary_sensors,
              by=c("highway"="type")) |>
    mutate(Total_lenght = set_units(Total_lenght,"km")) |>
    mutate(Ratio = ifelse(is.na(Total_lenght),0,Total/(Total_lenght))) |>
    write_csv(paste0("02_network_metrics/",
                     data_name,
                     ".csv"))

  return(TRUE)

}
