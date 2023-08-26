library(extrafont)

if (!"Nunito Sans" %in% extrafont::fonts()) {
  extrafont::font_import(pattern = "Nunito Sans")
}
extrafont::loadfonts(device = "win", quiet = TRUE)

font="Nunito Sans"

library(sf)
library(tmap)
library(tidyverse)
library(geomtextpath)
library(ggrepel)

# read the metrics data ---------------------------------------------------
myfiles = list.files(path = "03_maps/",pattern = "points_")

list_cities = gsub(pattern = ".geojson",
                   replacement = "",
                   gsub(x = myfiles,
                        pattern = "points_",
                        ""))

list_cities = list_cities[list_cities != "National Highways"]


# all_polygons = do.call(rbind,lapply(list_cities,function(x){
#   tpolygon = geojsonsf::geojson_sf(paste0("03_maps/polygon_", x, ".geojson"))
#   st_crs(tpolygon) = 27700
#   return(tpolygon)
# }))
#

all_metrics=do.call(rbind,
                    lapply(c(list_cities,"National Highways"),function(x){
  tmetric = read.csv(paste0("02_network_metrics/sensors_",x,".csv")) |> mutate(city = x)
}))

all_metrics = data.frame(highway=rep(unique(all_metrics$highway),
                                     length(unique(all_metrics$city))),
                         city = rep(unique(all_metrics$city),
                                    each=length(unique(all_metrics$highway)))) |>
  left_join(all_metrics,by=c("highway","city"))

all_areas=do.call(rbind,
                    lapply(c(list_cities,"National Highways"),function(x){
                      tmetric = read_csv(paste0("02_network_metrics/area_",x,".csv"),col_names = FALSE) |> mutate(city = x)
                    })) |> rename(Area = X1) |> select(city,Area)

# all_metrics$Ratio[is.na(all_metrics$Ratio)]=0

my_types = c("residential","unclassified","tertiary","secondary","primary","trunk","motorway")



# # mapping the polygons ----------------------------------------------------
#
# tmap_mode("view")
#
# tm_shape(all_polygons)+
#   tm_polygons(col = "name",alpha = 0.5)

# Some plots --------------------------------------------------------------


my_plots = all_metrics |>
  mutate(highway = factor(highway,
                          levels = str_to_title(rev(my_types)),
                          labels = str_to_title(rev(my_types)),
                          ordered = T)) |>
  nest(.by = city) |>
  mutate(plot = map2(.x = city, .y=data,function(.x,.y){
    ggplot(data = .y,
           aes(x= highway,
               y=Ratio,
               fill = highway))+
      geom_hline(
        aes(yintercept = 0),
        color = "grey",
        linewidth = 0.5
      )+
      geom_col(position = "dodge2")+
      coord_curvedpolar()+
      scale_y_continuous(limits = c(-6,4.9))+
      scale_x_discrete(position = "top")+
      geom_text(x=0,y=-6,
                aes(label = "Sensors\n per\n kilometre"),
                size = 5,
                col = "#474747",
                family= "Nunito Sans SemiBold"
                )+
      geom_text(aes(label = sprintf("%.1f",Ratio)),
                color = "#474747",
                nudge_y = 0.7,
                size=2.5,
                family= font
      )+
      scale_fill_manual(name="",
                        values=rev(c("#ffb703",
                                     "#fd9e02",
                                     "#fb8500",
                                     "#8ecae6",
                                     "#219ebc",
                                     "#126782",
                                     "#023047")))+
      geom_textpath(
        data = tibble(x1 = factor(str_to_title(rev(my_types)),
                                                  levels = str_to_title(rev(my_types)),
                                                  labels = str_to_title(rev(my_types)),
                                                  ordered = T),
                                      y1 = -0.5,
                                      label = str_to_title(rev(my_types))),
                    aes(x=x1, y=-0.5,label = factor(label,ordered = T)),
                    inherit.aes = F,
                    linetype = 0,
                    size = 2.5,
                    col = "gray12",
                    upright = TRUE,
                    family = font)+
      theme_void()+
      #labs (title = "Sensor Density")+
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.background = element_blank(),
        # plot.title = ggplot2::element_text(family = font,
        #                                    size = 16,
        #                                    face = "bold",
        #                                    color = "#222222"),
        # plot.subtitle = ggplot2::element_text(family = font,
        #                                       size = 12,
        #                                       margin = ggplot2::margin(9, 0, 9, 0)),
        # Remove axis ticks and text
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        # Use gray text for the region names
        axis.text.x = element_blank(),
        # Move the legend to the bottom
        legend.position = "none",
        legend.background = element_blank()
      )
  }))


a= my_plots$plot[[4]]

ggsave(filename = paste0("02_network_metrics/plot_test.png"),
       plot = a,
       dpi = 330,width = 11,height = 11,units = "cm")


lapply(1:length(my_plots$city),function(x){
  ggsave(filename = paste0("02_network_metrics/plot_",my_plots$city[x],".png"),
         plot = my_plots$plot[[x]],
         dpi = 330,width = 11,height = 11,units = "cm")
})


# Plots of Network in the area --------------------------------------------

my_plots_dist = all_metrics |>
  mutate(Total_length = ifelse(is.na(Total_length),0,Total_length)) |>
  mutate(highway = factor(highway,
                          levels = str_to_title(rev(my_types)),
                          labels = str_to_title(rev(my_types)),
                          ordered = T)) |>
  nest(.by = city) |>
  mutate(plot = map2(.x = city, .y=data,function(.x,.y){
    ynudge = max(.y$Total_length,na.rm = T)*0.05
    ggplot(data = .y,
           aes(x= highway,
               y = Total_length,
               fill = highway))+
      geom_hline(
        aes(yintercept = 0),
        color = "darkgrey",
        linewidth = 1
      )+
      geom_col(position = "dodge2")+
      # coord_curvedpolar()+
      # scale_y_continuous(limits = c(-6,4.9))+
      # scale_x_discrete(position = "top")+
      # geom_text(x=0,y=-6,
      #           aes(label = "Sensors\n per\n kilometre"),
      #           size = 5,
      #           col = "#474747",
      #           family= "Nunito Sans SemiBold"
      # )+
      geom_text(aes(y = Total_length+ynudge, label = sprintf("%.1f",Total_length)),
                color = "#474747",
                size=4,
                family= font,
                # nudge_y = 20
      )+
      scale_fill_manual(name="",
                        values=rev(c("#ffb703",
                                     "#fd9e02",
                                     "#fb8500",
                                     "#8ecae6",
                                     "#219ebc",
                                     "#126782",
                                     "#023047")))+
      theme_void()+
      labs (title = "Road kilometres within the monitoring area",
            subtitle = "Total distance by road class",
            caption = "Road Network extracted from OpenStreetMap")+
      theme(
        # panel.background = element_blank(),
        plot.title = ggplot2::element_text(family = font,
                                           size = 16,
                                           face = "bold",
                                           color = "#222222"),
        plot.subtitle = ggplot2::element_text(family = font,
                                              size = 12,
                                              margin = ggplot2::margin(9, 0, 9, 0)),
        plot.caption = ggplot2::element_text(family = font,
                                             size = 8,
                                             face = "italic",
                                             color = "gray"),
        # Remove axis ticks and text
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        # Use gray text for the region names
        axis.text.x = ggplot2::element_text(family = font,
                                            size = 12,
                                            # face = "light",
                                            color = "#222222"),
        # Move the legend to the bottom
        legend.position = "none",
        legend.background = element_blank()
      )
  }))


my_plots_dist$plot[[6]]

for (x in 1:length(my_plots_dist$city)){
  ggsave(filename = paste0("02_network_metrics/dist_plot/distplot_",my_plots_dist$city[x],".png"),
         plot = my_plots_dist$plot[[x]],
         dpi = 330,width = 17,height = 12,units = "cm")
}





# plots for introduction --------------------------------------------------



scatter = all_metrics |>
  summarise(across(Total:Total_length,\(x) sum(x, na.rm = TRUE)),.by = city) |>
  left_join(all_areas,by="city") |>
  filter(city != "National Highways") |>
  # write_csv("02_network_metrics/summary_all.csv") |>
  ggplot(aes(x=Area,y=Total,col = city))+
  geom_point(size = 3,alpha = 0.7,show.legend = F)+
  geom_text_repel(aes(x=Area*1.7,label = city),family = font, col="#333333",size = 3)+
  labs(x=expression("Area [ km"^2~"]"),y = "Number of sensors")+
  theme_minimal()+
  geom_hline(yintercept = 0,linewidth = 1,col = "black")+
  geom_vline(xintercept = 0,linewidth = 1,col = "black")+
  scale_colour_manual(name = "",
                      values = c("#669900",
                                 "#99cc33",
                                 "#ccee66",
                                 "#006699",
                                 "#3399cc",
                                 "#990066",
                                 "#cc3399",
                                 "#ff6600",
                                 "#ff9900",
                                 "#ffcc00"))+
  scale_x_continuous(trans = "log10")+
  scale_y_continuous(trans = "log10")+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(family = font,
                                       size = 16,
                                       face = "bold",
                                       color = "#222222"),
    plot.subtitle = ggplot2::element_text(family = font,
                                          size = 12,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_text(family = font,
                                         size = 8,
                                         face = "italic",
                                         color = "gray"),
    # Remove axis ticks and text
    axis.title = ggplot2::element_text(family = font,
                                       size = 10,
                                       # face = "light",
                                       color = "#222222"),
    # axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(family = font,
                                        size = 8,
                                        # face = "light",
                                        color = "#222222"),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(family = font,
                                        size = 8,
                                        # face = "light",
                                        color = "#222222"),
    # Move the legend to the bottom
    legend.position = "bottom"
  )

ggsave(filename = "02_network_metrics/scatter.png",plot = scatter,
       dpi = 330,width = 17,height = 12,units = "cm")



# proportions -------------------------------------------------------------


plot_proportion = all_metrics |>
  mutate(highway = factor(highway,
                          levels = str_to_title(rev(my_types)),
                          labels = str_to_title(rev(my_types)),
                          ordered = T)) |>
  mutate(proportion = Total_length/sum(Total_length),.by = city)|>
  ggplot(aes(x= highway,
             y = proportion,
             fill = highway))+
  geom_hline(
    aes(yintercept = 0),
    color = "darkgrey",
    linewidth = 1
  )+
  geom_boxplot(outlier.shape = NA,alpha = 0.6)+
  geom_jitter(aes(col = highway),alpha = 0.3,size = 2)+
  scale_fill_manual(name="",
                    values=rev(c("#ffb703",
                                 "#fd9e02",
                                 "#fb8500",
                                 "#8ecae6",
                                 "#219ebc",
                                 "#126782",
                                 "#023047")))+
  scale_colour_manual(name="",
                    values=rev(c("#ffb703",
                                 "#fd9e02",
                                 "#fb8500",
                                 "#8ecae6",
                                 "#219ebc",
                                 "#126782",
                                 "#023047")))+
  scale_y_continuous(labels = scales::percent)+
  # geom_textpath(data = data.frame(x1 = factor(str_to_title(rev(my_types)),
  #                                             levels = str_to_title(rev(my_types)),
  #                                             labels = str_to_title(rev(my_types)),
  #                                             ordered = T),
  #                                 y1 = -0.5,
  #                                 label = factor(str_to_title(rev(my_types)),
  #                                                levels = str_to_title(rev(my_types)),
  #                                                labels = str_to_title(rev(my_types)),
  #                                                ordered = T)),
  #               aes(x=x1,y=y1,label = label),
  #               inherit.aes = F,
#               linetype = 0,
#               size = 2.5,
#               col = "gray12",
#               upright = TRUE,
#               family = font)+
coord_flip()+
theme_void()+
  # labs (title = "Proportion of road kilometres by class",
  #       caption = "Road Network from OpenStreetMap")+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(family = font,
                                       size = 16,
                                       face = "bold",
                                       color = "#222222"),
    plot.subtitle = ggplot2::element_text(family = font,
                                          size = 12,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_text(family = font,
                                         size = 8,
                                         face = "italic",
                                         color = "gray"),
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(family = font,
                                        size = 10,
                                        # face = "light",
                                        color = "#222222"),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(family = font,
                                        size = 12,
                                        # face = "light",
                                        color = "#222222"),
    # Move the legend to the bottom
    legend.position = "none",
    legend.background = element_blank()
  )

ggsave(filename = paste0("02_network_metrics/box_plots_proportions.png"),
       plot = plot_proportion,
       dpi = 330,width = 20,height = 12,units = "cm")


# proportions -------------------------------------------------------------


plot_ratios = all_metrics |>
  mutate(highway = factor(highway,
                          levels = str_to_title(rev(my_types)),
                          labels = str_to_title(rev(my_types)),
                          ordered = T)) |>
  mutate(proportion = Total_length/sum(Total_length),.by = city)|>
  ggplot(aes(x= highway,
             y = Ratio,
             fill = highway))+
  geom_hline(
    aes(yintercept = 0),
    color = "darkgrey",
    linewidth = 1
  )+
  geom_boxplot(outlier.shape = NA,alpha = 0.6)+
  geom_jitter(aes(col = highway),alpha = 0.3,size = 2)+
  scale_fill_manual(name="",
                    values=rev(c("#ffb703",
                                 "#fd9e02",
                                 "#fb8500",
                                 "#8ecae6",
                                 "#219ebc",
                                 "#126782",
                                 "#023047")))+
  scale_colour_manual(name="",
                      values=rev(c("#ffb703",
                                   "#fd9e02",
                                   "#fb8500",
                                   "#8ecae6",
                                   "#219ebc",
                                   "#126782",
                                   "#023047")))+
  # scale_y_continuous(labels = scales::percent)+
  # geom_textpath(data = data.frame(x1 = factor(str_to_title(rev(my_types)),
  #                                             levels = str_to_title(rev(my_types)),
  #                                             labels = str_to_title(rev(my_types)),
  #                                             ordered = T),
  #                                 y1 = -0.5,
  #                                 label = factor(str_to_title(rev(my_types)),
  #                                                levels = str_to_title(rev(my_types)),
  #                                                labels = str_to_title(rev(my_types)),
  #                                                ordered = T)),
  #               aes(x=x1,y=y1,label = label),
  #               inherit.aes = F,
#               linetype = 0,
#               size = 2.5,
#               col = "gray12",
#               upright = TRUE,
#               family = font)+
coord_flip()+
  theme_void()+
  # labs (title = "Proportion of road kilometres by class",
  #       caption = "Road Network from OpenStreetMap")+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(family = font,
                                       size = 16,
                                       face = "bold",
                                       color = "#222222"),
    plot.subtitle = ggplot2::element_text(family = font,
                                          size = 12,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_text(family = font,
                                         size = 8,
                                         face = "italic",
                                         color = "gray"),
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(family = font,
                                        size = 10,
                                        # face = "light",
                                        color = "#222222"),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(family = font,
                                        size = 12,
                                        # face = "light",
                                        color = "#222222"),
    # Move the legend to the bottom
    legend.position = "none",
    legend.background = element_blank()
  )

ggsave(filename = paste0("02_network_metrics/box_plots_ratios.png"),
       plot = plot_ratios,
       dpi = 330,width = 20,height = 12,units = "cm")



# scatter ratios proportion -----------------------------------------------

plot_ratios = all_metrics |>
  mutate(highway = factor(highway,
                          levels = str_to_title(rev(my_types)),
                          labels = str_to_title(rev(my_types)),
                          ordered = T)) |>
  mutate(proportion = Total_length/sum(Total_length),.by = city)|>
  ggplot(aes(x= proportion,
             y = Ratio,
             col = highway))+
  geom_hline(
    aes(yintercept = 0),
    color = "darkgrey",
    linewidth = 1
  )+
  geom_jitter(alpha = 0.3,size = 2)+
  # scale_fill_manual(name="",
  #                   values=rev(c("#ffb703",
  #                                "#fd9e02",
  #                                "#fb8500",
  #                                "#8ecae6",
  #                                "#219ebc",
  #                                "#126782",
  #                                "#023047")))+
  scale_colour_manual(name="",
                      values=rev(c("#ffb703",
                                   "#fd9e02",
                                   "#fb8500",
                                   "#8ecae6",
                                   "#219ebc",
                                   "#126782",
                                   "#023047")))+
  # scale_y_continuous(labels = scales::percent)+
  # geom_textpath(data = data.frame(x1 = factor(str_to_title(rev(my_types)),
  #                                             levels = str_to_title(rev(my_types)),
  #                                             labels = str_to_title(rev(my_types)),
  #                                             ordered = T),
  #                                 y1 = -0.5,
  #                                 label = factor(str_to_title(rev(my_types)),
  #                                                levels = str_to_title(rev(my_types)),
  #                                                labels = str_to_title(rev(my_types)),
  #                                                ordered = T)),
  #               aes(x=x1,y=y1,label = label),
#               inherit.aes = F,
#               linetype = 0,
#               size = 2.5,
#               col = "gray12",
#               upright = TRUE,
#               family = font)+
  theme_void()+
  # labs (title = "Proportion of road kilometres by class",
  #       caption = "Road Network from OpenStreetMap")+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(family = font,
                                       size = 16,
                                       face = "bold",
                                       color = "#222222"),
    plot.subtitle = ggplot2::element_text(family = font,
                                          size = 12,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_text(family = font,
                                         size = 8,
                                         face = "italic",
                                         color = "gray"),
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(family = font,
                                        size = 10,
                                        # face = "light",
                                        color = "#222222"),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(family = font,
                                        size = 12,
                                        # face = "light",
                                        color = "#222222"),
    # Move the legend to the bottom
    legend.position = "bottom",
    legend.background = element_blank()
  )

ggsave(filename = paste0("02_network_metrics/box_plots_ratios.png"),
       plot = plot_ratios,
       dpi = 330,width = 20,height = 12,units = "cm")




