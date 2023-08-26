library(sf)

extrafont::loadfonts(device = "win", quiet = TRUE)

font="Nunito Sans"
library(tidyverse)
# From previous analysis
bristol = read_rds(file = "bristol.rds")

my_types = c("residential","unclassified","tertiary","secondary","primary","trunk","motorway")


# Original GeoJSON
Cam_Locations=geojsonsf::geojson_sf("01_data_sets/Bristol/fact-traffic-counts.geojson")


# Producing a clean version of the dataset for analysis
clean_bristol = Cam_Locations |>
  filter(!st_is_empty(Cam_Locations)) |>
  st_transform(crs = 27700) |>
  left_join(bristol |>
              st_drop_geometry(),
            by = "sk_dim_countdeviceid") |>
  drop_na(type) |>
  mutate(timestp = as_datetime(rollupdatetime)) |>
  mutate(date = date(timestp),
    Hr = hour(timestp),
    weekday = wday(timestp,week_start = 1)
         )

clean_bristol$date |> st_drop_geometry() |>  unique() |> length()

days_summary = clean_bristol |>
  st_drop_geometry() |>
  select(sk_dim_countdeviceid,date,type) |>
  unique() |>
  summarise(ndays = n(),
            lastdate = max(date),
            firstdate = min(date),
            daysdiff = as.numeric(difftime(lastdate,firstdate,units = "days")),
            .by=c(sk_dim_countdeviceid,type))


hist_days = days_summary |>
  mutate(
  highway = factor(type,
                   levels = str_to_title(rev(my_types)),
                   labels = str_to_title(rev(my_types)),
                   ordered = T)) |>
  ggplot(aes(ndays/daysdiff,
             fill = highway))+
  geom_histogram(binwidth = 0.1)+
  geom_hline(
    aes(yintercept = 0),
    color = "darkgrey",
    linewidth = 1
  )+
  scale_fill_manual(name="",
                    values=rev(c("#ffb703",
                                 "#fd9e02",
                                 "#fb8500",
                                 "#8ecae6",
                                 "#219ebc",
                                 "#126782",
                                 "#023047")))+
  scale_x_continuous(labels = scales::percent)+
  facet_wrap(~type,nrow = 2)+
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

ggsave(plot = hist_days,filename = "02_network_metrics/Briston_hist_days.png",
       dpi = 330,width = 20,height = 12,units = "cm")


# Daily data -----------------------------------------------------------

daily_summary = clean_bristol |>
  st_drop_geometry() |>
  select(sk_dim_countdeviceid,date,type,Hr) |>
  unique() |>
  summarise(nhours = n(),
            dailydata = nhours/24,
            .by=c(sk_dim_countdeviceid,type,date)) |>
  mutate(
    highway = factor(type,
                     levels = str_to_title(rev(my_types)),
                     labels = str_to_title(rev(my_types)),
                     ordered = T))



violin_daily = dayily_summary |>
 ggplot(aes(x=highway,y=dailydata,fill=highway))+
  geom_violin(alpha = 0.7)+
  # geom_point(aes(col = highway),alpha = .4,size = 1)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(name="",
                    values=rev(c("#ffb703",
                                 "#fd9e02",
                                 "#fb8500",
                                 "#8ecae6",
                                 "#219ebc",
                                 "#126782",
                                 "#023047")))+
  theme_minimal()+
  coord_flip()+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(
      family = font,
      size = 16,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 12,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_text(
      family = font,
      size = 8,
      face = "italic",
      color = "gray"
    ),
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(
      family = font,
      size = 10,
      # face = "light",
      color = "#222222"
    ),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(
      family = font,
      size = 12,
      # face = "light",
      color = "#222222"
    ),
    # Move the legend to the bottom
    legend.position = "none",
    legend.background = element_blank()
  )

ggsave(plot = violin_daily,
       filename = "02_network_metrics/Briston_violing_days.png",
       dpi = 330,width = 17,height = 12,units = "cm")


clean_bristol |> select(sk_dim_countdeviceid) |> unique() |>  left_join(daily_summary |>
  summarise(dailydata = mean(dailydata),
            .by = sk_dim_countdeviceid),by = "sk_dim_countdeviceid") |>
  write_rds("Daily_data_mapping.rds")

clean_bristol |> select(sk_dim_countdeviceid) |> unique() |>
  left_join(days_summary,
            by = "sk_dim_countdeviceid") |>
  write_rds("annual_data_mapping.rds")



# daily flows analysis ----------------------------------------------------

daily_flows = clean_bristol |>st_drop_geometry() |>
  semi_join(daily_summary |>
              filter(dailydata>0.8),
            by=c("sk_dim_countdeviceid","date")) |>
  summarise(dailyflow = sum(hourlyflow),.by=c(sk_dim_countdeviceid,type,date)) |>
  mutate(
    highway = factor(type,
                     levels = str_to_title(rev(my_types)),
                     labels = str_to_title(rev(my_types)),
                     ordered = T))

hist_AADF=daily_flows |>
  filter(dailyflow>0&dailyflow<30000) |>
  ggplot(aes(dailyflow))+
  geom_histogram(bins = 30, fill = "darkgreen",col="white")+
  geom_hline(yintercept = 0,linewidth=1,col="#363636")+
  theme_minimal()+
  labs(x="AADF* [veh/day]")+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(
      family = font,
      size = 16,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 12,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_text(
      family = font,
      size = 8,
      face = "italic",
      color = "gray"
    ),
    # Remove axis ticks and text
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(
      family = font,
      size = 10,
      # face = "light",
      color = "#222222"
    ),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(
      family = font,
      size = 12,
      # face = "light",
      color = "#222222"
    ),
    # Move the legend to the bottom
    legend.position = "none",
    legend.background = element_blank())

ggsave(plot = hist_AADF,filename = "02_network_metrics/Bristol_hist_AADF.png",
       dpi = 330,width = 20,height = 12,units = "cm")


# Daily flows by road -----------------------------------------------------

hist_AADF_roads = daily_flows |>
  filter(dailyflow>0&dailyflow<30000) |>
  ggplot(aes(dailyflow,fill=highway))+
  geom_histogram(bins = 30,col="white")+
  geom_hline(yintercept = 0,linewidth=1,col="#363636")+
  theme_minimal()+
  facet_wrap(~highway,nrow = 3)+
  labs(x="AADF* [veh/day]")+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(
      family = font,
      size = 16,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 12,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_text(
      family = font,
      size = 8,
      face = "italic",
      color = "gray"
    ),
    # Remove axis ticks and text
    axis.title.y = element_blank(),
    axis.title.x = ggplot2::element_text(
      family = font,
      size = 12,
      # face = "light",
      color = "#222222"
    ),
    axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(
      family = font,
      size = 8,
      # face = "light",
      color = "#222222"
    ),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(
      family = font,
      size = 6,
      # face = "light",
      color = "#222222"
    ),
    # Move the legend to the bottom
    legend.position = "none",
    legend.background = element_blank(),
    strip.text = ggplot2::element_text(
      family = font,
      size = 8,
      face = "bold",
      color = "#222222"
    ))

ggsave(plot = hist_AADF_roads,filename = "02_network_metrics/Bristol_hist_AADF_roads.png",
       dpi = 330,width = 20,height = 20,units = "cm")



# daily profile -----------------------------------------------------------

profiles = clean_bristol |>
  st_drop_geometry() |>
  semi_join(daily_summary |>
              filter(dailydata>0.8),
            by=c("sk_dim_countdeviceid","date")) |>
  summarise(across(hourlyflow,list(median=median,per10=\(x) quantile(x,0.1),per90=\(x) quantile(x,0.9))),
            .by=c(sk_dim_countdeviceid,type,Hr)) |>
  mutate(
    highway = factor(type,
                     levels = str_to_title(rev(my_types)),
                     labels = str_to_title(rev(my_types)),
                     ordered = T)) |>
  ggplot(aes(x=Hr,y=hourlyflow_median,col=highway))+
  geom_line(aes(group=sk_dim_countdeviceid),alpha = 0.1)+
  facet_wrap(~highway,nrow = 3,scales = "free_y")+
  scale_colour_manual(name="",
                    values=rev(c("#ffb703",
                                 "#fd9e02",
                                 "#fb8500",
                                 "#8ecae6",
                                 "#219ebc",
                                 "#126782",
                                 "#023047")))+
  stat_summary(fun = median,geom = "line",linewidth=1)+
  theme_minimal()+
  labs(x="Hour")+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(
      family = font,
      size = 16,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 12,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_text(
      family = font,
      size = 8,
      face = "italic",
      color = "gray"
    ),
    # Remove axis ticks and text
    axis.title.y = element_blank(),
    axis.title.x = ggplot2::element_text(
      family = font,
      size = 12,
      # face = "light",
      color = "#222222"
    ),
    axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(
      family = font,
      size = 8,
      # face = "light",
      color = "#222222"
    ),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(
      family = font,
      size = 6,
      # face = "light",
      color = "#222222"
    ),
    # Move the legend to the bottom
    legend.position = "none",
    legend.background = element_blank(),
    strip.text = ggplot2::element_text(
      family = font,
      size = 8,
      face = "bold",
      color = "#222222"
    ))

write_rds(profiles,file = "profiles_plots.rds")

ggsave(plot = profiles,filename = "02_network_metrics/profiles.png",units = "cm",
       dpi = 330,width = 24,height = 20)


# weekday profiles --------------------------------------------------------


profiles_wk = clean_bristol |>
  st_drop_geometry() |>
  semi_join(daily_summary |>
              filter(dailydata>0.8),
            by=c("sk_dim_countdeviceid","date")) |>
  filter(weekday<6) |>
  summarise(across(hourlyflow,list(median=median,per10=\(x) quantile(x,0.1),per90=\(x) quantile(x,0.9))),
            .by=c(sk_dim_countdeviceid,type,Hr)) |>
  mutate(
    highway = factor(type,
                     levels = str_to_title(rev(my_types)),
                     labels = str_to_title(rev(my_types)),
                     ordered = T)) |>
  ggplot(aes(x=Hr,y=hourlyflow_median,col=highway))+
  geom_line(aes(group=sk_dim_countdeviceid),alpha = 0.1)+
  facet_wrap(~highway,nrow = 3,scales = "free_y")+
  scale_colour_manual(name="",
                      values=rev(c("#ffb703",
                                   "#fd9e02",
                                   "#fb8500",
                                   "#8ecae6",
                                   "#219ebc",
                                   "#126782",
                                   "#023047")))+
  stat_summary(fun = median,geom = "line",linewidth=1)+
  theme_minimal()+
  labs(x="Hour")+
  theme(
    # panel.background = element_blank(),
    plot.title = ggplot2::element_text(
      family = font,
      size = 16,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 12,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_text(
      family = font,
      size = 8,
      face = "italic",
      color = "gray"
    ),
    # Remove axis ticks and text
    axis.title.y = element_blank(),
    axis.title.x = ggplot2::element_text(
      family = font,
      size = 12,
      # face = "light",
      color = "#222222"
    ),
    axis.ticks = element_blank(),
    axis.text.y = ggplot2::element_text(
      family = font,
      size = 8,
      # face = "light",
      color = "#222222"
    ),
    # Use gray text for the region names
    axis.text.x = ggplot2::element_text(
      family = font,
      size = 6,
      # face = "light",
      color = "#222222"
    ),
    # Move the legend to the bottom
    legend.position = "none",
    legend.background = element_blank(),
    strip.text = ggplot2::element_text(
      family = font,
      size = 8,
      face = "bold",
      color = "#222222"
    ))

ggsave(plot = profiles_wk,filename = "02_network_metrics/profiles_wk.png",units = "cm",
       dpi = 330,width = 24,height = 20)
