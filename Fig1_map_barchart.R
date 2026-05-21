### set your own working directory and store all used files there
setwd('C:/Users/lopez061/LER')

require(readxl); require(ggplot2); require(dplyr); require(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(scatterpie)
library(countrycode)
library(tidyr)
library(patchwork)

# --- read input data-----
data <- read_excel('data/map.xlsx')


#Clean data and use ISO codes
data <- data %>%
  mutate(
    country = trimws(country),
    agsys = trimws(agsys),
    iso3 = countrycode(country,
                       origin = "country.name",
                       destination = "iso3c")
  )

#Check for failures, if anything shows up, clean manually the database
data %>% filter(is.na(iso3))

#Pivot to wide format
data_wide <- data %>%
  pivot_wider(
    names_from = agsys,
    values_from = studies,
    values_fill = 0,
    values_fn = sum
  )

#Add total studies
data_wide <- data_wide %>%
  mutate(total_studies = rowSums(select(., -country, -iso3)))


# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")


#Extract coordinates 
coords <- world %>%
  st_point_on_surface() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(iso_a3_eh, lon, lat)


#Join data with coordinates
data_plot <- data_wide %>%
  left_join(coords, by = c("iso3" = "iso_a3_eh"))

# Check missing coordinates
data_plot %>% filter(is.na(lon) | is.na(lat))
#Should be empty, or very few rows

#scale pie sizes
data_plot <- data_plot %>%
  mutate(radius = scales::rescale((total_studies)^0.35, to = c(1, 4.7)))

#define the color palette
agsys_colors <- c(
  "Agroforest" = "#000000",
  "Tree&crop" = "#E69F00",
  "Intercropping" = "#56B4E9",
  "Orchards" = "#009E73",
  "Rotation" = "#F0E442",
  "Silvopasture" = "#0072B2",
  "Sparse trees" = "#D55E00",
  "Crop-livestock" = "#CC79A7",
  "Cover crops" = "#999999"
)

# Define reference values for legend
legend_values <- c(5, 25, 100)

# Legend sizes
legend_sizes <- scales::rescale(
  (legend_values)^0.35,
  to = c(1, 4.7),
  from = range((data_plot$total_studies)^0.35)
)



#Map
p_map <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray60") +
  geom_scatterpie(
    data = data_plot,
    aes(x = lon, y = lat, group = iso3, r = radius),
    cols = unique(data$agsys),
    color = NA,
    alpha = 0.7
  ) +
  #geom_scatterpie_legend(
  #  legend_sizes,
  #  x = -160, y = -55,
  #  n = 3,
  #  labeller = function(x) {
  #    c("5", "20", "50+")[seq_along(x)]
  #  }
  #)+
  #geom_scatterpie_legend(
  #  legend_sizes,
  #  x = -160, y = -55,   # adjust position as needed
  #  n=3,
  #  labeller = function(x) c("5", "20", "50+"))+
  #  theme(
  #    legend.key = element_rect(fill = NA, color = NA)
  #  )
  
  scale_fill_manual(values = agsys_colors, guide = guide_legend(override.aes = list(color = NA)))+
  #(values = agsys_colors) +
  #guides(fill = guide_legend(title = NULL)) +
  coord_sf() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


p_map



#Final check before looking at the map
summary(data_plot$lon)
summary(data_plot$lat)
#No NA, then it's a good map

####BAR CHART

#Add regions to data using countrycode
data <- data %>%
  mutate(region = countrycode(country,
                              origin = "country.name",
                              destination = "region"))


#Aggregate by regions and agsys
data_region_sum <- data %>%
  group_by(region, agsys) %>%
  summarise(studies = sum(studies), .groups = "drop")

#Order regions
data_region_sum <- data_region_sum %>%
  group_by(region) %>%
  mutate(total = sum(studies)) %>%
  ungroup() %>%
  mutate(region = reorder(region, total))

#Plot a bar chart with observations per world region
p_bar <- ggplot(data_region_sum, aes(x = region, y = studies, fill = agsys)) +
  geom_col() +
  scale_fill_manual(values = agsys_colors) +
  guides(fill = guide_legend(title = NULL)) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Region", y = "Number of observations")

p_bar

p_bar_narrow <- plot_spacer() + p_bar + plot_spacer() +
  plot_layout(widths = c(1, 3, 1))  # middle = bar chart


p_map <- p_map +
  guides(fill = "none")

combined_plot <- p_map / p_bar_narrow +
  plot_layout(
    heights = c(2.3, 1),
    guides = "collect",
    widths = c(1, 0.25) 
  ) &
  theme(#legend.position = "right",
        legend.box.margin = margin(0, 0, 0, 0),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.2, "cm"),
        plot.margin = margin(5, 5, 5, 5),
        plot.tag.position = c(0.05, 0.98))


combined_plot +
  plot_annotation(tag_levels = "a")

#combined_plot <- combined_plot +
#  plot_annotation(tag_levels = "a")&
#  theme(
#    plot.tag.position = c(0.10, 0.98)  # adjust x position
#  )

combined_plot



----------------------------------------------------------

p_map <- p_map + theme(legend.position = "none")

combined_plot <- p_map / p_bar +
  plot_layout(
    heights = c(3, 1),
    guides = "collect"
  ) &
  theme(
    legend.position = "right"
  )

combined_plot <- combined_plot +
  plot_annotation(tag_levels = "a")

combined_plot


combined_plot <- p_map / p_bar +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")
combined_plot +
  plot_annotation(tag_levels = "a")
