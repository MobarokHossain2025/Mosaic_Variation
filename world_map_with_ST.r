df <- data.frame(
  region = c("Africa", "Oceania", "Eastern Asia", "India", "Nepal", "Pakistan", "Bangladesh",
             "North America", "South America", "Southeast Asia", "Weastern Asia", "Europe"),
  lat = c(9.1, -25.2744, 35.86166, 20.59368, 28.39486, 30.37532, 23.68, 50, -14.235, 15.87003, 21.51258, 46.81819),
  long = c(18.28, 133.7751, 104.1954, 78.96288, 84.12401, 69.34512, 90, -102, -51.92528, 100.9925, 55.92326, 8.227512),
  ST11 = c(5,6,458,4,4,6,89,55,154,54,71,268),
  ST147 = c(13,15,3,82,1,12,40,58,17,148,45,174),
  ST15 = c(21,20,28,7,39,43,12,94,23,157,28,272),
  ST16 = c(2,21,10,12,0,0,51,63,8,201,3,65),
  ST307 = c(32,14,8,3,3,1,10,277,17,52,8,259),
  ST340 = c(6,5,1,0,3,0,17,24,8,35,1,46),
  ST37 = c(15,2,32,3,2,2,13,52,4,33,6,7),
  ST48 = c(13,3,3,5,0,16,45,10,0,13,2,40),
  ST502 = c(5,0,0,0,0,0,18,1,0,4,0,3),
  ST1998 = c(0,0,0,0,0,0,20,0,0,0,0,0),
  others = c(577,443,209,289,64,92,277,1872,402,1224,531,3067)
)
df

library(devtools)

# install_github("GuangchuangYu/scatterpie")
library(scatterpie)
exists("scatterpie") 
# install.packages(c("scatterpie"))
library(ggplot2)
library(scatterpie)
library(maps)
library(dplyr)
library(RColorBrewer)

# Prepare the base world map
world_map <- map_data("world")

# Pie columns
pie_cols <- colnames(df)[4:ncol(df)]

# Calculate total isolates per region (for pie size)
df$total <- rowSums(df[, pie_cols])

# Create a color palette (13 STs = 12 + "others")
colors <- brewer.pal(n = 12, name = "Set3")  # 12 colors
colors <- c(colors, "#999999")              # gray for "others"

# Adjust pie radius (scaled down)
scale_factor <- 5  # larger = smaller pies; adjust if needed
df$radius <- sqrt(df$total) / scale_factor

# Plot
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "black") +
  
  geom_scatterpie(
    data = df,
    aes(x = long, y = lat, r = radius),
    cols = pie_cols,
    color = "black"
  ) +
  scale_fill_manual(values = colors, name = "ST Types") +
  
  geom_scatterpie_legend(
    df$radius, 
    x = -170, y = -60, 
    n = 4, 
    labeller = function(x) round((x * scale_factor)^2)
  ) +
  
  coord_fixed() +
  theme_bw() +
  labs(
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )
# library(plotly)
