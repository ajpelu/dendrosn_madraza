library(tidyverse)
library(sf)


## Dendro sites
sites <- read_csv("data/dendro_sn_review.csv")

aux_sites <- sites |>
  dplyr::select(id_plot, long, lat, forest_type, species)


dendro_plots <- aux_sites |>
  mutate(sp_shape = case_when(
    str_detect(species, "Pinus") ~ "triangle",
    str_detect(species, "Quercus") ~ "circle",
    str_detect(species, "Juniperus") ~ "diamond"))

dendro_plots <- dendro_plots |> mutate(species = case_when(
  species == "Quercus pyrenaica/ilex" ~ "Quercus pyrenaica",
  TRUE ~ species
))

# convert to sf
# dendro_plots <- st_as_sf(dendro_plots, coords = c("long", "lat"), crs = 4326)




## Get limit SN

# library(wdpar)
#
# spa_wdpa <- wdpa_fetch(
#   "Spain", wait = TRUE, download_dir = rappdirs::user_data_dir("wdpar")
# )
#
# spa_wdpa |>
#   filter(NAME == "Sierra Nevada" & WDPAID == 555588878) |>
#   st_write("data/sn_limits/sn_limits.shp")

sn_limits <- st_read("data/sn_limits/sn_limits.shp")


## SN Ecosystems

library('pangaear')


# Pérez-Luque, Antonio Jesus; Bonet-García, Francisco Javier; Zamora Rodríguez, Regino (2019)
# Map of Ecosystems Types in Sierra Nevada mountain (southern Spain) [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.910176

data <- pg_data(doi = '10.1594/PANGAEA.910176')
zip_path <- data[[1]]$path
unzip(zip_path, exdir = "data/sn_ecosystems")

sn_ecosystems <- st_read("data/sn_ecosystems/ecosystems_sn.shp") |>
  st_transform(4326) |>
  st_make_valid()


eco <- sn_ecosystems |>
  mutate(eco_type = case_when(
    ecosystem == "Pine plantations"  ~ "Repoblaciones de coníferas",
    ecosystem == "Holm oak forest" ~ "Encinares",
    ecosystem == "Pyrenean oak" ~ "Robledales",
    str_detect(ecosystem, pattern = "Authoctonous") ~ "Pinares autóctonos",
    ecosystem == "High-mountain shrubland" ~ "Matorrales de alta montaña",
    TRUE ~ ecosystem
  )) |>
  filter(eco_type %in%
           c("Repoblaciones de coníferas", "Encinares", "Robledales", "Pinares autóctonos", "Matorrales de alta montaña"))


color_eco <-
  c("Repoblaciones de coníferas" = "green4",
    "Encinares" = "#8B864E",
    "Robledales" = "#EEAD0E",
    "Pinares autóctonos" = "#CAFF70",
    "Matorrales de alta montaña" = "darkgreen")


ggplot() +
  geom_sf(data = eco, aes(fill = eco_type), color = NA) +
  scale_fill_manual(values = color_eco) +
  theme_minimal()


# Get elevation data and hillshade
library(terra)
library(tidyterra)
library(elevatr)
library(scales)



## bbox
bbox_coords <- c(
  xmin = -3.70,  # Longitud mínima
  ymin = 36.85,  # Latitud mínima
  xmax = -2.5,   # Longitud máxima
  ymax = 37.3   # Latitud máxima
)

sn_bbox <- st_bbox(
  bbox_coords, crs = st_crs(4326)
  ) |>
  st_as_sfc()

sn_bbox_sf <- st_sf(geometry = sn_bbox)

# get elevation data
elevation_data <- elevatr::get_elev_raster(locations = sn_bbox_sf, z = 11)
r <- terra::rast(elevation_data)

# check
# plot(elevation_data, main = "Elevation Data for Sierra Nevada (3 arc-second resolution)")
# plot(sn_bbox, add = TRUE)
# plot(st_geometry(eco), add=TRUE)
#

# hillshade
slope <- terra::terrain(r, "slope", unit = "radians")
aspect <- terra::terrain(r, "aspect", unit = "radians")
hill <- terra::shade(slope, aspect, 30, 45)

# Crop to bbox
hill <- terra::crop(hill, sn_bbox_sf)


# normalize names
names(hill) <- "shades"

# Hillshading palette
pal_greys <- hcl.colors(1000, "Grays")

# Index of color by cell
index <- hill |>
  mutate(index_col = scales::rescale(shades, to = c(1, length(pal_greys)))) |>
  mutate(index_col = round(index_col)) |>
  pull(index_col)

# Get cols
vector_cols <- pal_greys[index]

# Plot
color_eco <-
  c("Repoblaciones de coníferas" = "green3",
    "Encinares" = "#8B864E",
    "Robledales" = "#EEAD0E",
    "Pinares autóctonos" = "#CAFF70",
    "Matorrales de alta montaña" = "darkslategray")

colours_sp <- c("Pinus halepensis" = "#c1666b",
                "Pinus pinaster" = "#ecb42e",
                "Pinus nigra" = "#43ba85",
                "Pinus sylvestris" = "#006494",
                "Pinus sylvestris nevadensis" = "purple",
                "Quercus pyrenaica"  = "#FFEC8B",
                "Quercus ilex" = "tan4",
                "Juniperus sabina" = "darkgreen"
)

# Convertir nombres de las especies a expresiones matemáticas
italic_species <- setNames(
  lapply(names(colours_sp), function(x) bquote(italic(.(x)))),
  names(colours_sp)
)

shape_sp <- c(
  "Pinus halepensis" = 24,
  "Pinus pinaster" = 24,
  "Pinus nigra" = 24,
  "Pinus sylvestris" = 24,
  "Pinus sylvestris nevadensis" = 24,
  "Quercus pyrenaica" = 21,
  "Quercus ilex" = 21,
  "Juniperus sabina" = 23)


library(ggnewscale)
library(ggspatial)

map <- ggplot() +
  geom_sf(data = eco, aes(fill = eco_type), color = NA) +
  scale_fill_manual(values = color_eco, name = "Ecosistemas") +
  geom_spatraster(
    data = hill, fill = vector_cols, maxcell = Inf,
    alpha = .6
  ) +
  geom_sf(data = sn_limits, fill = NA, color = "black") +
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  geom_point(data = dendro_plots,
             aes(x = long, y = lat,
                 shape = species, fill = species), size = 4) +
  scale_shape_manual(values = shape_sp, name = "Especies", labels = italic_species) +
  scale_fill_manual(values = colours_sp, name = "Especies", labels = italic_species) +
  coord_sf(xlim = c(-3.65, -2.58), ylim = c(36.87, 37.26), expand = FALSE) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)
  ) +
  # add north arrow
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  xlab("Longitude") +
  ylab("Latitude")


# Save plot
ggsave("output/sn_map.png",
       plot = map, width = 32, height = 15, dpi = 300, units = "cm")






