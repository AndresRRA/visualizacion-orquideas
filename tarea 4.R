# Libraries
library(raster)
library(udunits2)
library(sf)
library(spData)
library(leaflet)
library(dplyr)

# Carga datos
## Capa cantones
cantones <- st_read(
  "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_cantones_simp_wgs84.geojson",
    quiet = TRUE
)

## Capa provincias
provincias <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_provincias_simp_wgs84.geojson",
    quiet = TRUE
  )

##Datos orquídeas
orquideas <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/orchidaceae-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )
st_crs(orquideas) = 4326

##Datos Áreas Silvestres Protegidas
asp <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/sinac/asp/asp-wgs84.geojson",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )
st_crs(asp) = 4326

# Limpieza datos orquídeas
## Coordinate uncertainty
orquideas <-
  orquideas %>%
  mutate(coordinateUncertaintyInMeters = as.numeric
         (coordinateUncertaintyInMeters))%>%
  mutate(eventDate = as.Date(eventDate, "%Y-%m-%d"))
cat("Cantidad original de registros: ", nrow(orquideas))

orquideas <-
  orquideas %>%
  filter(!is.na(coordinateUncertaintyInMeters) & 
           coordinateUncertaintyInMeters <= 1000)
cat("Cantidad de registros después de descartar los de alta incertidumbre en la ubicación: ",
    nrow(orquideas))

## Species
orquideas <-
  orquideas %>%
  dplyr::filter(!is.na(species) & 
                  species != "")

# Limpieza datos ASP
asp <-
  asp %>%
  dplyr::filter(!is.na(descripcio) & 
                  descripcio != "Area Marina de Manejo")
asp <-
  asp %>%
  dplyr::filter(!is.na(descripcio) & 
                  descripcio != "Area marina protegida")

# Mapa
## Registros ASP
asp_registros <-
  asp %>%
  st_join(orquideas) %>%
  group_by(asp.x) %>%
  summarize(registros = n_distinct(gbifID, na.rm = TRUE)) %>%
  rename(asp = asp.x)