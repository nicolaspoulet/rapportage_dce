library(sf)
library(tidyverse)

load("D:/Documents/Analyses_stat/En_cours/rapportage_dce/etatdeslieux_pressions.RData")
load("~/Analyses_stat/En_cours/aspe_habitat/tables_sauf_mei_2024_01_22_12_13_56.RData")

couche_me <- st_read(dsn = "D:/Documents/SIG/Masses d'eau rapportage 2019/MasseDEauRiviere_FXX.shp", layer = "MasseDEauRiviere_FXX")

# Convertir les données des stations en objets sf
station_sf <- st_as_sf(station, coords = c("sta_coordonnees_x", "sta_coordonnees_y"), crs = 2154) # Remplacez 2154 par votre CRS si différent

# Effectuer la jointure spatiale
station_me <- st_join(station_sf, couche_me, join = st_intersects)

# Sélectionner les colonnes d'intérêt
station_me <- station_me %>% select(sta_id, sta_code_sandre, CdEuMasseD)

# Vérifier le résultat
head(station_me)

# Convertir les données des stations en objets sf
station_sf <- st_as_sf(station, coords = c("sta_coordonnees_x", "sta_coordonnees_y"), crs = 2154) # Remplacez 2154 par votre CRS si différent

# Effectuer la jointure spatiale
station_me <- st_join(station_sf, couche_me, join = st_nearest_feature)

# Extraire les colonnes d'intérêt
result <- station_me %>%
  select(sta_id, CdEuMasseD)

# Afficher les premières lignes du résultat
head(result)

# Renommer la colonne 'wb' de 'resum_pressions_poll' pour correspondre à 'CdEuMasseD'
resum_pressions_poll <- resum_pressions_poll %>%
  rename(CdEuMasseD = wb)

# Effectuer la jointure
station_me_poll <- result %>%
  left_join(resum_pressions_poll, by = "CdEuMasseD") %>% 
  drop_na()

write.csv()