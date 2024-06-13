library(readxl)
library(tidyverse)
library(ade4)
library(aspe)
library(matrixStats)


#### Analyse 2010 #######

pressions2010 <- read_excel("20120801_SWB_pression_impact_V3.xls")

pressions2010_sel <- pressions2010 %>%
  select(EUSurfaceWaterBodyCode...5,
         CATEGORY,
         Natural,
         TypologyCode,
         "1.1 Point - UWWT_General",
         "1.2 Point â€“ Storm Overflows",
         "1.3 Point - IPPC plants (EPRTR)",
         "1.4 Point - Non IPPC",
         "1.5 Point - Other",
         "2 Diffuse Source",
         "2.1 Diffuse â€“ Urban run off",
         "2.2 Diffuse - Agricultural",
         "2.3 Diffuse â€“ Transport and infrastructure",
         "2.4 Diffuse â€“ Abandoned industrial sites",
         "2.5 Diffuse - Releases from facilities not connected to sewerag",
         "2.6 Diffuse - Other",
         "3.1 Abstraction - Agriculture",
         "3.10 Abstraction - Other",
         "3.2 Abstraction â€“ Public Water Supply",
         "3.3 Abstraction - Manufacturing",
         "3.4 Abstraction â€“ Electricity cooling",
         "3.5 Abstraction â€“ Fish forms",
         "3.6 Abstraction â€“ Hydro-energy not cooling",
         "3.8 Abstraction - Navigation",
         "3.9 Abstraction â€“ Water transfer",
         "4.1 Flowmorph â€“ Groundwater recharge",
         "4.2 Flowmorph â€“ Hydroelectric dam",
         "4.3 Flowmorph â€“ Water supply reservoir",
         "4.4 Flowmorph â€“ Flood defence dams",
         "4.5 FlowMorph - Water Flow Regulation",
         "4.6 FlowMorph - Diversions",
         "4.7 Flowmorph â€“ Locks",
         "4.8 FlowMorph - Weirs",
         "5.1 RiverManagement - Physical alteration of channel",
         "5.2 RiverManagement - Engineering activities",
         "5.3 RiverManagement - Agricultural enhancement",
         "5.4 RiverManagement - Fisheries enhancement",
         "5.5 RiverManagement - Land infrastructure",
         "5.6 RiverManagement â€“ Dredging",
         "6.1 TRACManagement - Estuarine/coastal dredging",
         "6.2 TRACManagement - Marine constructions",
         "6.3 TRACManagement - Land reclamation",
         "6.4 TRACManagement - Coastal sand suppletion (safety)",
         "6.5 TRACManagement - Tidal barrages",
         "7.1 OtherMorph - Barriers",
         "7.2 OtherMorph - Land sealing",
         "8.1 OtherPressures - Litter/fly tipping",
         "8.2 OtherPressures - Sludge disposal to sea",
         "8.3 OtherPressures - Exploitation/removal of animals/plants",
         "8.4 OtherPressures - Recreation",
         "8.5 OtherPressures - Fishing",
         "8.6 OtherPressures - Introduced species",
         "8.7 OtherPressures - Introduced disease",
         "8.8 OtherPressures - Climate change",
         "8.9 OtherPressures - Land drainage",
         "8.10 OtherPressures- Other",
         Acidification,
         "Altered habitats",
         "Contaminated sediments",
         "Contamination by priority substances",
         "Elevated temperatures",
         "Nutrient enrichment",
         "Organic enrichment",
         "Saline intrusion",
         "Other Significant Impacts") %>%
  rename(code_wb = EUSurfaceWaterBodyCode...5) %>%
  filter(CATEGORY == "RW",
         Natural == "Natural") %>% 
  mutate_all(~ ifelse(. == "présence", 1, .))



#Conversion en numeric de toutes les variables
indices_colonnes <- 5:65
pressions2010_sel <- pressions2010_sel %>%
  mutate_at(indices_colonnes, as.numeric)

# On aggrege les groupes de pressions

# Calculer la somme des colonnes commençant par "1."

pressions2010_sel_agr <- pressions2010_sel %>%
  mutate(
    `1. pollution ponctuelle` = as.numeric(rowSums(select(., starts_with("1."))) > 0),
    `2. Pollution diffuse` = as.numeric(rowSums(select(., starts_with("2."))) > 0),
    `3. hydrologie` = as.numeric(rowSums(select(., starts_with("3."))) > 0),
    `4. hydromorpho` = as.numeric(rowSums(select(., starts_with("4."))) > 0),
    `5. gestion eau` = as.numeric(rowSums(select(., starts_with("5."))) > 0),
    `6. gestion eau transition` = as.numeric(rowSums(select(., starts_with("6."))) > 0),
    `7. autres altération morpho` = as.numeric(rowSums(select(., starts_with("7."))) > 0),
    `8. autres pressions` = as.numeric(rowSums(select(., starts_with("8."))) > 0),
    année = 2010
    
  ) %>%
  select(année,
         code_wb,
         `1. pollution ponctuelle`, 
         `2. Pollution diffuse`, 
         `3. hydrologie`,
         `4. hydromorpho`,
         `5. gestion eau`,
         `6. gestion eau transition`,
         `7. autres altération morpho`,
         `8. autres pressions`
         ) 
  


######## Analyse 2016 #####

pressions2016 <- read_excel("20170404_Rapportage_DCE_2016_SWB.xlsx", sheet = "ESU_Pression_Impact")
caract_wb_2016 <- read_excel("20170404_Rapportage_DCE_2016_SWB.xlsx", sheet = "ESU_Caract")


caract_wb_2016_simp <- caract_wb_2016 %>% 
  select(surfaceWaterBodyCode,surfaceWaterBodyCategory,naturalAWBHMWB) %>% 
  filter(surfaceWaterBodyCategory == "RW",
         naturalAWBHMWB == "Natural")


pressions2016_sel <- pressions2016 %>%
  left_join(caract_wb_2016_simp) %>%
  mutate_all(~ replace(as.character(.), is.na(.), "0")) %>%
  mutate_all(~ replace(., . == "Yes", "1")) %>%
  rename(code_wb = surfaceWaterBodyCode) %>%
  mutate(année = 2016) %>%
  mutate(across(3:ncol(.), as.numeric))





# Supprimer les colonnes avec uniquement  des 0
# pressions2016_sel <- pressions2016_sel %>%
#  mutate_all(~ replace(as.numeric(.), is.na(.), 0)) %>%
#  select(-where(~ sum(.x) == 0, na.rm = TRUE))


# Aggrégation des pressions

pressions2016_sel_agr <- pressions2016_sel %>%
  mutate(
    `1. pollution ponctuelle` = as.numeric(rowSums(select(., starts_with("1."))) > 0),
    `2. Pollution diffuse` = as.numeric(rowSums(select(., starts_with("2."))) > 0),
    `3. hydrologie` = as.numeric(rowSums(select(., starts_with("3."))) > 0),
    `4. hydromorpho` = as.numeric(rowSums(select(., starts_with("4."))) > 0),
    `5. gestion eau` = as.numeric(rowSums(select(., starts_with("5."))) > 0),
    `6. gestion eau transition` = as.numeric(rowSums(select(., starts_with("6."))) > 0),
    `7. autres altération morpho` = as.numeric(rowSums(select(., starts_with("7."))) > 0),
    `8. autres pressions` = as.numeric(rowSums(select(., starts_with("8."))) > 0)
    
  ) %>%
  select(année,
         code_wb,
         `1. pollution ponctuelle`, 
         `2. Pollution diffuse`, 
         `3. hydrologie`,
         `4. hydromorpho`,
         `5. gestion eau`,
         `6. gestion eau transition`,
         `7. autres altération morpho`,
         `8. autres pressions`
  )


###### Analyse 2022 #######

pressions2022 <- read_excel("SWB_2024-02-07T10-00-48.xlsx", sheet = "Pressure")

pressions2022_sel <- pressions2022 %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = swSignificantPressureType, values_from = value, values_fill = 0) %>% 
  select(-swSignificantPressureOther,
         -euRBDCode) %>%
  rename(code_wb = euSurfaceWaterBodyCode) %>%
  mutate(année = 2022)

pressions2022_sel_agr <- pressions2022_sel %>%
  mutate(
    `1. pollution ponctuelle` = as.numeric(rowSums(select(., starts_with("1."))) > 0),
    `2. Pollution diffuse` = as.numeric(rowSums(select(., starts_with("2."))) > 0),
    `3. hydrologie` = as.numeric(rowSums(select(., starts_with("3."))) > 0),
    `4. hydromorpho` = as.numeric(rowSums(select(., starts_with("4."))) > 0),
    `5. gestion eau` = as.numeric(rowSums(select(., starts_with("5."))) > 0),
    `6. gestion eau transition` = as.numeric(rowSums(select(., starts_with("6."))) > 0),
    `7. autres altération morpho` = as.numeric(rowSums(select(., starts_with("7."))) > 0),
    `8. autres pressions` = as.numeric(rowSums(select(., starts_with("8."))) > 0)
    
  ) %>%
  select(année,
         code_wb,
         `1. pollution ponctuelle`, 
         `2. Pollution diffuse`, 
         `3. hydrologie`,
         `4. hydromorpho`,
         `5. gestion eau`,
         `6. gestion eau transition`,
         `7. autres altération morpho`,
         `8. autres pressions`
  )

#### Tableau total #####

sel_wb <- pressions2010_sel_agr %>% 
  na.omit(.) %>% 
  inner_join(pressions2016_sel_agr, by = "code_wb") %>% 
  inner_join(pressions2022_sel_agr, by = "code_wb")

pressions_tot <- bind_rows(pressions2010_sel_agr, pressions2016_sel_agr, pressions2022_sel_agr)

pressions_tot_sel <- pressions_tot %>% 
  select(année,
         code_wb,
         `1. pollution ponctuelle`, 
         `2. Pollution diffuse`, 
         `3. hydrologie`,
         `4. hydromorpho`,
         `5. gestion eau`,
         `6. gestion eau transition`,
         `7. autres altération morpho`,
         `8. autres pressions`)

summary(pressions_tot_sel)

# les pressions 6, 7 et 8 sont à 0, donc on les vire et on ne garde que les WB ayant été évalués au moins 2 ans

pressions_tot_sel <- pressions_tot %>% 
  select(année,
         code_wb,
         `1. pollution ponctuelle`, 
         `2. Pollution diffuse`, 
         `3. hydrologie`,
         `4. hydromorpho`,
         `5. gestion eau`) %>% 
  na.omit(.) %>% 
  group_by(code_wb) %>%
  filter(n() == 3) %>%
  ungroup()

##### Analyse tableau total ####

m_press_tot <- pressions_tot_sel [,3:7]
m_press_poll <- pressions_tot_sel[,3:4]
m_press_phys <- pressions_tot_sel[,5:7]
année <- as.factor(pressions_tot_sel$année)
wb <- as.factor(pressions_tot_sel$code_wb)

## Analyse toutes pressions


acp_tot <- dudi.pca(m_press_tot, scannf = F, nf = 2, center = T, scale = F)
s.arrow(acp_tot$co)
s.label(acp_tot$li)

scores_wb_tot <- bind_cols(année,wb,acp_tot$li)
colnames(scores_wb_tot)[1] <- "année"
colnames(scores_wb_tot)[2] <- "wb"

# On calcule les pentes des scores de l'axe1 au cours des années pour chaque wb et
# la moyenne des scores pour chaque wb pour avoir un indice de pression

resum_pressions_tot <- scores_wb_tot %>%
  group_by(wb) %>%
  summarize(
    tendance = lm(Axis1 ~ année)$coefficients[2],
    indice_pressions = mean(Axis1)
  )


ggplot(resum_pressions_tot, aes(x = indice_pressions)) +
  geom_histogram(binwidth = 0.1) +  # spécifiez la largeur des bacs selon vos préférences
  labs(x = "Indice de pression", y = "Fréquence") +  # ajoutez des étiquettes d'axes
  ggtitle("Histogramme de l'indice de pression")  # ajoutez un titre au graphique

ggplot(resum_pressions_tot, aes(x = tendance)) +
  geom_histogram(binwidth = 0.1) +  # spécifiez la largeur des bacs selon vos préférences
  labs(x = "Pente", y = "Fréquence") +  # ajoutez des étiquettes d'axes
  ggtitle("Histogramme de la pente")  # ajoutez un titre au graphique



## Même chose pour pollutions uniquement

acp_poll <- dudi.pca(m_press_poll,scannf = F, nf = 2, center = T, scale = F)
s.arrow(acp_poll$co)
s.label(acp_poll$li)

scores_wb_poll <- bind_cols(année,wb,acp_poll$li)
colnames(scores_wb_poll)[1] <- "année"
colnames(scores_wb_poll)[2] <- "wb"

resum_pressions_poll <- scores_wb_poll %>%
  group_by(wb) %>%
  summarize(
    tendance_poll = lm(Axis1 ~ année)$coefficients[2],
    indice_pressions_poll1 = mean(Axis1),
    indice_pressions_poll2 = mean(Axis2)
  )


ggplot(resum_pressions_poll, aes(x = indice_pressions_poll2)) +
  geom_histogram(binwidth = 0.1) +  # spécifiez la largeur des bacs selon vos préférences
  labs(x = "Indice de pression", y = "Fréquence") +  # ajoutez des étiquettes d'axes
  ggtitle("Histogramme de l'indice de pression chimique")  # ajoutez un titre au graphique

ggplot(resum_pressions_poll, aes(x = tendance_poll)) +
  geom_histogram(binwidth = 0.1) +  # spécifiez la largeur des bacs selon vos préférences
  labs(x = "Pente", y = "Fréquence") +  # ajoutez des étiquettes d'axes
  ggtitle("Evolution des pressions chimiques")  # ajoutez un titre au graphique


## Même chose pour les pressions physiques

acp_phys <- dudi.pca(m_press_phys,scannf = F, nf = 2, center = T, scale = F)
s.arrow(acp_phys$co)
s.label(acp_phys$li)

scores_wb_phys <- bind_cols(année,wb,acp_phys$li)
colnames(scores_wb_phys)[1] <- "année"
colnames(scores_wb_phys)[2] <- "wb"

resum_pressions_phys <- scores_wb_phys %>%
  group_by(wb) %>%
  summarize(
    tendance_phys = lm(Axis1 ~ année)$coefficients[2],
    indice_pressions_morpho = mean(Axis1),
    indice_pressions_hydro = mean(Axis2)
  )


ggplot(resum_pressions_phys, aes(x = indice_pressions_phys)) +
  geom_histogram(binwidth = 0.1) +  # spécifiez la largeur des bacs selon vos préférences
  labs(x = "Indice de pression", y = "Fréquence") +  # ajoutez des étiquettes d'axes
  ggtitle("Histogramme de l'indice de pression physique")  # ajoutez un titre au graphique

ggplot(resum_pressions_phys, aes(x = tendance_phys)) +
  geom_histogram(binwidth = 0.1) +  # spécifiez la largeur des bacs selon vos préférences
  labs(x = "Pente", y = "Fréquence") +  # ajoutez des étiquettes d'axes
  ggtitle("Evolution des pressions physiques")  # ajoutez un titre au graphique

### On sauve les tableaux finaux

save(pressions_tot_sel, resum_pressions_tot, resum_pressions_poll, resum_pressions_phys, file="etatdeslieux_pressions.RData")

