library(tidyverse)
library(spdplyr)
library(sf)
library(cartography)
library(osmdata)
library(osrm)
library(patchwork)
library(viridis)
library(scales)
options(osrm.server="http://0.0.0.0:5000/")

#### Initialisation des bases de données ####

###                                   ###
### Base des équipements de l'Insee   ###
### https://www.insee.fr/fr/statistiques/3568629?sommaire=3568656
###                                   ###

equipements<-read_delim("bpe19_ensemble_xy.csv",";", escape_double = FALSE, trim_ws = TRUE)

###                                   ###
###       Géographie des IRIS         ###
### https://geoservices.ign.fr/documentation/diffusion/telechargement-donnees-libres.html#contoursiris
###                                   ###

iris<-st_read("IRIS/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2019/CONTOURS-IRIS.shp")
iris_info<-readxl::read_excel("base-ic-evol-struct-pop-2016.xls",skip = 5)

#### Initialisation du serveur ####

###                                   ###
###       Calculs des trajets         ###
### 1. osrm-extract france-latest.osm.pbf -p ~/Sites/dev/osrm-backend/profiles/foot.lua
### 2. osrm-contract france-latest.osrm
### 3. osrm-routed france-latest.osrm --max-table-size 10000
###                                   ###

#### Correspondances ####

###                                   ###
###      Villes et limitrophes        ###
###  (celles de plus de 200 000 hab.) ###
###                                   ###

villes<-tribble(
  ~ville,~code_insee,~limitrophes,
  "Paris",list(75101:75120),list(75101:75120,"92024","92044","92051","93070","93066","93001","93055","93061","93045","92062","92073","92064","93006","93048","94067","94080","94033","94052","92012","92040","92075","92046","92049","94037","94043","94042","94069","94018","94041"),
  "Marseille",list(13201:13216),list(13201:13216,"13117","13054","13088","13106","13107","13190","13005","13070","13022","13119","13071","13075"),
  "Lyon",list(69381:69389),list(69381:69389,"69194","69040","69081","69063","69244","69089","69202","69029","69142","69149","69152","69191","69034","69199","69259","69266"),
  "Toulouse","31555",list("31555","31182","31056","31069","31770","31557","31445","31157","31433","31254","31446","31022","31575","31561","31044","31506","31282","31411"),
  "Nice","06088",list("06088","06064","06006","06060","06114","06147","06149","06122","06123","06059","06159","06046","06031"),
  "Nantes","44109",list("44109","44114","44209","44162","44020","44190","44215","44035","44143","44026","44172","44009"),
  "Montpellier","34172",list("34172","34154","34169","34116","34077","34240","34123","34217","34057","34270","34337","34129"),
  "Strasbourg","67482",list("67482","67218","67343","67447","67118","67043","67519","67267","67365","67131"),
  "Bordeaux","33063",list("33063","33075","33069","33162","33249","33281","33318","33039","33056","33032","33119","33167","33522"),
  "Lille","59350",list("59350","59328","59356","59368","59410","59457","59470","59128","59196","59195","59566","59360","59220","59507","59527","59378","59648","59346","59009"),
  "Rennes","35238",list("35238","35210","35189","35353","35240","35281","35278","35206","35051","35055","35024")
)

###                                   ###
###       Communes limitrophes        ###
###          (merci Victor)           ###
###                                   ###

st_read("~/Documents/SIG/COG_2019/ADE-COG_2-0_SHP_LAMB93_FR/COMMUNE.shp", quiet=T)
Voisins<-st_touches(ToutesLesCommunesdeFrance,ToutesLesCommunesdeFrance,sparse = T)
Voisins<-as.data.frame(Voisins)
Voisins$COM<-ToutesLesCommunesdeFrance$INSEE_COM[Voisins$row.id]
Voisins$COMVOISIN<-ToutesLesCommunesdeFrance$INSEE_COM[Voisins$col.id]
Voisins<-Voisins[,3:4]
colnames(Voisins)<-c("INSEE_COM","INSEE_COM_VOISIN")
write.csv(Voisins, "ToutesLesCommunesetTousLeursVoisins.csv",fileEncoding = "UTF-8", row.names = F)

limitrophes<-read_csv("limitrophes.csv")

###                                   ###
###    Correspondance équipements     ###
###                                   ###

liste_equipements<-tribble(
  ~nom,~id,
  "cinema","F303",
  "piscine","F101",
  "sport",list("F113","F121","F111"),
  "ecole",list("C101","C102","C104","C105"),
  "police",list("A101","A104"),
  "sante",list("D201","D106"),
  "emploi",list("A122","A123"),
  "commerce",list("B101","B102","B201","B202")
)

#### Fonctions ####

###                                   ###
###       Récupération des durées     ###
###                                   ###

recupDureesEquip<-function(iris_id,equip_id,communes_id) {
  print(paste0("Analyse de l'IRIS : ",iris_id))
  
  source<-iris %>% 
    filter(CODE_IRIS == iris_id) %>%
    st_sample(150) %>%
    as_Spatial() %>%
    spTransform(CRSobj="+init=epsg:4326")
  
  destination<-equipements %>%
    filter(DEPCOM %in% communes_id & TYPEQU %in% equip_id & QUALITE_XY %in% c("Acceptable","Bonne","Mauvaise"))
  
  ### Au cas où aucun équipement ne soit dans la commune, ni dans les limitrophes, on regarde dans les départements
  if(nrow(destination) == 0) {
    dpt<-lapply(communes_id, function(x){substr(x,0,2)})
    
    destination<-equipements %>%
      filter(DEP %in% dpt & TYPEQU %in% equip_id & QUALITE_XY %in% c("Acceptable","Bonne","Mauvaise"))
  }
  
  coordinates(destination) <- ~ LAMBERT_X + LAMBERT_Y
  proj4string(destination)<-CRS("+init=epsg:2154")
  
  dist<-osrmTable(src=source,dst=destination)
  
  duree<-as.data.frame(dist$durations) %>%
    mutate(DUREE = as.double(pmap(.,min,na.rm=TRUE))) %>%
    mutate(CODE_IRIS=iris_id) %>%
    select(CODE_IRIS,DUREE)
  
  return(duree)
}

###                                   ###
###       Calculer les durées         ###
###   Toutes villes et tous services  ###
###                                   ###

quartdheure<-function(nom_ville) {
  
  ### VARIABLES
  code_insee_ville <- villes %>% filter(ville == nom_ville) %>% select(code_insee) %>% unnest(cols = c(code_insee)) %>% unlist()
  code_insee_limitrophes <- villes %>% filter(ville == nom_ville) %>% select(limitrophes) %>% unnest(cols = c(limitrophes)) %>% unlist()
  iris_ville<- iris %>% filter(INSEE_COM %in% code_insee_ville) %>% select(CODE_IRIS) %>% st_drop_geometry %>% distinct() %>% unlist()
  
  map(code_insee_ville,function(x) {
    map_df(c("cinema","piscine","sport","ecole","police","sante","emploi","commerce"),
           function(y) {
             equip_ids<-liste_equipements %>% filter(nom == y) %>% unnest(cols = c(id)) %>% unlist()
             iris_ids<-iris %>% filter(INSEE_COM %in% x) %>% select(CODE_IRIS) %>% st_drop_geometry %>% distinct() %>% unlist()
             map_df(iris_ids,function(z) {
               recupDureesEquip(z,equip_ids,code_insee_limitrophes) %>%
                 mutate(LIEU = y)
             })
           }) %>%
      left_join(iris_info %>% select(CODE_IRIS = IRIS,LIBIRIS,LIBCOM,P16_POP)) %>%
      write_csv(paste0("/Volumes/LaCie/quart_dheure/results/results_",x,".csv"))
  })
  
  durees<-map_df(code_insee_ville,function(x) {
    read_csv(paste0("/Volumes/LaCie/quart_dheure/results/results_",x,".csv"))
  })
  
  return(durees)
}

quartdheure_code<-function(code_insee_ville) {
  
  ### VARIABLES
  code_insee_limitrophes <- limitrophes %>% filter(INSEE_COM == code_insee_ville) %>% unlist()
  iris_ville<- iris %>% filter(INSEE_COM %in% code_insee_ville) %>% select(CODE_IRIS) %>% st_drop_geometry %>% distinct() %>% unlist()
  
  map(code_insee_ville,function(x) {
    map_df(c("cinema","piscine","sport","ecole","police","sante","emploi","commerce"),
           function(y) {
             equip_ids<-liste_equipements %>% filter(nom == y) %>% unnest(cols = c(id)) %>% unlist()
             iris_ids<-iris %>% filter(INSEE_COM %in% x) %>% select(CODE_IRIS) %>% st_drop_geometry %>% distinct() %>% unlist()
             map_df(iris_ids,function(z) {
               recupDureesEquip(z,equip_ids,code_insee_limitrophes) %>%
                 mutate(LIEU = y)
             })
           }) %>%
      left_join(iris_info %>% select(CODE_IRIS = IRIS,LIBIRIS,LIBCOM,P16_POP)) %>%
      write_csv(paste0("/Volumes/LaCie/quart_dheure/results/results_",x,".csv"))
  })
}

readDurees<-function(nom_ville) {
  code_insee_ville <- villes %>% filter(ville == nom_ville) %>% select(code_insee) %>% unnest(cols = c(code_insee)) %>% unlist()
  
  durees<-map_df(code_insee_ville,function(x) {
    read_csv(paste0("/Volumes/LaCie/quart_dheure/results/results_",x,".csv"))
  })
  
  return(durees)
}

readDurees_code<-function(x) {
  return(read_csv(paste0("/Volumes/LaCie/quart_dheure/results/results_",x,".csv")))
}

###                                   ###
###              Résumé               ###
###                                   ###

resume<-function(ville) {
  ville %>%
    group_by(CODE_IRIS,LIEU,P16_POP) %>% 
    summarise(moyenne = mean(DUREE,na.rm = TRUE)) %>%
    mutate(plus15 = moyenne > 15) %>%
    group_by(LIEU,plus15) %>%
    summarise(pop = sum(P16_POP)) %>%
    mutate(freq = pop/sum(pop)*100) %>%
    filter(plus15 == TRUE)
  
  ville %>% 
    group_by(LIEU,P16_POP) %>%
    summarise(moyenne = mean(DUREE,na.rm=TRUE)) %>%
    summarise(duree_moy = weighted.mean(moyenne,P16_POP))
}

###                                   ###
### Dessiner un compte-rendu visuel   ###
###                                   ###

dessinerCR_equip<-function(nom_ville,equipement,durees) {
  
  equip_id<-liste_equipements %>% filter(nom == equipement) %>% unnest(cols = c(id)) %>% unlist()
  commune_id<-villes %>% filter(ville == nom_ville) %>% select(code_insee) %>% unnest(cols = c(code_insee)) %>% unlist()
  code_insee_limitrophes <- villes %>% filter(ville == nom_ville) %>% select(limitrophes) %>% unnest(cols = c(limitrophes)) %>% unlist()
  
  destination<-equipements %>%
    filter(DEPCOM %in% code_insee_limitrophes & TYPEQU %in% equip_id & QUALITE_XY %in% c("Acceptable","Bonne","Mauvaise"))
  
  coordinates(destination) <- ~ LAMBERT_X + LAMBERT_Y
  proj4string(destination)<-CRS("+init=epsg:2154")
  
  iris %>% 
    inner_join(durees %>%
                 filter(LIEU == equipement) %>%
                 mutate(CODE_IRIS = as.character(CODE_IRIS)) %>%
                 group_by(CODE_IRIS) %>%
                 summarise(moyenne = mean(DUREE,na.rm=TRUE)) %>%
                 select(CODE_IRIS,moyenne)) %>% 
    choroLayer(var="moyenne",legend.title.txt=equipement)
  

  plot(destination,pch=16,add=TRUE)
}

dessinerCR<-function(nom_ville,durees) {
  
  commune_id<-villes %>% filter(ville == nom_ville) %>% select(code_insee) %>% unnest(cols = c(code_insee)) %>% unlist()
  code_insee_limitrophes <- villes %>% filter(ville == nom_ville) %>% select(limitrophes) %>% unnest(cols = c(limitrophes)) %>% unlist()
  
  iris %>% 
    inner_join(durees %>%
                 mutate(CODE_IRIS = as.character(CODE_IRIS)) %>%
                 group_by(CODE_IRIS) %>%
                 summarise(moyenne = mean(DUREE,na.rm=TRUE)) %>%
                 select(CODE_IRIS,moyenne)) %>% 
    choroLayer(var="moyenne")
}

dessinerCR_code<-function(commune_id,durees) {
  
  code_insee_limitrophes <- limitrophes %>% filter(INSEE_COM == commune_id) %>% unlist()
  
  iris %>% 
    inner_join(durees %>%
                 mutate(CODE_IRIS = as.character(CODE_IRIS)) %>%
                 group_by(CODE_IRIS) %>%
                 summarise(moyenne = mean(DUREE,na.rm=TRUE)) %>%
                 select(CODE_IRIS,moyenne)) %>% 
    choroLayer(var="moyenne")
}

dessinerCR_equip_code<-function(commune_id,equipement,durees) {
  
  equip_id<-liste_equipements %>% filter(nom == equipement) %>% unnest(cols = c(id)) %>% unlist()
  code_insee_limitrophes <- limitrophes %>% filter(INSEE_COM == commune_id) %>% unlist()
  
  destination<-equipements %>%
    filter(DEPCOM %in% code_insee_limitrophes & TYPEQU %in% equip_id & QUALITE_XY %in% c("Acceptable","Bonne","Mauvaise"))
  
  coordinates(destination) <- ~ LAMBERT_X + LAMBERT_Y
  proj4string(destination)<-CRS("+init=epsg:2154")
  
  iris %>% 
    inner_join(durees %>%
                 filter(LIEU == equipement) %>%
                 mutate(CODE_IRIS = as.character(CODE_IRIS)) %>%
                 group_by(CODE_IRIS) %>%
                 summarise(moyenne = mean(DUREE,na.rm=TRUE)) %>%
                 select(CODE_IRIS,moyenne)) %>% 
    choroLayer(var="moyenne",legend.title.txt=equipement)
  
  
  plot(destination,pch=16,add=TRUE)
}

###                                   ###
###    Durées pour chaque ville       ###
###                                   ###

quartdheure("Paris")
quartdheure("Marseille")
quartdheure("Lyon")
quartdheure("Toulouse")
quartdheure("Nice")
quartdheure("Nantes")
quartdheure("Montpellier")
quartdheure("Strasbourg")
quartdheure("Bordeaux")
quartdheure("Lille")
quartdheure("Rennes")

villes_quartdheure<-bind_rows(
  readDurees("Paris") %>% mutate(CODE_IRIS = as.character(CODE_IRIS),LIBCOM = "Paris"),
  readDurees("Marseille") %>% mutate(CODE_IRIS = as.character(CODE_IRIS),LIBCOM = "Marseille"),
  readDurees("Lyon") %>% mutate(CODE_IRIS = as.character(CODE_IRIS),LIBCOM = "Lyon"),
  readDurees("Toulouse") %>% mutate(CODE_IRIS = as.character(CODE_IRIS)),
  readDurees("Nice") %>% mutate(CODE_IRIS = as.character(CODE_IRIS)),
  readDurees("Nantes") %>% mutate(CODE_IRIS = as.character(CODE_IRIS)),
  readDurees("Montpellier") %>% mutate(CODE_IRIS = as.character(CODE_IRIS)),
  readDurees("Strasbourg") %>% mutate(CODE_IRIS = as.character(CODE_IRIS)),
  readDurees("Bordeaux") %>% mutate(CODE_IRIS = as.character(CODE_IRIS)),
  readDurees("Lille") %>% mutate(CODE_IRIS = as.character(CODE_IRIS)),
  readDurees("Rennes") %>% mutate(CODE_IRIS = as.character(CODE_IRIS)))

villes_quartdheure %>%
  write_csv("/Volumes/LaCie/quart_dheure/results.csv")

villes_quartdheure<-read_csv("/Volumes/LaCie/quart_dheure/results.csv") %>%
  mutate(CODE_IRIS = as.character(CODE_IRIS))

### Divers éclairages

villes_quartdheure %>%
  group_by(LIBCOM,CODE_IRIS,LIEU,P16_POP) %>% 
  summarise(moyenne = mean(DUREE,na.rm = TRUE)) %>%
  mutate(plus15 = moyenne > 15) %>%
  group_by(LIEU,plus15) %>%
  summarise(pop = sum(P16_POP,na.rm = TRUE)) %>%
  mutate(freq = pop/sum(pop,na.rm=TRUE)*100) %>%
  filter(plus15 == TRUE) %>%
  select(-plus15,-pop) #%>%
  pivot_wider(names_from = LIEU,values_from = freq) 

### Moyennes pour les grandes villes

villes_quartdheure %>% 
  group_by(LIEU,LIBCOM,P16_POP) %>%
  summarise(moyenne = mean(DUREE,na.rm=TRUE)) %>%
  summarise(duree_moy = weighted.mean(moyenne,P16_POP)) %>%
  summarise(moyenne = mean(duree_moy,na.rm = TRUE))
  pivot_wider(names_from = LIEU,values_from = duree_moy)

### Moyennes par ville

villes_quartdheure %>% 
  filter(LIEU != "police") %>%
  group_by(LIBCOM,LIEU,P16_POP) %>%
  summarise(moyenne = mean(DUREE,na.rm=TRUE)) %>%
  summarise(duree_moy = weighted.mean(moyenne,P16_POP)) %>%
  summarise(moyenne = mean(duree_moy,na.rm = TRUE))

villes_quartdheure %>% 
  group_by(LIBCOM,LIBIRIS) %>%
  summarise(moyenne = mean(DUREE,na.rm=TRUE),
            population = mean(P16_POP,na.rm = TRUE)) %>%
  arrange(desc(moyenne))

dessinerCR("Marseille",villes_quartdheure %>% filter(LIBCOM == "Marseille"))
dessinerCR_equip("Marseille","ecole",villes_quartdheure %>% filter(LIBCOM == "Marseille"))

lapply(c("cinema","piscine","sport","ecole","police","sante","emploi","commerce"), function(x) {
  dessinerCR_equip("Rennes",x,villes_quartdheure %>% filter(LIBCOM == "Rennes"))
  dev.print(device = svg, file = paste0("rennes_export_",x,".svg"), width = 600)
})


villes_quartdheure %>%
  filter(LIBCOM == "Paris" & LIEU != "police") %>% 
  mutate(LIEU = case_when(
    LIEU == "cinema" ~ "Cinéma",
    LIEU == "commerce" ~ "Commerces",
    LIEU == "ecole" ~ "Écoles",
    LIEU == "emploi" ~ "Pôle emploi",
    LIEU == "piscine" ~ "Piscines",
    LIEU == "sante" ~ "Médecins généralistes\net urgences",
    LIEU == "sport" ~ "Gymnases et stades",
  )) %>%
  group_by(LIEU,CODE_IRIS) %>%
  summarise(moyenne = mean(DUREE,na.rm=TRUE)) %>%
  left_join(iris) %>%
  ggplot() +
    geom_sf(aes(fill=moyenne,geometry=geometry),size=0) +
    scale_fill_viridis(limits=c(0,60),breaks=c(0,15,30,45),option = "magma",oob = squish,direction = -1) +
    facet_wrap(~LIEU) +
    theme_minimal() +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "bottom") +
    guides(fill=guide_legend("Trajet à pied (min)")) +
    plot_annotation(title="Des équipements à six minutes en moyenne à pied à Paris",
                    subtitle = "Temps moyen de trajet, en minutes, par équipement et par IRIS à Paris",
                    caption = "La Gazette des communes – Données : Insee, OpenStreetMap",
                    theme = theme(plot.title = element_text(family = "Roboto",color = "black", face = "bold", size = 16)))
  

###                                   ###
###      Durées par département       ###
###                                   ###

limitrophes %>%
  filter(startsWith(INSEE_COM,"60")) %>%
  select(INSEE_COM) %>% 
  distinct() %>%
  unlist() %>%
  map(quartdheure_code)

limitrophes %>%
  filter(startsWith(INSEE_COM,"91")) %>%
  select(INSEE_COM) %>% 
  distinct() %>%
  unlist() %>%
  map_df(readDurees_code) %>%
  write_csv("/Volumes/LaCie/quart_dheure/results_91.csv")


###                                   ###
###      Durées pour Dijon et St-É    ###
###                                   ###

quartdheure_code("21231")
quartdheure_code("42218")

map_df(c("21231","42218",69381:69389),readDurees_code) %>% 
  mutate(CODE_IRIS = as.character(CODE_IRIS))


dessinerCR_code("42218",readDurees_code("42218"))
dessinerCR_equip_code("21231","commerce",readDurees_code("21231"))
