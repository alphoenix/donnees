## Imports des librairies

library(haven)
library(plyr)
library(dplyr)
library(reshape)
library(devtools)
library(data.table)
library(cartography)
library(rgdal)
library(SpatialPosition)
library(xlsx)

library(osrm)
options(osrm.server="http://0.0.0.0:5000/")

## Chargement de la base sur les établissements
devtools::install_github('jomuller/finess',ref='47de6e2')
data(finess_geo, package = 'finess')

## Ajout des données de la Statistique annuelle des établissements (base statistique) 2013-2018
## http://www.data.drees.sante.gouv.fr/ReportFolders/reportFolders.aspx

datp2013<-read_sas("files/perinat_p_2013a.sas7bdat")
datp2014<-read_sas("files/perinat_p_2014a.sas7bdat")
datp2015<-read_sas("files/perinat_p_2015a.sas7bdat")
datp2016<-read_sas("files/perinat_p_2016a.sas7bdat")
datp2017<-read_sas("files/perinat_p_2017r.sas7bdat")
datp2018<-read_sas("files/perinat_p_2018.sas7bdat")

## Ajout du nombre de médecins participant à l'activité d'IVG (SAE/MIVG) mais les chiffres sont mauvais

mivg2013<-subset(datp2013,PERSO=="MIVG")
mivg2014<-subset(datp2014,PERSO=="MIVG")
mivg2015<-subset(datp2015,PERSO=="MIVG")
mivg2016<-subset(datp2016,PERSO=="MIVG")
mivg2017<-subset(datp2017,PERSO=="MIVG")
mivg2018<-subset(datp2018,PERSO=="MIVG")

mivg<-rbind(mivg2013,mivg2014,mivg2015,mivg2016,mivg2017,mivg2018)

names(mivg)[names(mivg) == "EFFPL"] <- "EFFPL_IVG"
names(mivg)[names(mivg) == "EFFPA"] <- "EFFPA_IVG"
names(mivg)[names(mivg) == "ETP"] <- "ETP_IVG"
mivg<-subset(mivg,select=-c(PERSO,GAR,GARDED,ASTDED,AST,BOR))

## Ajout du nombre de médecins gynécos (SAE/M2050)

mgy_2013<-subset(datp2013,PERSO=="M2050")
mgy_2014<-subset(datp2014,PERSO=="M2050")
mgy_2015<-subset(datp2015,PERSO=="M2050")
mgy_2016<-subset(datp2016,PERSO=="M2050")
mgy_2017<-subset(datp2017,PERSO=="M2050")
mgy_2018<-subset(datp2018,PERSO=="M2050")

mgy<-rbind(mgy_2013,mgy_2014,mgy_2015,mgy_2016,mgy_2017,mgy_2018)

names(mgy)[names(mgy) == "EFFPL"] <- "EFFPL_GY"
names(mgy)[names(mgy) == "EFFPA"] <- "EFFPA_GY"
names(mgy)[names(mgy) == "ETP"] <- "ETP_GY"
mgy<-subset(mgy,select=-c(PERSO,GAR,GARDED,ASTDED,AST,BOR))

mivg<-merge(mivg,mgy,by.x=c("AN","FI_EJ","FI"),by.y=c("AN","FI_EJ","FI"),all.x=TRUE,all.y=TRUE)

## Nombre d'actes IVG et accouchements (PERINAT / SAE)

dat2013<-read_sas("files/perinat_2013r.sas7bdat")
dat2014<-read_sas("files/perinat_2014r.sas7bdat")
dat2015<-read_sas("files/perinat_2015r.sas7bdat")
dat2016<-read_sas("files/perinat_2016r.sas7bdat")
dat2017<-read_sas("files/perinat_2017r.sas7bdat")
dat2018<-read_sas("files/perinat_2018.sas7bdat")

nivg<-rbind.fill(dat2014,dat2013,dat2015,dat2016,dat2017,dat2018)

## FICHIER FINAL

ivg<-merge(mivg,nivg,by.x=c("AN","FI_EJ","FI"),by.y=c("AN","FI_EJ","FI"),all.x=TRUE,all.y=TRUE)

## Ajout des infos de la base Finess
ivg<-merge(ivg,finess_geo,by.x="FI",by.y="nofinesset",all.x=TRUE)

## AJOUTS COLONNES

# Nombre d'accouchements (enfants morts-nés compris)

ivg$ACC<-ivg$ACCMU+ivg$ACCUN

# Département

ivg$DPT<-substr(ivg$FI,start=1,stop=2)
dpt_reg<-read.csv("files/departement2019.csv",sep=",",col.names=c("dep","reg","cheflieu","tncc","ncc","nccenr","libelle"))
ivg<-merge(ivg,dpt_reg,by.x="DPT",by.y="dep",all.x=TRUE)

## Renseigner les établissements sans info

finess_old<-read.csv("files/finess_old.csv",sep=";",col.names=c("nofinesset","nofinessej","rs","rslongue","complrs","compldistrib","numvoie","typvoie","voie","compvoie","lieuditbp","region","libregion","departement","libdepartement","cog","codepostal","libelle_routage","ligneacheminement","telephone","telecopie","categetab","libcategetab","liblongcategetab","categretab","libcategretab","siret","codeape","libcodeape","mft","libmft","liblongmft","sph","libsph","numen","coordx","coordy","sourcegeocod","dategeocod","dateautor","dateouvert","datemaj","lat","lon"),stringsAsFactors=FALSE,colClasses=c(rep("character",44)))
ivg$rs[is.na(ivg$lat)]<-finess_old$rs[match(ivg$FI,finess_old$nofinesset)][which(is.na(ivg$lat))]
ivg$departement[is.na(ivg$lat)]<-finess_old$departement[match(ivg$FI,finess_old$nofinesset)][which(is.na(ivg$lat))]
ivg$lat[is.na(ivg$lat)]<-finess_old$lat[match(ivg$FI,finess_old$nofinesset)][which(is.na(ivg$lat))]
ivg$lon[is.na(ivg$lon)]<-finess_old$lon[match(ivg$FI,finess_old$nofinesset)][which(is.na(ivg$lon))]

## EXPORT

write.csv(ivg,file="ivg.csv",row.names = FALSE)

## Nettoyage

rm(mivg2013,mivg2014,mivg2015,mivg2016,mivg2017,mivg2018)
rm(mgy_2013,mgy_2014,mgy_2015,mgy_2016,mgy_2017,mgy_2018)
rm(datp2013,datp2014,datp2015,datp2016,datp2017,datp2018)
rm(dat2013,dat2014,dat2015,dat2016,dat2017,dat2018)
rm(nivg,mivg,mgy)

## Nb. établissements 12-14 (SAE)

ivg %>% filter(IVG1214 > 0 & AN == "2018") %>% nrow
ivg %>% filter(IVG-IVGME > 0 & AN == "2018") %>% nrow
ivg %>% filter(IVG > 0 & AN == "2018") %>% nrow

ivg %>% filter(IVG1214 > 0 & AN == "2018") %>% cast(libcategetab~AN,length,value="IVG1214") %>% View
ivg %>% filter(IVG-IVGME > 0 & AN == "2018") %>% cast(libcategetab~AN,length,value="IVG") %>% View
ivg %>% filter(IVG > 0 & AN == "2018") %>% cast(libcategetab~AN,length,value="IVG") %>% View

### Taux d'IVG médicamenteuses (moins de 5% / moins que moyenne / plus que moyenne / exclu. +95%)
### Moyenne : 0,5416
ivg$tx_me<-ivg$IVGME/ivg$IVG
ivg$tx_me[is.infinite(ivg$tx_me)]<-1

sum(ivg$IVGME[ivg$AN == "2017"],na.rm=TRUE)/sum(ivg$IVG[ivg$AN == "2018"],na.rm=TRUE)
mean(ivg$tx_me[ivg$AN == "2017"],na.rm=TRUE)

### Taux d'IVG tardives (12-14) (aucune / peu / moyenne)
### Moyenne : 0,0761
ivg$tx_1214<-ivg$IVG1214/ivg$IVG

sum(ivg$IVG1214[ivg$AN == "2017"],na.rm=TRUE)/sum(ivg$IVG[ivg$AN == "2018"],na.rm=TRUE)
mean(ivg$tx_1214[ivg$AN == "2017"],na.rm=TRUE)

### Évolution 2013-2018 (IVG en hausse // stable // déroute)
ivg_export <- ivg %>% filter(AN == "2013" | AN == "2017") %>% group_by(FI) %>% arrange(AN, .by_group = TRUE) %>% mutate(ivg_change = (IVG/lag(IVG) - 1)) %>% mutate(acc_change = (ACC/lag(ACC) - 1)) %>% filter(AN == "2017")

ivg_export$cat_evol<-cut(ivg_export$ivg_change,breaks=c(-1.01,1,-0.05,0.05,13),labels=c("arrêt","en chute","stable","en hausse"),right=TRUE)

### EXPORT

write.csv(ivg_export[,c("FI","DPT","ligneacheminement","rs","libcategetab","IVG","IVGME","IVG1214","ACC","ivg_change","acc_change","cat_evol","tx_me","cat_medic","tx_1214","cat_1214","lat","lon")],"exports/ivg_export.csv",na="",row.names=FALSE)

## Exports étab. 2018

result<-ivg %>% filter(AN == "2013" | AN == "2018") %>% group_by(FI) %>% arrange(AN, .by_group = TRUE) %>% mutate(ivg_change = (IVG/lag(IVG) - 1)) %>% mutate(acc_change = (ACC/lag(ACC) - 1)) %>% filter(AN == "2018")

write.csv(result[,c("FI","DPT","ligneacheminement","rs","libcategetab","IVG","IVGME","ivg_change","acc_change")],"exports/change.csv",na="",row.names=FALSE)

write.csv(merge(subset(ivg_ccam,annee == "2018"),ivg[,c("FI","AN","IVG","IMG")],by.x=c("finess_geo","annee"),by.y=c("FI","AN"),all.x=TRUE)[,c("annee","finess_geo","dep","ligneacheminement","rs","libcategetab","nb_actes.c","nb_actes.m","nb_actes.img2","nb_actes.acc","tx","IVG","IMG")],"exports/ccam.csv",na="",row.names=FALSE)

write.csv(merge(subset(ivg_ccam,annee == "2017"),ivg[,c("FI","AN","IVG","IMG")],by.x=c("finess_geo","annee"),by.y=c("FI","AN"),all.x=TRUE)[,c("annee","finess_geo","dep","ligneacheminement","rs","libcategetab","nb_actes.c","nb_actes.m","nb_actes.img2","nb_actes.acc","tx","IVG","IMG")],"exports/ccam17.csv",na="",row.names=FALSE)

write.csv(subset(ivg[,c("AN","FI","DPT","ligneacheminement","rs","libcategetab","IVG","IVGME","IVG1214","lat","lon")],AN=="2018"),"exports/sae.csv",na="",row.names=FALSE)

## Nombre d'actes (base PMSI-CCAM)
## https://www.scansante.fr/open-ccam/open-ccam-2017

read_ivgCCAM <- function(year) {
  filename = paste("files/open_ccam_",year,".csv",sep="")
  if(year < 18) {
    ccam<-read.csv(filename,sep=";",col.names=c("finess","finess_geo","ccam","nb_actes","dms","nb_sej_0_nuit","dep","reg"))
    ccam<-subset(ccam,select=-c(dms,nb_sej_0_nuit))
  }
  else {
    ccam<-read.csv(filename,sep=";",col.names=c("finess","finess_geo","ccam","nb_sejsea","nb_actes","dms","nb_sej_0_nuit","nb_actes_ambu","dep","reg"))
    ccam<-subset(ccam,select=-c(nb_sejsea,dms,nb_sej_0_nuit,nb_actes_ambu))
  }
  ## Subsets avortements
  ivg_ccamc<-subset(ccam,ccam=="JNJD0020") ## Évacuation d'un utérus gravide par aspiration et/ou curetage, au 1er trimestre de la grossesse
  ivg_ccamm<-subset(ccam,ccam=="JNJP0010") ## Évacuation d'un utérus gravide par moyen médicamenteux, au 1er trimestre de la grossesse
  ivg_ccamimg<-subset(ccam,ccam=="JNJD0010") ## Évacuation d'un utérus gravide, au 2ème trimestre de la grossesse avant la 22ème semaine d'aménorrhée
  # Merge
  ivg_ccam<-merge(ivg_ccamc,ivg_ccamm,by.x=c("finess","finess_geo","dep","reg"),by.y=c("finess","finess_geo","dep","reg"),all.x=TRUE,all.y=TRUE,suffix=c(".c",".m"))
  ivg_ccam<-merge(ivg_ccam,ivg_ccamimg,by.x=c("finess","finess_geo","dep","reg"),by.y=c("finess","finess_geo","dep","reg"),all.x=TRUE,all.y=TRUE)
  setnames(ivg_ccam,c("ccam","nb_actes"),c("ccam.img2","nb_actes.img2"))
  ## Subsets accouchements
  ## JQGD001, JQGD002, JQGD003, JQGD004, JQGD005, JQGD007, JQGD008, JQGD012, JQGD013, JQGA002, JQGA003, JQGA004, JQGA005, JQGD010
  ivg_acc<-subset(ccam,ccam %in% c("JQGD0010","JQGD0020","JQGD0030","JQGD0040","JQGD0050","JQGD0070","JQGD0080","JQGD0120","JQGD0130","JQGA0020","JQGA0030","JQGA0040","JQGA0050","JQGD0100"))
  ivg_acc<-aggregate(ivg_acc$nb_actes,list(ivg_acc$finess_geo),sum,na.rm=TRUE)
  colnames(ivg_acc)<-c("finess_geo","nb_actes.acc")
  ivg_ccam<-merge(ivg_ccam,ivg_acc,by.x="finess_geo",by.y="finess_geo")  
  ## Ajout année
  ivg_ccam["annee"]<-paste("20",year,sep="")
  ivg_ccam<-subset(ivg_ccam,select=-c(ccam.c,ccam.m,ccam.img2))
  return(ivg_ccam)
}

ivg_ccam<-purrr::map_df(c("15","16","17","18"),read_ivgCCAM)
ivg_ccam$nb_actes<-rowSums(ivg_ccam[,c("nb_actes.c","nb_actes.m")],na.rm=TRUE)
ivg_ccam<-merge(ivg_ccam,finess_geo,by.x="finess_geo",by.y="nofinesset",all.x=TRUE)
dpt_reg<-read.csv("files/departement2019.csv",sep=",",col.names=c("dep","reg","cheflieu","tncc","ncc","nccenr","libelle"))
ivg_ccam<-merge(ivg_ccam,dpt_reg,by.x=c("dep","reg"),by.y=c("dep","reg"),all.x=TRUE)

in.dir<- ("geo")

## FRANCE METROP.
france_1<-readOGR(in.dir,layer="COMMUNE_1",verbose=FALSE)
france_r<-readOGR(in.dir,layer="REGION_1",verbose=FALSE)
## MAYOTTE
france_2<-readOGR(in.dir,layer="COMMUNE_2",verbose=FALSE)
## LA RÉUNION
france_3<-readOGR(in.dir,layer="COMMUNE_3",verbose=FALSE)
## GUADELOUPE
france_4<-readOGR(in.dir,layer="COMMUNE_4",verbose=FALSE)
## MARTINIQUE
france_5<-readOGR(in.dir,layer="COMMUNE_5",verbose=FALSE)
## GUYANE
france_6<-readOGR(in.dir,layer="COMMUNE_6",verbose=FALSE)

### Nettoyage des bases, préparation
### Établissements ayant proposé des IVG chir. dans l'année (2013, 2014 et 2018)
### Établissements ayant réalisé des IVG tardives dans l'année 2018

ivg_geo_2013<-subset(ivg,AN==2013 & IVG-IVGME > 0)
ivg_geo_2014<-subset(ivg,AN==2014 & IVG-IVGME > 0)
ivg_geo_2018<-subset(ivg,AN==2018 & IVG-IVGME > 0)
ivg1214_geo_2018<-subset(ivg,AN==2018 & IVG1214 > 0)

## Calcul des durées de trajet

## Calculer les parcours : osrm-extract france-latest.osm.pbf -p ~/Sites/dev/osrm-backend/profiles/car.lua 
## Lancer le serveur : osrm-routed france-latest.osrm

fetchDurees<-function(region) {
  if(region == "Mayotte") {
    print("Mayotte")
    df<-data.frame(as.character(france_2$INSEE_COM),coordinates(spTransform(france_2,CRSobj="+init=epsg:4326")))
    
    ivg13_dist<-subset(ivg_geo_2013,departement=="9F")
    ivg14_dist<-subset(ivg_geo_2014,departement=="9F")
    ivg18_dist<-subset(ivg_geo_2018,departement=="9F")
    ivg1214_dist<-subset(ivg1214_geo_2018,departement=="9F")
  }
  else if(region == "Guadeloupe") {
    print("Guadeloupe")
    df<-data.frame(as.character(france_4$INSEE_COM),coordinates(spTransform(france_4,CRSobj="+init=epsg:4326")))
    
    ivg13_dist<-subset(ivg_geo_2013,departement=="9A")
    ivg14_dist<-subset(ivg_geo_2014,departement=="9A")
    ivg18_dist<-subset(ivg_geo_2018,departement=="9A")
    ivg1214_dist<-subset(ivg1214_geo_2018,departement=="9A")
  }
  else if(region == "Martinique") {
    print("Martinique")
    df<-data.frame(as.character(france_5$INSEE_COM),coordinates(spTransform(france_5,CRSobj="+init=epsg:4326")))
    
    ivg13_dist<-subset(ivg_geo_2013,departement=="9B")
    ivg14_dist<-subset(ivg_geo_2014,departement=="9B")
    ivg18_dist<-subset(ivg_geo_2018,departement=="9B")
    ivg1214_dist<-subset(ivg1214_geo_2018,departement=="9B")
  }
  else if(region == "Reunion") {
    print("La Réunion")
    df<-data.frame(as.character(france_3$INSEE_COM),coordinates(spTransform(france_3,CRSobj="+init=epsg:4326")))
    
    ivg13_dist<-subset(ivg_geo_2013,departement=="9D")
    ivg14_dist<-subset(ivg_geo_2014,departement=="9D")
    ivg18_dist<-subset(ivg_geo_2018,departement=="9D")
    ivg1214_dist<-subset(ivg1214_geo_2018,departement=="9D")
  }
  else if(region == "Guyane") {
    print("Guyane")
    df<-data.frame(as.character(france_6$INSEE_COM),coordinates(spTransform(france_6,CRSobj="+init=epsg:4326")))
    
    ivg13_dist<-subset(ivg_geo_2013,departement=="9C")
    ivg14_dist<-subset(ivg_geo_2014,departement=="9C")
    ivg18_dist<-subset(ivg_geo_2018,departement=="9C")
    ivg1214_dist<-subset(ivg1214_geo_2018,departement=="9C")
  }
  else if(region == "Metropole") {
    print("Métropole")
    df<-data.frame(as.character(france_1$INSEE_COM),coordinates(spTransform(france_1,CRSobj="+init=epsg:4326")))
    
    ivg13_dist<-subset(ivg_geo_2013,!departement %in% c("9A","9B","9C","9D","9F"))
    ivg14_dist<-subset(ivg_geo_2014,!departement %in% c("9A","9B","9C","9D","9F"))
    ivg18_dist<-subset(ivg_geo_2018,!departement %in% c("9A","9B","9C","9D","9F"))
    ivg1214_dist<-subset(ivg1214_geo_2018,!departement %in% c("9A","9B","9C","9D","9F"))
  }
  
  colnames(df) <- c("id", "x", "y")
  
  iterations=nrow(df)
  
  duree_tmp_2013<-matrix(ncol=2,nrow=iterations)
  duree_tmp_2014<-matrix(ncol=2,nrow=iterations)
  duree_tmp_2018<-matrix(ncol=2,nrow=iterations)
  duree_tmp_1214<-matrix(ncol=2,nrow=iterations)
  
  for(i in 1:iterations) {
    # 2013
    print(paste("Analysing for 2013 : ",df[i,1]," (",iterations-i," to go)",sep=""))
    dist<-osrmTable(src=df[i,c("id", "x", "y")],dst=ivg13_dist[,c("FI","lon","lat")])
    duree_tmp_2013[i,1]<-as.character(df[i,1])
    duree_tmp_2013[i,2]=tryCatch({
      as.numeric(apply(dist$durations,1,min))
    },error= function(e) {
      NA
    })
    # 2014
    print(paste("Analysing for 2014 : ",df[i,1]," (",iterations-i," to go)",sep=""))
    dist<-osrmTable(src=df[i,c("id", "x", "y")],dst=ivg14_dist[,c("FI","lon","lat")])
    duree_tmp_2014[i,1]<-as.character(df[i,1])
    duree_tmp_2014[i,2]=tryCatch({
      as.numeric(apply(dist$durations,1,min))
    },error= function(e) {
      NA
    })
    # 2018
    print(paste("Analysing for 2018 : ",df[i,1]," (",iterations-i," to go)",sep=""))
    dist<-osrmTable(src=df[i,c("id", "x", "y")],dst=ivg18_dist[,c("FI","lon","lat")])
    duree_tmp_2018[i,1]<-as.character(df[i,1])
    duree_tmp_2018[i,2]=tryCatch({
      as.numeric(apply(dist$durations,1,min))
    },error= function(e) {
      NA
    })
    # 12-14 (2018)
    print(paste("Analysing for 2018 (12-14) : ",df[i,1]," (",iterations-i," to go)",sep=""))
    dist<-osrmTable(src=df[i,c("id", "x", "y")],dst=ivg1214_dist[,c("FI","lon","lat")])
    duree_tmp_1214[i,1]<-as.character(df[i,1])
    duree_tmp_1214[i,2]=tryCatch({
      as.numeric(apply(dist$durations,1,min))
    },error= function(e) {
      NA
    })
  }
  
  duree_2013<-as.data.frame(duree_tmp_2013,stringsAsFactors=FALSE)
  colnames(duree_2013) <- c("id", "val")
  
  duree_2014<-as.data.frame(duree_tmp_2014,stringsAsFactors=FALSE)
  colnames(duree_2014) <- c("id", "val")
  
  duree_2018<-as.data.frame(duree_tmp_2018,stringsAsFactors=FALSE)
  colnames(duree_2018) <- c("id", "val")
  
  duree_1214<-as.data.frame(duree_tmp_1214,stringsAsFactors=FALSE)
  colnames(duree_1214) <- c("id", "val")
  
  duree<-cbind(duree_2013,duree_2014[2],duree_2018[2],duree_1214[2])
  colnames(duree)<-c("code","d2013","d2014","d2018","d1214")
  
  duree$d2013<-as.numeric(duree$d2013)
  duree$d2014<-as.numeric(duree$d2014)
  duree$d2018<-as.numeric(duree$d2018)
  duree$d1214<-as.numeric(duree$d1214)
  
  duree$diff<-duree$d2018-duree$d2013
  duree$diff1214<-duree$d1214-duree$d2018
  
  return(duree)
}

# France métrop.
duree_me<-fetchDurees("Metropole")

# Mayotte
duree_ma<-fetchDurees("Mayotte")

# La Réunion
duree_lr<-fetchDurees("Reunion")

# Guadeloupe
duree_ga<-fetchDurees("Guadeloupe")

# Martinique
duree_mt<-fetchDurees("Martinique")

# Guyane
duree_gy<-fetchDurees("Guyane")

duree<-rbind(duree_me,duree_ma,duree_lr,duree_ga,duree_mt,duree_gy)
#duree<-rbind(duree_ma,duree_lr,duree_ga,duree_mt,duree_gy)

## Croisement avec la population (rec. 2016 sauf Mayotte 2012)

pop2016<-read.csv("files/BTX_TD_POP1B_2016.csv",sep=";")
pop2016_ma<-read.csv("files/BTX_TD_POP1B_2012.csv",sep=";")
pop2016<-rbind.fill(pop2016,pop2016_ma)

colfap<-c("SEXE2_AGED100015","SEXE2_AGED100016","SEXE2_AGED100017","SEXE2_AGED100018","SEXE2_AGED100019","SEXE2_AGED100020","SEXE2_AGED100021","SEXE2_AGED100022","SEXE2_AGED100023","SEXE2_AGED100024","SEXE2_AGED100025","SEXE2_AGED100026","SEXE2_AGED100027","SEXE2_AGED100028","SEXE2_AGED100029","SEXE2_AGED100030","SEXE2_AGED100031","SEXE2_AGED100032","SEXE2_AGED100033","SEXE2_AGED100034","SEXE2_AGED100035","SEXE2_AGED100036","SEXE2_AGED100037","SEXE2_AGED100038","SEXE2_AGED100039","SEXE2_AGED100040","SEXE2_AGED100041","SEXE2_AGED100042","SEXE2_AGED100043","SEXE2_AGED100044","SEXE2_AGED100045","SEXE2_AGED100046","SEXE2_AGED100047","SEXE2_AGED100048","SEXE2_AGED100049","SEXE2_AGED100050")
colfap_2<-c("SEXE2_AGED100020","SEXE2_AGED100021","SEXE2_AGED100022","SEXE2_AGED100023","SEXE2_AGED100024","SEXE2_AGED100025","SEXE2_AGED100026","SEXE2_AGED100027","SEXE2_AGED100028","SEXE2_AGED100029","SEXE2_AGED100030","SEXE2_AGED100031","SEXE2_AGED100032","SEXE2_AGED100033","SEXE2_AGED100034","SEXE2_AGED100035")
pop2016$F_AP<-rowSums(pop2016[,colfap])
pop2016$F_AP2<-rowSums(pop2016[,colfap_2])

duree<-merge(duree,pop2016,by.x="code",by.y="CODGEO",all.x=TRUE)

write.csv(duree[,c("code","d2013","d2014","d2018","d1214","diff","diff1214","LIBGEO","F_AP","F_AP2")],"duree_ivg.csv",row.names=FALSE)
#duree<-read.csv("duree_ivg.csv",sep=",")


## FRANCE

tapply(duree$F_AP,cut(duree$d2018,breaks=c(0,30,45,500),labels=c("0-30","30-45","+45")),FUN=sum,na.rm=TRUE)

tapply(duree$F_AP,cut(duree$d1214,breaks=c(0,30,45,500),labels=c("0-30","30-45","+45")),FUN=sum,na.rm=TRUE)

tapply(duree$F_AP,cut(duree$diff,breaks=c(-60,-45,-30,-15,0,15,30,45,60),labels=c("-60m-45m","-45m-30m","-30m-15m","-15m0m","0m+15m","+15m+30m","+30m+45m","+45m+60m")),FUN=sum,na.rm=TRUE)


## Loiret

tapply(subset(duree,startsWith(duree$code,"45"))$F_AP,cut(subset(duree,startsWith(duree$code,"45"))$d2013,breaks=c(0,30,45,500),labels=c("0-30","30-45","+45")),FUN=sum,na.rm=TRUE)

tapply(subset(duree,startsWith(duree$code,"45"))$F_AP,cut(subset(duree,startsWith(duree$code,"45"))$d2018,breaks=c(0,30,45,500),labels=c("0-30","30-45","+45")),FUN=sum,na.rm=TRUE)

tapply(subset(duree,startsWith(duree$code,"45"))$F_AP,cut(subset(duree,startsWith(duree$code,"45"))$diff,breaks=c(-60,-45,-30,-15,0,15,30,45,60),labels=c("-60m-45m","-45m-30m","-30m-15m","-15m0m","0m+15m","+15m+30m","+30m+45m","+45m+60m")),FUN=sum,na.rm=TRUE)

###### moyenne dans le 45 ??? 
##### durée du trajet par commune * population par commune / population totale

sum(duree$d2018[startsWith(duree$code,"45")]*duree$F_AP[startsWith(duree$code,"45")])/sum(duree$F_AP[startsWith(duree$code,"45")])

## Loire-Atlantique

tapply(subset(duree,startsWith(duree$code,"44"))$F_AP,cut(subset(duree,startsWith(duree$code,"44"))$d2013,breaks=c(0,30,45,500),labels=c("0-30","30-45","+45")),FUN=sum,na.rm=TRUE)

tapply(subset(duree,startsWith(duree$code,"44"))$F_AP,cut(subset(duree,startsWith(duree$code,"44"))$d2018,breaks=c(0,30,45,500),labels=c("0-30","30-45","+45")),FUN=sum,na.rm=TRUE)

tapply(subset(duree,startsWith(duree$code,"44"))$F_AP,cut(subset(duree,startsWith(duree$code,"44"))$diff,breaks=c(-60,-45,-30,-15,0,15,30,45,60),labels=c("-60m-45m","-45m-30m","-30m-15m","-15m0m","0m+15m","+15m+30m","+30m+45m","+45m+60m")),FUN=sum,na.rm=TRUE)

###### moyenne dans le 44 ??? 
##### durée du trajet par commune * population par commune / population totale

sum(duree$d2018[startsWith(duree$code,"44")]*duree$F_AP[startsWith(duree$code,"44")])/sum(duree$F_AP[startsWith(duree$code,"44")])

## Pays-de-la-Loire

sapply(c("44","49","53","72","85"),function(x){tapply(subset(duree,startsWith(duree$code,x))$F_AP,cut(subset(duree,startsWith(duree$code,x))$d2018,breaks=c(0,30,45,500),labels=c("0-30","30-45","+45")),FUN=sum,na.rm=TRUE)})

sapply(c("44","49","53","72","85"),function(x){sum(duree$d2018[startsWith(duree$code,x)]*duree$F_AP[startsWith(duree$code,x)],na.rm=TRUE)/sum(duree$F_AP[startsWith(duree$code,x)],na.rm=TRUE)})

## Réalisation de cartes

write.csv(ivg_geo_2013,"ivg2013.csv")
write.csv(ivg_geo_2017,"ivg2017.csv")
write.csv(ivg_geo_2018,"ivg2018.csv")

coordinates(ivg_geo_2013)<- ~lon+lat
proj4string(ivg_geo_2013)<-CRS("+proj=longlat +datum=WGS84")

coordinates(ivg_geo_2018)<- ~lon+lat
proj4string(ivg_geo_2018)<-CRS("+proj=longlat +datum=WGS84")

drawRegion<-function(nom_reg,annee){
  par(bg="#006994")
  region<-france_1[france_1$NOM_REG==nom_reg,]
  ivg_geo_2013<-spTransform(ivg_geo_2013,CRS(proj4string(france_r)))
  ivg_geo_2018<-spTransform(ivg_geo_2018,CRS(proj4string(france_r)))
  poi<-subset(region,STATUT=="Capitale d'état" | STATUT=="Préfecture" | STATUT=="Préfecture de région" | STATUT=="Sous-préfecture")
  plot(france_r,col="#DDDDDD",border=1,xlim=bbox(region)[1,],ylim=bbox(region)[2,])
  if(annee=="2013") {
    choroLayer(spdf=region,spdfid="INSEE_COM",df=duree,dfid="code",var="d2013",nclass=7,lwd=0.0001,breaks=c(0,15,30,45,60,75,90,500),col=carto.pal("wine.pal",8),add=TRUE)
    points(ivg_geo_2013,pch=15,cex=0.5,col="red")
  }
  else if(annee=="2018") {
    choroLayer(spdf=region,spdfid="INSEE_COM",df=duree,dfid="code",var="d2017",nclass=7,lwd=0.0001,breaks=c(0,15,30,45,60,75,90,500),col=carto.pal("wine.pal",8),add=TRUE)
    points(ivg_geo_2018,pch=15,cex=0.5,col="red")
  }
  points(coordinates(poi),pch=20,cex=0.5,col="white")
  labelLayer(poi,poi@data,spdfid="INSEE_COM",dfid="INSEE_COM",txt="NOM_COM",cex=0.4,pos=2,font=4,offset=0.2,col= "#000000", bg = "#FFFFFF50",halo=TRUE,overlap=FALSE)
  layoutLayer(title = paste(nom_reg,annee),coltitle="black",col=NA,sources="",scale = NULL,author=NULL,frame=FALSE)
  dev.print(pdf,paste('cartes/',nom_reg,'_',annee,'.pdf',sep=""))
}

drawRegionMayotte<-function(){
  par(bg="#006994")
  region<-france_2
  ivg_geo<-spTransform(ivg_geo_2018,CRS(proj4string(region)))
  poi<-subset(region,STATUT=="Capitale d'état" | STATUT=="Préfecture" | STATUT=="Préfecture de région" | STATUT=="Sous-préfecture")
  plot(region,col="#DDDDDD",border=1,xlim=bbox(region)[1,],ylim=bbox(region)[2,])
  choroLayer(spdf=region,spdfid="INSEE_COM",df=duree_ma,dfid="id",var="val.x",nclass=7,lwd=0.0001,breaks=c(0,15,30,45,60,75,90,500),col=carto.pal("wine.pal",8),add=TRUE)
  points(ivg_geo,pch=15,cex=0.5,col="red")
  points(coordinates(poi),pch=20,cex=0.5,col="white")
  labelLayer(spdf=poi,df=poi@data,spdfid="INSEE_COM",dfid="INSEE_COM",txt="NOM_COM",cex=0.4,pos=2,font=4,offset=0.2,col= "#000000", bg = "#FFFFFF50",halo=TRUE,overlap=TRUE)
  layoutLayer(title = paste("ACCÈS À L'IVG À MAYOTTE"),coltitle="black",col=NA,sources="",scale = NULL,author=NULL,frame=FALSE)
  dev.print(pdf,'cartes/MAYOTTE.pdf')
}

drawRegionReunion<-function(){
  par(bg="#006994")
  region<-france_3
  ivg_geo<-spTransform(ivg_geo_2018,CRS(proj4string(region)))
  poi<-subset(region,STATUT=="Capitale d'état" | STATUT=="Préfecture" | STATUT=="Préfecture de région" | STATUT=="Sous-préfecture")
  plot(region,col="#DDDDDD",border=1,xlim=bbox(region)[1,],ylim=bbox(region)[2,])
  choroLayer(spdf=region,spdfid="INSEE_COM",df=duree_lr,dfid="id",var="val.x",nclass=7,lwd=0.0001,breaks=c(0,15,30,45,60,75,90,500),col=carto.pal("wine.pal",8),add=TRUE)
  points(ivg_geo,pch=15,cex=0.5,col="red")
  points(coordinates(poi),pch=20,cex=0.5,col="white")
  labelLayer(spdf=poi,df=poi@data,spdfid="INSEE_COM",dfid="INSEE_COM",txt="NOM_COM",cex=0.4,pos=2,font=4,offset=0.2,col= "#000000", bg = "#FFFFFF50",halo=TRUE,overlap=FALSE)
  layoutLayer(title = paste("ACCÈS À L'IVG À LA RÉUNION"),coltitle="black",col=NA,sources="",scale = NULL,author=NULL,frame=FALSE)
  dev.print(pdf,'cartes/REUNION.pdf')
}

drawRegionGuadeloupe<-function(){
  par(bg="#006994")
  region<-france_4
  ivg_geo<-spTransform(ivg_geo_2018,CRS(proj4string(region)))
  poi<-subset(region,STATUT=="Capitale d'état" | STATUT=="Préfecture" | STATUT=="Préfecture de région" | STATUT=="Sous-préfecture")
  plot(region,col="#DDDDDD",border=1,xlim=bbox(region)[1,],ylim=bbox(region)[2,])
  choroLayer(spdf=region,spdfid="INSEE_COM",df=duree,dfid="id",var="val.x",nclass=7,lwd=0.0001,breaks=c(0,15,30,45,60,75,90,500),col=carto.pal("wine.pal",8),add=TRUE)
  points(ivg_geo,pch=15,cex=0.5,col="red")
  points(coordinates(poi),pch=20,cex=0.5,col="white")
  labelLayer(spdf=poi,df=poi@data,spdfid="INSEE_COM",dfid="INSEE_COM",txt="NOM_COM",cex=0.4,pos=2,font=4,offset=0.2,col= "#000000", bg = "#FFFFFF50",halo=TRUE,overlap=FALSE)
  layoutLayer(title = paste("ACCÈS À L'IVG EN GUADELOUPE"),coltitle="black",col=NA,sources="",scale = NULL,author=NULL,frame=FALSE)
  dev.print(pdf,'cartes/GUADELOUPE.pdf')
}

drawRegionMartinique<-function(){
  par(bg="#006994")
  region<-france_5
  ivg_geo<-spTransform(ivg_geo_2018,CRS(proj4string(region)))
  poi<-subset(region,STATUT=="Capitale d'état" | STATUT=="Préfecture" | STATUT=="Préfecture de région" | STATUT=="Sous-préfecture")
  plot(region,col="#DDDDDD",border=1,xlim=bbox(region)[1,],ylim=bbox(region)[2,])
  choroLayer(spdf=region,spdfid="INSEE_COM",df=duree,dfid="id",var="val",nclass=7,lwd=0.0001,breaks=c(0,15,30,45,60,75,90,500),col=carto.pal("wine.pal",8),add=TRUE)
  points(ivg_geo,pch=15,cex=0.5,col="red")
  points(coordinates(poi),pch=20,cex=0.5,col="white")
  labelLayer(spdf=poi,df=poi@data,spdfid="INSEE_COM",dfid="INSEE_COM",txt="NOM_COM",cex=0.4,pos=2,font=4,offset=0.2,col= "#000000", bg = "#FFFFFF50",halo=TRUE,overlap=FALSE)
  layoutLayer(title = paste("ACCÈS À L'IVG EN MARTINIQUE"),coltitle="black",col=NA,sources="",scale = NULL,author=NULL,frame=FALSE)
  dev.print(pdf,'cartes/MARTINIQUE.pdf')
}

drawRegionGuyane<-function(){
  par(bg="#006994")
  region<-france_6
  ivg_geo<-spTransform(ivg_geo_2018,CRS(proj4string(region)))
  poi<-subset(region,STATUT=="Capitale d'état" | STATUT=="Préfecture" | STATUT=="Préfecture de région" | STATUT=="Sous-préfecture")
  plot(region,col="#DDDDDD",border=1,xlim=bbox(region)[1,],ylim=bbox(region)[2,])
  choroLayer(spdf=region,spdfid="INSEE_COM",df=duree,dfid="id",var="val",nclass=7,lwd=0.0001,breaks=c(0,15,30,45,60,75,90,500),col=carto.pal("wine.pal",8),add=TRUE)
  points(ivg_geo,pch=15,cex=0.5,col="red")
  points(coordinates(poi),pch=20,cex=0.5,col="white")
  labelLayer(spdf=poi,df=poi@data,spdfid="INSEE_COM",dfid="INSEE_COM",txt="NOM_COM",cex=0.4,pos=2,font=4,offset=0.2,col= "#000000", bg = "#FFFFFF50",halo=TRUE,overlap=FALSE)
  layoutLayer(title = paste("ACCÈS À L'IVG EN GUYANE"),coltitle="black",col=NA,sources="",scale = NULL,author=NULL,frame=FALSE)
  dev.print(pdf,'cartes/GUYANE.pdf')
}


drawRegions<-function() {
  drawRegionMayotte()
  drawRegionReunion()
  drawRegionGuadeloupe()
  drawRegionMartinique()
  drawRegionGuyane()
  for(region in c("AUVERGNE-RHONE-ALPES","BOURGOGNE-FRANCHE-COMTE","BRETAGNE","CENTRE-VAL DE LOIRE","CORSE","GRAND EST","HAUTS-DE-FRANCE","ILE-DE-FRANCE","NORMANDIE","NOUVELLE-AQUITAINE","OCCITANIE","PAYS DE LA LOIRE","PROVENCE-ALPES-COTE D'AZUR")){
    drawRegion(region,"2018")
  }
}

drawRegions()

drawRegion("CENTRE-VAL DE LOIRE","2013")
drawRegion("CENTRE-VAL DE LOIRE","2017")

## Exports régionaux

extractRegion<- function(liste,nom) {
  ## Extraction SAE
  ivg_local<-subset(ivg,departement %in% liste)
  ivg_local<-ivg_local[,c("AN","rs","departement","libcategetab","IVG","IVGME","CONV","EFFPL","EFFPA","ETP","siret","nofinessej")]
  colnames(ivg_local)<-c("Année","Nom","Département","Type d'établissement","Nombre d'IVG","Nombre d'IVG médicamenteuses","Conventions","Temps plein","Temps partiel","ETP moyens","Siret","Finess")
  filename=paste("exports/sae_",nom,".csv",sep="")
  write.csv(ivg_local,file=filename,row.names=FALSE)
  
  ## Extraction CCAM
  ivg_local_ccam<-subset(ivg_ccam,departement %in% liste)
  ivg_local_ccam<-ivg_local_ccam[,c("annee","rs","departement","libcategetab","ccam","nb_actes","siret","nofinessej")]
  colnames(ivg_local_ccam)<-c("Année","Nom","Département","Type d'établissement","Type d'IVG","Nombre d'IVG","Siret","Finess")
  filename_ccam=paste("exports/scansante_",nom,".csv",sep="")
  write.csv(ivg_local_ccam,file=filename_ccam,row.names=FALSE)
  
  print(paste(length(table(ivg_local$Nom))," établissements dans le SAE et ",length(table(ivg_local_ccam$Nom))," dans ScanSanté",sep=""))
}
