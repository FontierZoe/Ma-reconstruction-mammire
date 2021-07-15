#### RECONSTRUCTIN MAMMAIRE - Descriptif #####
#### Données de regroupements des actes par centres de soin ####
### Le but est d'établir une carte d'identité du centres en fonctions de ses activités, de leur volume et des caractéristiques de patientes



#library
library(dplyr)
library(ggplot2)
library(fmsb)
library(data.table)
library(tidyr)
library(purrr)
library(scales)
library(gglorenz)
library(ggfittext)
library(ggpubr)
library(rstatix)


DEFAULT_CAPTION <- "Source: PMSI"
####1.1 Les tables ####
source("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/utils.R")
pdf(
  file = file.path("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/01.Graphes", "plots.pdf"),
  height = 10,
  width = 15
)


reconstruction<-read.csv("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/RECONSTRUCTIONS_1519_b.csv",sep=";")
finess<-read.csv("/Users/zoefontier/Desktop/04.93/01.Data/finess_geo.csv", sep=";")

finess<-finess%>%
  distinct(finessGeo, .keep_all=TRUE)

summary(reconstruction)



##### 1.2 Stat de volume #####
# Remarque: les actes QEEB n'ont été crée qu'en 2017, ils ne sont donc pas présents dans la base PMSI avant cette date
summary<-reconstruction%>%
  group_by(year, code)%>%
  summarise(total=sum(n_patients))

reconstruction%>%
  group_by(year, code)%>%
  summarise(total=sum(n_sejours))

#pas les mêmes chiffres pour le nombre de séjour
#en tête les lipomodelage et les implants, puis les opérations bidon de dédoublement du sein restant

##### 1.3 Merge avec les établissements #####
reconstruction<-reconstruction%>%
  inner_join(finess %>% select(categ_pmsi, finessGeo, secteur_pmsi, rs, latitude, longitude, adresse), by=c("finessGeoDP"="finessGeo"))

reconstruction%>%
  group_by(rs)%>%
  distinct(rs)  # 687 centres qui interviennent d'une façon ou d'une autre dans la reconstruction mammaire


reconstruction%>%
  group_by(rs, code, year)%>%
  summarise(total_patients=sum(n_patients))
#Les établissements apparaissent plus de fois, à cause des différentes dates, différentes origines des patients

# On va grouper par établissement
# Nombre de patients par  type établissement
reconstruction%>%
  group_by(rs)%>%
  filter(year=="2019")%>%
  distinct(rs, .keep_all=TRUE)%>%
  group_by(categ_pmsi, year)%>%
  summarise(n=n())
# en 2015: 118 CH, 80 CHR/U, 20 CLCC, 303 Prives, 27 PSPH/EBNL
# en 2016: 124 CH, 81 CHR/U, 20 CLCC, 305 Prives, 30 PSPH/EBNL
# en 2017: 112 CH, 82 CHR/U, 20 CLCC, 310 Prives, 29 PSPH/EBNL
# en 2018: 119 CH, 78 CHR/U, 20 CLCC, 304 Prives, 32 PSPH/EBNL
# en 2019: 120 CH, 81 CHR/U, 20 CLCC, 308 Prives, 34 PSPH/EBNL

t<-reconstruction %>% 
  group_by(categ_pmsi, year) %>% 
  summarise(total_patient=sum(n_patients)) #CCLC et Privé ont le plus de patients

# #age par type d'etablissement
# ggplot(reconstruction, aes(fill=age_cut, y=n_patients, x=category_pmsi)) + 
#   geom_bar(position="stack", stat="identity") +labs(title="Répartition des âges selon le type d'établissement ", x="type d'établissement", y="nombre de patientes")+scale_fill_brewer(palette="RdGy")
# ggplot(reconstruction, aes(fill=age_cut, y=n_patients, x=category_pmsi)) + 
#   geom_bar(position="fill", stat="identity") +labs(title="Répartition des âges selon le type d'établissement(en % sur le total des opérations)", x="type d'établissement", y="nombre de patientes")+scale_fill_brewer(palette="RdGy")
# #il n'y a pas un certain type d'etablissement qui capte qui clientele d'un age particulier (apres les categories sont assez larges)
# # les CLCC et les CHR/U captent peut etre un peu plus la population plus jeune
# ggplot(reconstruction, aes(fill=category_pmsi, y=n_patients, x=age_cut)) + 
#   geom_bar(position="fill", stat="identity") +labs(title="Répartition des tranches d'ages dans les types d'établissement(en % sur le total des opérations)", x="Tranches d'age", y="nombre de patientes")+scale_fill_brewer(palette="RdGy")


##### 1.4 Repartition des patientes dans l'ensemble des établissements #####
#total d'actes par hopitaux


reconstruction%>% group_by(rs, year)%>%
  summarize(total_patients=sum(n_patients))%>%
  ggplot( aes(x=total_patients)) +
  facet_wrap(~year)+
  geom_histogram(binwidth = 5) +
  theme_light()+
labs(title="Distribution des établissements selon le nombre de patientes par structures, par an", x="Nombre de patientes par structure", y="Nombre de structures" )


reconstruction %>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs)%>%
  summarise(total_patients=sum(n_patients))%>%
  arrange(total_patients)%>%
  mutate(cum_patients=cumsum(total_patients), etab=1, cum_etab=cumsum(etab))%>%
  ggplot(aes(x=cum_patients, y=cum_etab)) +
  geom_line()+
  theme_light()+
  labs(title="Rrpartition des patientes dans les établissements", x="Effectif cumulé du nombre de patients", y="Effectif cumulé du nombre d'établissements")
#on regarde le nombre total de patientes qui se rendent dans des structures qui font peu d'opérations de chirurgie reconstructrice
#la moitié des patientes se repartissent entre 550 strutures, et 'autres moitié dans une 50aine.

reconstruction %>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs)%>%
  mutate(total_patients=sum(n_patients))%>%
  distinct(finessGeoDP,.keep_all=TRUE)%>%
arrange(total_patients)%>%
  ggplot(aes(x=total_patients)) +
 stat_lorenz(desc=TRUE)+
  geom_abline(linetype = "dashed") +
  theme_light()+
labs(title="Courbe de Lorenz du Nombre de patients par établissements", x="Pourcentage cumulé du nombre de patients", y="Pourcentage cumulé du nombre d'établissements")
#on regarde le nombre total de patientes qui se rendent dans des structures qui font peu d'opérations de chirurgie reconstructrice
#50% des patients vont vers 10% des etbalissements
#### 2. Répartition des actes - Stat ####
##### 2.2 regroupement des actes #####
#on regroupe des actes qui sont similaires
# QEMA020 DIEP
#QEMA001 et QEMA014 ensemble car lambeaux pediculés
#QEFA015 et PZMA004, QEMA002: Gracillis/PAP/lambeaux libres
#QEEB152,QEEB317 et QZEA045 ensemble car lipomodelage
#QEMA009 et QEMA010 ensemble car reconstruction plaque aréolo mammelonaire
#QEMB001: tatouage
# QEMA008 et QEFA013: Dorsal


reconstruction$acte<-"0"
reconstruction$acte[reconstruction$code=="QEEB152"|reconstruction$code=="QEEB317"|reconstruction$code=="QZEA045"]<-"lipomodelage"
reconstruction$acte[reconstruction$code=="QEMA001"|reconstruction$code=="QEMA014"]<-"lambeau pedicule/ TRAM"

reconstruction$acte[reconstruction$code=="QEMA020"]<-"DIEP"
reconstruction$acte[reconstruction$code=="QEMA008"|reconstruction$code=="QEFA013"]<-"grand dorsal"
reconstruction$acte[reconstruction$code=="QEMA006"]<-"implant"
reconstruction$acte[reconstruction$code=="QEMA009"|reconstruction$code=="QEMA010"]<-"plaque areolo mamelonnaire"
reconstruction$acte[reconstruction$code=="QEMB001"]<-"dermopigmentation"
reconstruction$acte[reconstruction$code=="QEMA011"]<-"dedoublement du sein restant"
reconstruction$acte[reconstruction$code=="QEMA002"|reconstruction$code=="QEFA015"|reconstruction$code=="PZMA004"]<-"Gracillis/PAP/lambeau libre"


reconstruction$categ_pmsi[reconstruction$categ_pmsi=="Pri<xe9>"]<-"Prive"
table(reconstruction$categ_pmsi)
table(reconstruction$acte)
reconstruction<-reconstruction%>%
  filter(acte!="0", .keep_all=TRUE)

##### 2.3 Repartition des actes #####

#repartition du volume d'actes dans les hopitaux par techniques
#graphe de répartition de l'offre de soins par type d'actes (en fonction du nombres d'actes totals par structures)

t<-reconstruction%>%
  group_by(year,acte)%>%
  distinct(rs, .keep_all=TRUE)%>%
  summarise(n=n())
  


reconstruction%>%
  group_by(rs,year,acte)%>%
  summarise(total_patients=sum(n_patients))%>%
  ggplot(aes(x=total_patients,fill=acte))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~year)+
  theme_light()+
  scale_fill_manual(values=COLOR_CODE, label=LABEL_CODE, name="Actes de reconstruction") +
  labs(title="Répartition des structures selon le nombre d'actes de chaque technique", 
    x="Nombre d'actes par structure", y="Nombre de structures" )
#la plupart des hopitaux ne font pas enormement d'actes dans l'année, 

#repartition du nombre d'actes par hopitaux, par techniques
table<-reconstruction %>% 
  group_by(rs,acte,year) %>%
  summarise(total=sum(n_patients))%>%
  group_by(acte,year)%>%
  summarise( min=min(total),
              q1=quantile(total, 0.25),  mediane=quantile(total, 0.5),moy = mean(total),
             q3=quantile(total, 0.75),max=max(total), deviation=sd(total))

# Moyenne de patientes par types d'établissement
table<-reconstruction %>% 
  group_by(rs,year, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  group_by(categ_pmsi,year)%>%
  summarise( min=min(total),
             q1=quantile(total, 0.25),  mediane=quantile(total, 0.5),moy = mean(total),
             q3=quantile(total, 0.75),max=max(total), deviation=sd(total))

reconstruction$year<-as.character(reconstruction$year)
reconstruction %>% 
  group_by(rs,acte, year) %>%
  summarise(total=sum(n_patients))%>%
  group_by(acte, year)%>%
  ggplot(aes(x=total, y=year, fill=year)) + 
  geom_boxplot(lwd=0.2)+ 
  facet_wrap(~acte, scale="free", labeller = as_labeller(LABEL_CODE))+
  labs(title="Distribution des actes techniques selon l'année", 
       x="Répartition du volume de patients", 
       y="Année", caption="données PMSI") +
  scale_fill_brewer(palette="Set3")+
  theme_light()

# on voit bien que la plupart des hopitaux font en général très peu d'actes de reconstruction (plus de 25% font un seul acte de chaque technique par an)
#Seules 25% des structures font plus d"une dizaine d'opértions d'un même acte

#repartition du nombre de patient par techniques, en fonction du type d'établissement
#bx plot, ecart type , mediane et quartile de la distrib. ie positionnement général du type d'etablissements sur l'acte plus variation interclasse
reconstruction %>% 
  filter(year=="2015", .keep_all=TRUE)%>%
  group_by(rs,acte, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  group_by(acte, categ_pmsi)%>%
 ggplot(aes(x=total, y=categ_pmsi, fill=categ_pmsi)) + 
   geom_boxplot(lwd=0.2)+ 
  facet_wrap(~acte, scale="free", labeller = as_labeller(LABEL_CODE)) +
  labs(title="Distribution des actes techniques selon le type d'établissement, en 2015", 
       x="Répartition du volume de patients", y="Type d'établissement (catégorisation PMSI)", subtitle = "En 2015: 87 CH, 80 CHR/U,19 CLCC, 249 Prives, 22 PSPH/EBNL", caption=DEFAULT_CAPTION) +
  scale_fill_manual(values=COLOR_PMSI, name="Categorie PMSI")+
  theme_light()


reconstruction %>% 
  filter(year=="2016", .keep_all=TRUE)%>%
  group_by(rs,acte, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  group_by(acte, categ_pmsi)%>%
  ggplot(aes(x=total, y=categ_pmsi, fill=categ_pmsi)) + 
  geom_boxplot(lwd=0.2)+ 
  facet_wrap(~acte, scale="free", labeller = as_labeller(LABEL_CODE)) +
  labs(title="Distribution des actes techniques selon le type d'établissement, en 2016", 
       x="Répartition du volume de patients", y="Type d'établissement (catégorisation PMSI)", subtitle = "En 2016: 89 CH, 76 CHR/U, 19 CLCC, 248 Prives, 23 PSPH/EBNL", caption=DEFAULT_CAPTION) +
  scale_fill_manual(values=COLOR_PMSI, name="Categorie PMSI")+
  theme_light()
 
reconstruction%>%
filter(year=="2017", .keep_all=TRUE)%>%
  group_by(rs,acte, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  group_by(acte, categ_pmsi)%>%
  ggplot(aes(x=total, y=categ_pmsi, fill=categ_pmsi)) + 
  geom_boxplot(lwd=0.2)+ 
  facet_wrap(~acte, scale="free", labeller = as_labeller(LABEL_CODE)) +
  labs(title="Distribution des actes techniques selon le type d'établissement, en 2017", 
       x="Répartition du volume de patients", y="Type d'établissement (catégorisation PMSI)", subtitle = "En 2017:90 CH, 73 CHR/U, 19 CLCC, 267 Prives, 23 PSPH/EBNL", caption=DEFAULT_CAPTION) +
  scale_fill_manual(values=COLOR_PMSI, name="Categorie PMSI")+
  theme_light()

reconstruction%>%
  filter(year=="2018", .keep_all=TRUE)%>%
  group_by(rs,acte, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  group_by(acte, categ_pmsi)%>%
  ggplot(aes(x=total, y=categ_pmsi, fill=categ_pmsi)) + 
  geom_boxplot(lwd=0.2)+ 
  facet_wrap(~acte, scale="free", labeller = as_labeller(LABEL_CODE)) +
  labs(title="Distribution des actes techniques selon le type d'établissement, en 2018", 
       x="Répartition du volume de patients", y="Type d'établissement (catégorisation PMSI)", subtitle = "En 2018: 95 CH, 72 CHR/U, 19 CLCC, 258 Prives, 27 PSPH/EBNL", caption=DEFAULT_CAPTION) +
  scale_fill_manual(values=COLOR_PMSI, name="Categorie PMSI")+
  theme_light()


reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs,acte, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  group_by(acte, categ_pmsi)%>%
  ggplot(aes(x=total, y=categ_pmsi, fill=categ_pmsi)) + 
  geom_boxplot(lwd=0.2)+ 
  facet_wrap(~acte, scale="free", labeller = as_labeller(LABEL_CODE)) +
  labs(title="Distribution des actes techniques selon le type d'établissement, en 2019", 
       x="Répartition du volume de patients", y="Type d'établissement (catégorisation PMSI)", subtitle = "En 2019: 96 CH, 73 CR/U, 19 CLCC, 270 Prives, 27 PSPH/EBNL", caption=DEFAULT_CAPTION) +
  scale_fill_manual(values=COLOR_PMSI, name="Categorie PMSI")+
  theme_light()



reconstruction%>%
  group_by(rs,acte, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  group_by(acte, categ_pmsi)%>%
  ggplot(aes(x=total, y=categ_pmsi, fill=categ_pmsi)) + 
  geom_boxplot(lwd=0.2)+ 
  facet_wrap(~acte, scale="free", labeller = as_labeller(LABEL_CODE)) +
  labs(title="Distribution des actes techniques selon le type d'établissement, entre 2015 et 2019", 
       x="Répartition du volume de patients", y="Type d'établissement (catégorisation PMSI)", caption=DEFAULT_CAPTION) +
  scale_fill_manual(values=COLOR_PMSI, name="Categorie PMSI")+
  theme_light()


##### 2.4 La différenciation des établissements #####
reconstruction%>%
  group_by(categ_pmsi, year)%>%
  summarise(total=sum(n_patients))%>%
  ggplot(aes(x=total, y=categ_pmsi, fill=categ_pmsi, label=sprintf("n=%s", total)))+
  geom_bar(stat="identity",show.legend=TRUE) +
  geom_bar_text(position="stack", place="center")+
  coord_flip() +
  theme_light()+
  facet_wrap(~year)+
  scale_fill_manual(values=COLOR_PMSI, name="Catégorie PMSI")+
  labs(title="Répartition du nombre de patientes, selon la catégorie PMSI", xlan="Catégorie PMSI", ylab="Effectif total", caption=DEFAULT_CAPTION)
  
reconstruction%>%
  group_by(categ_pmsi, year)%>%
  summarise(total=sum(n_patients))%>%
  group_by(year)%>%
  mutate(somme=sum(total), ratio=total/somme)%>%
  ggplot(aes(x=ratio, y=year, fill=categ_pmsi, label=sprintf("n=%s, (%s%%)", total, round(ratio*100))))+
  geom_bar(stat="identity",show.legend=TRUE) +
  geom_bar_text(position="stack", place="center")+
  coord_flip() +
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI, name="Catégorie PMSI")+
  labs(title="Répartition du nombre de patientes, selon la catégorie PMSI", xlan="Catégorie PMSI", ylab="Effectif total", caption=DEFAULT_CAPTION)




# Quelle catégorie à un volume d'actes importants?
#Quels sont les centres au volume de patientes le plus important?


#en 2019
reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  filter(total< 10) %>%
  group_by(categ_pmsi)%>%
  count(categ_pmsi)
# Parmi les structures qui ont moins de 10 patientes au total par an, 45 sont des CH, 16 sont des CHR/U,  106 en prive et 8 PSPH/ENBL

#densité du nombre de patientes par type de structures

reconstruction%>%
  group_by(rs,year,categ_pmsi)%>%
  summarise(total_patients=sum(n_patients))%>%
  ggplot(aes(x=total_patients,fill=categ_pmsi))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~year)+
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI,name="Catégorie PMSI") +
  labs(title="Répartition des structures selon leur volume de patientes", 
       x="Nombre de patientes par structure", y="Nombre de structures" )

reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  ggplot(aes(x=total, group=categ_pmsi, fill=categ_pmsi)) +
  geom_density(adjust=1.5) +
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI, name="Catégorie PMSI")+
  labs(title=" Répartition du nombre de patientes en 2019, selon la catégorie PMSI", xlab="Nombre de patients", ylab="densité")


reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  filter(total>50) %>%
  group_by(categ_pmsi)%>%
  count(categ_pmsi)
#Parmi les structures qui font plus de 50 actes par an: 11 sont des CH, 32 sont des CHR/U, 18 sont des CLCC, 71 sont des cliniques privées, 6 sont des PSPH/EBNL

reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs, categ_pmsi) %>%
  summarise(total=sum(n_patients))%>%
  filter(total>100) %>%
  group_by(categ_pmsi)%>%
  count(categ_pmsi)
#Parmi les structures qui font plus de 100 actes par an: 3 sont des CH, 21 sont des CHR/U, 18 sont des CLCC, 33 sont des cliniques privées, 2 sont des PSPH/EBNL




#### 3. Maps ####

library(broom)
library(sf)
library(osmdata)
library(ggplot2)
library(geojsonio)
library(broom)
library(dplyr)
library(ggsci)
library(mapproj)
library(dplyr)


##### 3.1 layers #####
# les constantes
REGIONS_DOM_TOM = c("01", "02", "03", "04", "06", "TOM")
DEPARTEMENTS_DOM_TOM = c("971", "972", "973", "974", "976")
DEPS_IDF <- c("75", "91", "92", "93", "94", "95", "77", "78")
DEPS_PARIS<-c("75","92","93", "94")

  # Load data
  spdf_departements <- geojson_read(
    file.path("/Users/zoefontier/Desktop/04.93/01.Data/departements-francais-2015.geojson"),
    what = "sp"
  )

spdf_regions <- geojson_read(
  file.path("/Users/zoefontier/Desktop/04.93/01.Data/regions.geojson"),
  what = "sp"
)

france_regions_layer <- tidy(
  spdf_regions[ !spdf_regions@data$code %in% REGIONS_DOM_TOM, ],
  region="code"
) ### pour enlever les DOM TOM

france_departements_layer <- tidy(
  spdf_departements[ !spdf_departements@data$code_dep %in% DEPARTEMENTS_DOM_TOM, ],
  region="code_dept"
)

ile_de_france_layer<-tidy(
  spdf_departements[ spdf_departements@data$code_dep %in% DEPS_IDF, ],
  region="code_dept"
)

paris_layer<-tidy(
  spdf_departements[ spdf_departements@data$code_dep %in% DEPS_PARIS, ],
  region="code_dept"
)
##### 3.2 Tous types de reconstruction #####
p<-ggplot() +
  geom_polygon(data=france_regions_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.5) +
  geom_polygon(data=france_departements_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.2) +
  theme_void() +
  coord_map()

reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  filter(substr(finessGeoDP,1,2) != "97")%>%
  group_by(rs,categ_pmsi)%>%
  mutate(total=sum(n_patients))%>%
  ggplot() +
  geom_polygon(data=france_regions_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.5) +
  geom_polygon(data=france_departements_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.2) +
  theme_void() +
  coord_map()+
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,
             
             alpha=1) +
  scale_size(range = c(0, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Répartition des structures ayant effectué de la reconstruction mammaire en 2019", subtitle = "Données PMSI 2018")

reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  filter(substr(finessGeoDP,1,2) != "97")%>%
  group_by(rs,categ_pmsi)%>%
  mutate(total=sum(n_patients))%>%
  filter(total>100, .keep_all=TRUE)%>%
  ggplot() +
  geom_polygon(data=france_regions_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.5) +
  geom_polygon(data=france_departements_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.2) +
  theme_void() +
  coord_map()+
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,
             alpha=1) +
  scale_size(range = c(4, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Répartition des structures ayant effectué de la reconstruction mammaire sur plus de 100 patientes en 2019", subtitle = "Données PMSI 2018")

##### 3.3 Lipomodelage #####
  
LIPO<-reconstruction[which(reconstruction$acte=="lipomodelage"),]
LIPO<-LIPO%>%
    filter(substr(finessGeoDP,1,2) != "97")%>%
  filter(year=="2019", .keep_all=TRUE)%>%
    group_by(rs, latitude, longitude, categ_pmsi, finessGeoDP)%>%
    summarise(total=sum(n_patients))
  
  p+geom_point(data=LIPO,
               aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=3)+
    scale_size(range = c(4, 20), name="Nombre total de patientes")+
    scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
    labs(title="Répartition des structures ayant effectué des techniques de lipomodelage en 2019", subtitle = "Données PMSI 2019")
 
  
 LIPO%>%
    filter(substr(finessGeoDP,1,2) %in% DEPS_IDF, .keep_all=TRUE)%>%
    #filter(total>100, .keep_all=TRUE)%>%
    ggplot() +geom_polygon(data=ile_de_france_layer,
                           aes(x=long,
                               y=lat,
                               group=group),
                           color="black",
                           alpha=0,
                           size=.2) +
    theme_void() +
    coord_map()  +
    geom_point(aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=1)+
    scale_size(range=c(4,10))+
    scale_color_manual(values=COLOR_PMSI) +
    labs(title="Répartition des structures ayant effectué des techniques de lipomodelage en 2019 (Ile de France)", 
         subtitle = "Données PMSI 2019")
 
 LIPO%>%
   filter(substr(finessGeoDP,1,2) %in% DEPS_PARIS, .keep_all=TRUE)%>%
   #filter(total>100, .keep_all=TRUE)%>%
   ggplot() +geom_polygon(data=paris_layer,
                          aes(x=long,
                              y=lat,
                              group=group),
                          color="black",
                          alpha=0,
                          size=.2) +
   theme_void() +
   coord_map()  +
   geom_point(aes(x=longitude,
                  y=latitude,
                  color=categ_pmsi,size=total),
              shape=16,alpha=1)+
   scale_size(range=c(4,10))+
   scale_color_manual(values=COLOR_PMSI) +
   labs(title="Répartition des structures ayant effectué des techniques de lipomodelage en 2019 (Ile de France)", 
        subtitle = "Données PMSI 2019")
 
  
   p+geom_point(data=LIPO[which(LIPO$total>20),],
               aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=3)+
    scale_size(range = c(4, 20), name="Nombre total de patientes")+
    scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
    labs(title="Répartition des structures ayant effectué des techniques de lipomodelage sur plus de 20 patientes en 2019", subtitle = "Données PMSI 2019")
  
   
   
   
  
  p+geom_point(data=LIPO[which(LIPO$total>50),],
               aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=3)+
    scale_size(range = c(4, 20), name="Nombre total de patientes")+
    scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
    labs(title="Répartition des structures ayant effectué des techniques de lipomodelage sur plus de 50 patientes en 2019", subtitle = "Données PMSI 2019")

  LIPO%>%
    filter(substr(finessGeoDP,1,2) %in% DEPS_PARIS, .keep_all=TRUE)%>% 
    filter(total>50, .keep_all=TRUE)%>%
    ggplot() +geom_polygon(data=paris_layer,
                           aes(x=long,
                               y=lat,
                               group=group),
                           color="black",
                           alpha=0,
                           size=.2) +
    theme_void() +
    coord_map()  +
    geom_point(aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=1)+
    scale_size(range=c(4,10))+
    scale_color_manual(values=COLOR_PMSI) +
    labs(title="Répartition des structures ayant effectué des techniques de lipomodelage sur plus de 50 patientes en 2019 (Ile de France)", 
         subtitle = "Données PMSI 2019")
  
##### 3.4 Implants #####
  
  IMPLANT<-reconstruction[which(reconstruction$code=="QEMA006"),]
  IMPLANT<-IMPLANT%>%
    filter(substr(finessGeoDP,1,2) != "97")%>%
    filter(year=="2019", .keep_all=TRUE)%>%
    group_by(rs, latitude, longitude, categ_pmsi, finessGeoDP)%>%
    summarise(total=sum(n_patients))
  
  p+geom_point(data=IMPLANT,
               aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=3)+
    scale_size(range = c(4, 20), name="Nombre total de patientes")+
    scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
    labs(title="Répartition des structures ayant effectué des techniques d'implant en 2019", subtitle = "Données PMSI 2019")
  
  IMPLANT%>%
    filter(substr(finessGeoDP,1,2) %in% DEPS_IDF, .keep_all=TRUE)%>%
    #filter(total>100, .keep_all=TRUE)%>%
    ggplot() +geom_polygon(data=ile_de_france_layer,
                           aes(x=long,
                               y=lat,
                               group=group),
                           color="black",
                           alpha=0,
                           size=.2) +
    theme_void() +
    coord_map()  +
    geom_point(aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=1)+
    scale_size(range=c(4,10))+
    scale_color_manual(values=COLOR_PMSI) +
    labs(title="Répartition des structures ayant effectué des techniques d'implants en 2019 (Ile de France)", 
         subtitle = "Données PMSI 2019")
  
  
  p+geom_point(data=IMPLANT[which(IMPLANT$total>20),],
               aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=3)+
    scale_size(range = c(4, 20), name="Nombre total de patientes")+
    scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
    labs(title="Répartition des structures ayant effectué des techniques d'implant sur plus de 20 patientes en 2019", subtitle = "Données PMSI 2019")
  
  
  p+geom_point(data=IMPLANT[which(IMPLANT$total>50),],
               aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=3)+
    scale_size(range = c(4, 20), name="Nombre total de patientes")+
    scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
    labs(title="Répartition des structures ayant effectué des techniques d'implant sur plus de 50 patientes en 2019", subtitle = "Données PMSI 2019")
  
  
  

  IMPLANT%>%
    filter(substr(finessGeoDP,1,2) %in% DEPS_PARIS, .keep_all=TRUE)%>%
    filter(total>50, .keep_all=TRUE)%>%
    ggplot() +geom_polygon(data=paris_layer,
                           aes(x=long,
                               y=lat,
                               group=group),
                           color="black",
                           alpha=0,
                           size=.2) +
    theme_void() +
    coord_map()  +
    geom_point(aes(x=longitude,
                   y=latitude,
                   color=categ_pmsi,size=total),
               shape=16,alpha=1)+
    scale_size(range=c(4,10))+
    scale_color_manual(values=COLOR_PMSI) +
    labs(title="Répartition des structures ayant effectué des techniques d'implants sur plus de 50 patientes en 2019 (Ile de France)", 
         subtitle = "Données PMSI 2019")
  
  
##### 3.5 DIEP #####

DIEP<-reconstruction[which(reconstruction$acte=="DIEP"),]
DIEP<-DIEP%>%
  filter(substr(finessGeoDP,1,2) != "97")%>%
  group_by(rs, latitude, longitude, categ_pmsi, finessGeoDP)%>%
  summarise(total=sum(n_patients))

p+geom_point(data=DIEP,
             aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=3)+
  scale_size(range = c(4, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Répartition des structures ayant effectué des techniques de lambeaux libres ou DIEP en 2019", subtitle = "Données PMSI 2019")



DIEP%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_IDF, .keep_all=TRUE)%>%
  #filter(total>100, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=ile_de_france_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des techniques DIEP en 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")


DIEP%>%
  filter(total>20, .keep_all=TRUE)%>%
  ggplot() +
  geom_polygon(data=france_regions_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.5) +
  geom_polygon(data=france_departements_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.2) +
  theme_void() +
  coord_map()+geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16, alpha=1)+
  scale_size(range = c(4, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant eu plus de 20 patientes pour des techniques de lambeaux libres ou DIEP en 2019", subtitle = "Données PMSI 2019")


DIEP%>%
  filter(total>50, .keep_all=TRUE)%>%
  ggplot() +
  geom_polygon(data=france_regions_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.5) +
  geom_polygon(data=france_departements_layer,
               aes(x=long,
                   y=lat,
                   group=group),
               color="black",
               alpha=0,
               size=.2) +
  theme_void() +
  coord_map()+geom_point(aes(x=longitude,
                             y=latitude,
                             color=categ_pmsi,size=total),
                         shape=16, alpha=1)+
  scale_size(range = c(4, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant eu plus de 50 patientes pour des techniques de lambeaux libres ou DIEP en 2019", subtitle = "Données PMSI 2019")

DIEP%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_PARIS, .keep_all=TRUE)%>%
  filter(total>50, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=paris_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des techniques DIEP sur plus de 50 patientes en 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")


##### 3.6 Grand Dorsal #####
DORSAL<-reconstruction[which(reconstruction$acte=="grand dorsal"),]
DORSAL<-DORSAL%>%
  filter(substr(finessGeoDP,1,2) != "97")%>%
  group_by(rs, latitude, longitude, categ_pmsi, finessGeoDP)%>%
  summarise(total=sum(n_patients))

p+geom_point(data=DORSAL,
             aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=3)+
  scale_size(range = c(4, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Répartition des structures ayant effectué des techniques de lambeaux de grand dorsal en 2019", subtitle = "Données PMSI 2019")

DORSAL%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_PARIS, .keep_all=TRUE)%>%
 # filter(total>50, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=paris_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des techniques de grand dorsalen 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")



p+geom_point(data=DORSAL[which(DORSAL$total>50),],
             aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=3)+
  scale_size(range = c(4, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Répartition des structures ayant effectué des techniques de lambeaux de grand dorsal, sur plus de 20 patientes en 2019", subtitle = "Données PMSI 2019")


DORSAL%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_PARIS, .keep_all=TRUE)%>%
  filter(total>50, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=paris_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des techniques de grand dorsal sur plus de 50 patientes en 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")


##### 3.6 PAP/Gracillis#####
PAP<-reconstruction[which(reconstruction$acte=="Gracillis/PAP/lambeau libre"),]
PAP<-PAP%>%
  filter(substr(finessGeoDP,1,2)!="97")%>%
  filter(substr(finessGeoDP,1,2)!="98")%>%
  group_by(rs, latitude, longitude, categ_pmsi, finessGeoDP)%>%
  summarise(total=sum(n_patients))

p+geom_point(data=PAP,
             aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=3)+
  scale_size(range = c(4, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Répartition des structures ayant effectué des techniques de PAP /Gracillis en 2019", subtitle = "Données PMSI 2019")

PAP%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_PARIS, .keep_all=TRUE)%>%
  # filter(total>50, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=paris_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des techniques de PAP/Gracillis 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")



p+geom_point(data=PAP[which(PAP$total>50),],
             aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=3)+
  scale_size(range = c(4, 20), name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Répartition des structures ayant effectué des techniques de PAP/Gracillis sur plus de 20 patientes en 2019", subtitle = "Données PMSI 2019")


PAP%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_PARIS, .keep_all=TRUE)%>%
  filter(total>50, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=paris_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des techniques de PAP/Gracillis sur plus de 50 patientes en 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")

##### 3.8 Reconstruction du mamelon #####

Mamelon<-reconstruction[which(reconstruction$acte=="plaque areolo mamelonnaire"),]
Mamelon<-Mamelon%>%
  filter(substr(finessGeoDP,1,2) != "97")%>%
  group_by(rs, latitude, longitude, categ_pmsi, finessGeoDP)%>%
  summarise(total=sum(n_patients))

p+geom_point(data=Mamelon,
             aes(x=longitude,
                 y=latitude,
                 color= categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_color_manual(values=COLOR_PMSI)+
  scale_size(range=c(4,20))+
  labs(title="Répartition des structures ayant effectué des reconstructions de la plaque aréolomamelonnaire (QEMA009 et QEMA010) en 2019", subtitle = "Données PMSI 2019")



Mamelon%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_IDF, .keep_all=TRUE)%>%
  #filter(total>100, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=ile_de_france_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,20))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des reconstruction de la plaque aréolo mamelonnaire en 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")

p+geom_point(data=Mamelon[which(Mamelon$total>50),],
             aes(x=longitude,
                 y=latitude,
                 color= categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,20))+
  scale_color_manual(values=COLOR_PMSI)+
  labs(title="Répartition des structures ayant effectué des reconstructions de la plaque aréolomamelonnaire, sur plus de 50 patientes en 2019", subtitle = "Données PMSI 2019")


Mamelon%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_IDF, .keep_all=TRUE)%>%
  filter(total>50, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=ile_de_france_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des reconstruction de la plaque aréolo mamelonnaire sur plus de 50 patientes en 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")



##### 3.9 Reconstruction du mamelon #####

DERMO<-reconstruction[which(reconstruction$acte=="dermopigmentation"),]
DERMO<-DERMO%>%
  filter(substr(finessGeoDP,1,2) != "97")%>%
  group_by(rs, latitude, longitude, categ_pmsi, finessGeoDP)%>%
  summarise(total=sum(n_patients))

p+geom_point(data=DERMO,
             aes(x=longitude,
                 y=latitude,
                 color= categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_color_manual(values=COLOR_PMSI)+
  scale_size(range=c(4,20))+
  labs(title="Répartition des structures ayant effectué des dermopigmentation en 2019", subtitle = "Données PMSI 2019")



DERMO%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_IDF, .keep_all=TRUE)%>%
  #filter(total>100, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=ile_de_france_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des dermopigmentation en 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")

p+geom_point(data=DERMO[which(DERMO$total>50),],
             aes(x=longitude,
                 y=latitude,
                 color= categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,20))+
  scale_color_manual(values=COLOR_PMSI)+
  labs(title="Répartition des structures ayant effectué des dermopigmentations, sur plus de 50 patientes en 2019", subtitle = "Données PMSI 2019")


DERMO%>%
  filter(substr(finessGeoDP,1,2) %in% DEPS_IDF, .keep_all=TRUE)%>%
  filter(total>20, .keep_all=TRUE)%>%
  ggplot() +geom_polygon(data=ile_de_france_layer,
                         aes(x=long,
                             y=lat,
                             group=group),
                         color="black",
                         alpha=0,
                         size=.2) +
  theme_void() +
  coord_map()  +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=categ_pmsi,size=total),
             shape=16,alpha=1)+
  scale_size(range=c(4,10))+
  scale_color_manual(values=COLOR_PMSI) +
  labs(title="Répartition des structures ayant effectué des dermopigmentations sur plus de 20 patientes en 2019 (Ile de France)", 
       subtitle = "Données PMSI 2019")


dev.off()

#### 4. Spider Graph#####


pdf(
  file = file.path("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/01.Graphes", "spider_50.pdf"),
  height = 10,
  width = 15
)
TOP_50<-reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs, acte) %>%
  summarise(total=sum(n_patients))%>%
  filter(total>50, .keep_all=TRUE)%>%
  distinct(rs) #nombre de structure dont au moins une activité a été réalisées sur plus de 50 patientes

name<-as.list(TOP_50$rs)


TOP_100<-reconstruction%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  group_by(rs) %>%
  summarise(total=sum(n_patients))%>%
  filter(total>100, .keep_all=TRUE)%>%
  distinct(rs)#nombre de structure dont au moins une activité a été réalisées sur plus de 50 patientes


name_100<-as.list(TOP_100$rs)

#90 etablissements qui ont pris en charge plus de 1OO patientes en 2019
#109 ont pris en charge au moins 50 patients sur un type d'actes
test<-reconstruction%>%
  filter(rs %in% name, .keep_all=TRUE)%>%
  group_by(acte, year, rs, categ_pmsi, latitude, longitude)%>%
  summarise(total=sum(n_patients))%>%
  filter(year=="2019", .keep_all=TRUE)%>%
  select(rs,acte,total, categ_pmsi)

require(reshape2)
t<-dcast(test, rs + categ_pmsi ~ acte, mean, value.var="total", fill=0)
max<-data.frame(c("Max", "Min"),c("NA","NA"),c( 180,0),c(52,0),c(207,0),c(128,0),c(159,0),c(311,0),c(11,0),c(459,0),c(115,0))
names(max)<-c("rs" ,"categ_pmsi" , "dedoublement du sein restant","dermopigmentation" , "DIEP" , 
              "Gracillis/PAP/lambeau libre","grand dorsal" , "implant" ,                    
             "lambeau pedicule/ TRAM", "lipomodelage","plaque areolo mamelonnaire") 
              
t<-dplyr::bind_rows(max, t)



### fonction radargraph (pas de moi)

create_beautiful_radarchart <- function(data, color = NULL, 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Personnaliser le polygone
    pcol =scales::alpha(color, 1), pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Personnaliser la grille
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Personnaliser l'axe
    axislabcol = "grey", 
    # Étiquettes des variables
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}



#Parmi les structures avec plus de 50 actes on va en prendre 1 de chaque type et regarder si différence de structure

is.num <- sapply(t, is.numeric)
t[is.num] <- lapply(t[is.num], round, 0)

data<-data.frame(t[,3:11])
sapply(data, class)



titles<-as.list(t[,1])
color<-data.frame(t[,2])
color<-color%>%
  mutate(col= case_when(t...2.=="CH"~ "#FC4E07",
                        t...2.=="CHR/U"~"#045991",
                        t...2.=="CLCC"~"#B3DE3A",
                        t...2.=="Prive"~"#E7B800",
                        t...2.=="PSPH/EBNL"~"#D83E81" ))
list_color<-as.list(color[,2])

#4 premiers
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 1:4){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}


#4 autres

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 5:8){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}


par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 9:12){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 13:16){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 17:20){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 21:24){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 25:28){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 29:32){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}


par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 33:36){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 37:40){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 41:44){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}



par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 45:48){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 49:52){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 53:56){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 57:60){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 61:64){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 65:68){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 69:72){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 73:76){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 77:80){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 81:84){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 85:88){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 89:92){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 93:96){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 97:100){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 101:104){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 105:108){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}
par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))
for(i in 109:){
  create_beautiful_radarchart(
    data = data[c(1:2, i+2), ],
    color = c(colors=list_color[i+2]), title = titles[i+2])
}

dev.off()

##### 5.Mesure de la diversité de l'activité #####
#exclusion de l'acte de dedoublement du sein
#on garde les reconstruction de la plaque aréolo mammelonnaire, prise en charge complète
#on prend l'activité de 2019

diversite<-reconstruction%>%
  filter(year=="2019", acte!="dedoublement du sein restant", .keep.all=TRUE)%>%
  group_by(rs, categ_pmsi)%>%
  mutate(total=sum(n_patients))%>%
  group_by(rs,acte)%>%
  mutate(n= sum(n_patients), ratio=(n/total)*100)



require(reshape2)
#table avec pourcentage
diversite<-dcast(diversite, rs + categ_pmsi ~ acte, mean, value.var="ratio", fill=0)
diversite<-melt(diversite, id=c("rs", "categ_pmsi"), na.rm=TRUE) 

#on a ajouter les 0, pour les actes qui n'apparaissaient pas
#on a une table avec des poucentage d'activité, même pour les activités nulles

#On va construire un indicateur qui calcule la moyenne des distances entre les pourcentage (concentration) et qui est pondéré par le nombre total
# de type d'activité pratiquées
# on discrimine les etablissements spécialisés
# mais aussi ceux dont la répartition des activités est très hétérogène
# plus ce score est elevé moins l'établissement a une activité diversifiée

#on récupère les indicatrices pour faire un score de 1 à 8 qui permet de voir combien de type d'actes sont effectués par l'établissement
d<-reconstruction%>%
  group_by(acte, year, rs, categ_pmsi, latitude, longitude)%>%
  summarise(total=sum(n_patients))%>%
  filter(year=="2019", acte!="dedoublement du sein restant", .keep_all=TRUE)%>%
  select(rs,acte,total, categ_pmsi)

require(reshape2)
d<-dcast(d, rs +categ_pmsi ~ acte, value.var = "total") #on récupère les indicatrices


summary(d)
# On calcul un score (somme des indicatrices) et on corrige le beug pour la CLINIQUE DU PARC
d<-d%>%
  mutate(score = rowSums(across(where(is.numeric))))%>%
  mutate(score = replace(score, score == 14, 5))
  
table(d$score)
#dispersion cohérente

#on merge la table avec les pourcentages et celle avec le score, pour préparer les variables pour la diversité effective
diversite<-diversite%>%
  inner_join(d %>% select(rs, score), by=c("rs"="rs"))%>%
  group_by(rs)%>%
  arrange(value, .by_group = TRUE) #on trie par ordre decroissant de pourcentage


# #on calcule le score de diversité avec les ecarts de pourcentage et la pondération par l'inverse du score
# DIV<- data.table(diversite)
# DIV<-DIV[ , list(rs,categ_pmsi, variable,score, value,Diff=diff(value))] #calcul des différences entre les lignes
# DIV$Diff[which(DIV$Diff<0)]<-0
# 
# DIV<-DIV%>%
#   group_by(rs)%>%
#  mutate(mean_diff=if_else(score!=1, sum(Diff)/(score-1), 100), score_div=mean_diff*(1/score))%>%
#   distinct(rs, .keep_all=TRUE)
# 
# 
# DIV%>%
#   ggplot(aes(x=score_div,fill=categ_pmsi))+
#   geom_histogram(binwidth = 1)+
#   theme_light()+
#   scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
#   labs(title="Répartition des score de diversité", 
#        x="Score de diversité", y="Nombre de structures" )
# 
# 
# DIV%>%
#   ggplot(aes(x=score,fill=categ_pmsi))+
#   geom_histogram(binwidth = 1)+
#   theme_light()+
#   scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
#   labs(title="Répartition du nombre d'activités pratiquées", 
#        x="Score de diversité", y="Nombre de structures" )
# 
# 
# DIV%>%
#   mutate(score_1=as.character(score))%>%
#   ggplot(aes(x=score_div,fill=score_1))+
#   geom_histogram(binwidth = 1)+
#   theme_light()+
#   scale_fill_brewer(palette="Set3", name="Nombre de types d'actes pratiqués")+
#   labs(title="Répartition des score de diversité", 
#        x="Score de diversité", y="Nombre de structures" )
# 
# 

##### indice de diversite ######

d<-diversite%>%
  group_by(rs)%>%
  mutate(sc_diversite=1/sum((value/100)^2))

d%>%group_by(rs)%>%
  distinct(rs, .keep_all=TRUE)%>%
  ggplot(aes(x=sc_diversite, fill=as.character(score)))+
  geom_histogram(binwidth = 0.5)+
  theme_light()+
  scale_fill_brewer(palette="Set3", name="Nombre de types d'actes pratiqués")+
  labs(title="Répartition des score de diversité", 
       x="Score de diversité", y="Nombre de structures" )
  
  

d%>%group_by(rs)%>%
  distinct(rs, .keep_all = TRUE)%>%
  ggplot(aes(x=sc_diversite,fill=categ_pmsi))+
  geom_histogram()+
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
  labs(title="Répartition du nombre d'activités pratiquées, en diversité effective", 
       x="Score de diversité", y="Nombre de structures" )

#### evolution de l'activite au cours des 5 années


t<- reconstruction%>%
  filter(year=="2019" | year=="2015", .keep_all=TRUE)%>%
  group_by(rs,year)%>%
  mutate(n=sum(n_patients))%>%
  mutate(n_2015=ifelse(year=="2015",n,0), n_2019=ifelse(year=="2019",n,0))

t<-t%>%
  group_by(rs, year)%>%
  distinct(rs,year, .keep_all=TRUE)

t<-t%>%
  group_by(rs)%>%
  mutate(occur=n())%>%
  mutate(var =ifelse(occur==2, n[year =="2019"] - n[year=="2015"], 0), variation=ifelse(occur==2, var/n[year=="2015"], 0))
 

 t%>%
   filter(year=="2019", .keep_all=TRUE)%>%
   #summary(t$variation)
   ggplot(aes(x=variation,fill=categ_pmsi))+
   geom_histogram(binwidth=0.05)+
   theme_light()+
   scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
   labs(title="Répartition de la variation du taux d'activité entre 2015 et 2019", 
        x="ariation du taux d'activité", y="Nombre de structures" )
   
  
t<-table1(~ volume + sc_diversite |categ_pmsi, 
       data=d,topclass="Rtable1-zebra", caption= "Distribution of variable used for the total score (1b)", footnote="Data: PMSI")

# resultats estatis pour la satisfaction des patients pour l'établissement

estatis<-read.csv("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/resultats-esatisca-mco-open-data-2019.csv", 
                  colClasses= c("score_all_ajust"="numeric"), dec=",", sep=";")


reconstruction<-reconstruction%>%
  inner_join(estatis %>% select(finess_geo, score_all_ajust, classement), by=c("finessGeoDP"="finess_geo"))

summary(reconstruction)
########### Creation de la table pour l'algo ############

# bdd_finale<-reconstruction%>%
#   filter(year=="2019", .keep_all=TRUE)%>%
#   group_by(rs)%>%
#   mutate(volume=sum(n_patients))%>%
#   inner_join(DIV%>% select (rs,score_div ), by=c("rs"="rs"))%>%
#   select(rs, latitude, longitude,categ_pmsi,volume, score_div, acte, adresse)

bdd_finale<-reconstruction%>%
    filter(year=="2019",acte!="dedoublement du sein restant", .keep_all=TRUE)%>%
    group_by(rs)%>%
    mutate(volume=sum(n_patients))%>%
    distinct(rs, acte, .keep_all=TRUE)%>% 
    inner_join(d%>% select (rs,variable,sc_diversite), by=c("rs"="rs", "acte"="variable"))%>%
    inner_join(t%>% filter(year=="2019") %>% select(rs, variation) , by=c("rs"="rs"))%>%
    select(finessGeoDP, rs, latitude, longitude,categ_pmsi,volume, variation,sc_diversite, score_all_ajust, acte, adresse)


dummy<-bdd_finale%>%
  dcast(bdd_finale,  rs  ~ acte, length)


bdd_finale<-dummy%>%
  inner_join(bdd_finale%>%distinct(rs, .keep_all=TRUE) %>% select(volume, variation,sc_diversite, score_all_ajust, adresse, finessGeoDP, rs, latitude, longitude,categ_pmsi)
             , by=c("rs"="rs"))
 


write.csv(bdd_finale, "/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/bdd_finale.csv", row.names=FALSE)
  
bdd_finale<-as.data.frame(bdd_finale)
summarise(bdd_finale)

finess_reco<-bdd_finale[,1:2]
write.csv(finess_reco, "/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/finess_reco.csv", row.names=FALSE)

##### Stats sur les indicateurs: sa cohérence avec le volume ####

bdd_finale%>%
  filter(volume>50, .keep_all=TRUE)%>%
  ggplot(aes(x=sc_diversite,fill=categ_pmsi))+
  geom_histogram()+
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
  labs(title="Répartition du nombre d'activités pratiquées, en diversité effective", 
       x="Score de diversité", y="Nombre de structures" )

summary(bdd_finale)
