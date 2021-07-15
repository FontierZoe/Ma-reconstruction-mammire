###### Algorithme de reconstruction mammaire ##############
############### Fontier Zoé - AVril 2021 ##################


library(dplyr)
library(formattable)
library(table1)
library(sjPlot)
library(stringi)
library(questionr)
library(ggplot2)
library(broom)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(gt)
source("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/utils.R")


#### lecture tables ####

reconstruction<-read.csv("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/reconstruction_v0.csv")


#### 1. Pre processing de la base de données ####

###### 1.1 Score de diversité ######

#exclusion de l'acte de dedoublement du sein
#on garde les reconstruction de la plaque aréolo mammelonnaire, prise en charge complète
#on prend l'activité de 2019
#on regarde la répartition de chaque acte sur le total de l'activité

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

#on a ajouté les 0, pour les actes qui n'apparaissaient pas
#on a une table avec des poucentage d'activité, même pour les activités nulles

#On va construire un indicateur qui calcule la moyenne des distances entre les pourcentages (concentration) et qui est pondéré 
#par le nombre total de type d'activité pratiquées
# on discrimine les etablissements spécialisés
# mais aussi ceux dont la répartition des activités est très hétérogène
# plus ce score est elevé moins l'établissement a une activité diversifiée

#on récupère les indicatrices pour faire un score de 1 à 8 qui permet de voir combien de type d'actes 
#sont effectués par l'établissement

d<-reconstruction%>%
  group_by(acte, year, rs, categ_pmsi, latitude, longitude)%>%
  summarise(total=sum(n_patients))%>%
  filter(year=="2019", acte!="dedoublement du sein restant", .keep_all=TRUE)%>%
  select(rs,acte,total, categ_pmsi)

require(reshape2)
dummy<-dcast(d, rs +categ_pmsi ~ acte, value.var = "total") #on récupère les indicatrices


summary(dummy)
# On calcul un score (somme des indicatrices) et on corrige le beug pour la CLINIQUE DU PARC
d<-dummy%>%
  mutate(score = rowSums(across(where(is.numeric))))%>%
  mutate(score = replace(score, score == 14, 5))

table(d$score)

#dispersion cohérente

#on merge la table avec les pourcentages et celle avec le score, pour préparer les variables pour la diversité effective
diversite<-diversite%>%
  inner_join(d %>% select(rs, score), by=c("rs"="rs"))%>%
  group_by(rs)%>%
  arrange(value, .by_group = TRUE) #on trie par ordre decroissant de pourcentage

#calcul du score de diversité
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




##### 1.2 Variation de l'activité #####

t<- reconstruction%>%
  filter(year=="2019" | year=="2015",acte!="dedoublement du sein restant", .keep_all=TRUE)%>%
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
       x="Variation du taux d'activité", y="Nombre de structures" )



##### 1.3 Qualité des soins ##### 

estatis<-read.csv("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/resultats-esatisca-mco-open-data-2019.csv", 
                  colClasses= c("score_all_ajust"="numeric"), dec=",", sep=";")


reconstruction<-reconstruction%>%
  inner_join(estatis %>% select(finess_geo, score_all_ajust, classement), by=c("finessGeoDP"="finess_geo"))

summary(reconstruction)


##### 1.4 Regroupement des variables + volume #####
bdd_finale<-reconstruction%>%
  filter(year=="2019",acte!="dedoublement du sein restant", .keep_all=TRUE)%>%
  group_by(rs)%>%
  mutate(volume=sum(n_patients))%>%
  distinct(rs, acte, .keep_all=TRUE)%>% 
  inner_join(d%>% select (rs,variable,sc_diversite), by=c("rs"="rs", "acte"="variable"))%>%
  inner_join(t%>% filter(year=="2019") %>% select(rs, variation) , by=c("rs"="rs"))%>%
  select(finessGeoDP, rs, latitude, longitude,categ_pmsi,volume, variation,sc_diversite, score_all_ajust, acte, adresse, ville)

#on reprend la table dummy créée au dessus
bdd_finale<-dummy%>%
  inner_join(bdd_finale%>%distinct(rs, .keep_all=TRUE) %>% select(volume, variation,sc_diversite, score_all_ajust, adresse,ville, finessGeoDP, rs, latitude, longitude)
             , by=c("rs"="rs"))


Encoding(bdd_finale$rs) <- "latin1"
write.csv(bdd_finale, "/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/bdd_finale.csv", row.names=FALSE)

bdd_finale<-as.data.frame(bdd_finale)
summarise(bdd_finale)

finess_reco<-bdd_finale[,1:2]
write.csv(finess_reco, "/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/finess_reco.csv", row.names=FALSE)

#### 2. Stats sur les indicateurs: sa cohérence avec le volume ####
var_label(bdd_finale$sc_diversite)<-"Diversity Score for technics"
var_label(bdd_finale$volume)<-"Volume of activity"
var_label(bdd_finale$score_all_ajust)<-"Quality score (all services)"
var_label(bdd_finale$variation)<-"Variation rate of volume of activity (2015-2019)"


#volume
bdd_finale%>%
  ggplot(aes(x=volume,fill=categ_pmsi))+
  geom_histogram()+
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
  labs(title="Répartition du volume d'activités pratiquées", 
       x="Volume d'activité", y="Nombre de structures" )


#diversité selon volume
bdd_finale%>%
  filter(volume>50, .keep_all=TRUE)%>%
  ggplot(aes(x=sc_diversite,fill=categ_pmsi))+
  geom_histogram()+
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
  labs(title="Répartition du nombre d'activités pratiquées, en diversité effective", 
       x="Score de diversité", y="Nombre de structures" )

summary(bdd_finale)



#### Stats diversité ####


table1(~ volume +sc_diversite+variation +score_all_ajust|categ_pmsi, bdd_finale, 
       topclass="Rtable1-zebra", "Répartition des différentes variables de recommandations, selon le type d'établissement", footnote="Data: PMSI 2015 à 2019")


gd<-bdd_finale%>%
  ggplot(aes(x=sc_diversite))+
  geom_histogram(fill="slategray")+
  theme_light()+
  labs(title="Distribution of the diversity score", 
       subtitle="n=528",
       x="Diversity score", y="Number of establishments", caption="Data PMSI 2019" )+ 
  theme(legend.title = element_text( size = 11),legend.text = element_text(size=10),text = element_text(size=11))+
  ggeasy::easy_center_title()
 
gv<-bdd_finale%>%
  ggplot(aes(x=volume))+
  geom_histogram(color="slategray3", fill="slategray2")+
  theme_light()+
  labs(title="Distribution of the volume of activity", 
       subtitle="n=528",
       x="Volume of activity", y="Number of establishments", caption="Data PMSI 2019" )+ 
  theme(legend.title = element_text( size = 11),legend.text = element_text(size=10),text = element_text(size=11))+
  ggeasy::easy_center_title()


gvar<-bdd_finale%>%
ggplot(aes(x=variation))+
  geom_histogram(color= "steelblue4",fill="steelblue3")+
  theme_light()+
  labs(title="Variation of activity between 2015 and 2019 \n for the differents establishments", 
       subtitle="n=528",
       x="Variation of activity", y="Number of establishments", caption="Data PMSI 2015-2019" )+ 
  theme(legend.title = element_text( size = 11),legend.text = element_text(size=10),text = element_text(size=11))+
  ggeasy::easy_center_title()


gq<-bdd_finale%>%
  ggplot(aes(x=score_all_ajust))+
  geom_histogram(color= "wheat3",fill="wheat2")+
  theme_light()+
  labs(title="Quality score for service in 2019", 
       subtitle="n=528, NA=18",
       x="Quality score", y="Number of establishments", caption="Data HAS, 2019, score estatis" )+ 
  theme(legend.title = element_text( size = 11),legend.text = element_text(size=10),text = element_text(size=11))+
  ggeasy::easy_center_title()

ggarrange(gv,gvar,gd,gq,  ncol=2, nrow=2)

bdd_finale%>%
  filter(volume>50, .keep_all=TRUE)%>%
  ggplot(aes(x=sc_diversite,fill=categ_pmsi))+
  geom_histogram()+
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
  labs(title="Répartition du nombre d'activités pratiquées, en diversité effective", 
       subtitle="n=528",
       x="Score de diversité", y="Nombre de structures", caption="Calcul à partir des données PMSI 2019" )+ theme(legend.title = element_text( size = 14),
                                                                 legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title()

bdd_finale%>%
  pairwise_t_test(sc_diversite~ categ_pmsi, p.adjust.method = "bonferroni")
#ce score différencie bien les grandes catégories d'établissement. 

bdd_finale%>%
  ggplot(aes(x=sc_diversite,y=volume))+
  geom_point()+
  theme_light()+
  labs(title="Répartition du nombre d'activités pratiquées, en diversité effective avec le volume", 
       subtitle="n=528",
       x="Score de diversité", y="Nombre de patientes", caption="Calcul à partir des données PMSI 2019" )+ theme(legend.title = element_text( size = 14),
                                                                                                                  legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title()



bdd_finale%>%
  ggplot(aes(x=sc_diversite,y=variation))+
  geom_point()+
  theme_light()+
  labs(title="Répartition du nombre d'activités pratiquées, en diversité effective avec la variation de volume", 
       subtitle="n=528",
       x="Score de diversité", y="Variation du volume", caption="Calcul à partir des données PMSI 2015-2019" )+ theme(legend.title = element_text( size = 14),
                                                                                                                 legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title()
#pas de correlation entre score de diversité et variation du volume, 2 dimensions différenets

bdd_finale%>%
  ggplot(aes(x=volume))+
  geom_density()+
  theme_light()+
  labs(title="Répartition du volume d'activités", 
       subtitle="n=528",
       x="Volume", y="Etablissement", caption="Calcul à partir des données PMSI 2019" )+ theme(legend.title = element_text( size = 14),
                                                                                                                 legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title()


bdd_finale%>%
  ggplot(aes(x=score_all_ajust))+
  geom_density()+
  theme_light()+
  labs(title="Répartition du score estatis", 
       subtitle="n=528",
       x="score estatis", y="Etablissement", caption="Données HAS 2019" )+ theme(legend.title = element_text( size = 14),
                                                                                               legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title()



bdd_finale%>%
  pairwise_t_test(sc_diversite~ categ_pmsi, p.adjust.method = "bonferroni")

bdd_finale%>%
  pairwise_t_test(volume~ categ_pmsi, p.adjust.method = "bonferroni")


bdd_finale%>%
  filter(volume>50, .keep_all=TRUE)%>%
  ggplot(aes(x=score_all_ajust,fill=categ_pmsi))+
  geom_histogram()+
  theme_light()+
  scale_fill_manual(values=COLOR_PMSI, name="Type d'établissement") +
  labs(title="Répartition du score e statis", 
       subtitle="n=528",
       x="Score de qualité estatis", y="Nombre de structures", caption="Calcul à partir des données PMSI 2019" )+ theme(legend.title = element_text( size = 14),
                                                                                                                  legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title()

bdd_finale%>%
  pairwise_t_test(score_all_ajust~ categ_pmsi, p.adjust.method = "bonferroni")


#matrice de corraltion
M<- bdd_finale%>%
  select(volume, variation, sc_diversite,score_all_ajust)

row.names(M) <- M$libelle_dep

M<-as.matrix(M)
M<-cor(M, use = "complete.obs")
corrplot(M,type="upper", tl.col="black")
#pas de correlation, donc represente bien des dimensions différentes



distance<-read.csv("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/res_200km.csv", sep=",")



#### Recommandation :test (Partie qui peut être ignorée) ####

# test avec selection un seul geocode ex: 01390
##### Ain #####

t<-distance%>%
  filter(code_geo=="01390", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_quality=min(score_all_ajust))

summary(bdd_finale$score_all_ajust)

 #moyenne pondérée


bdd_finale%>%
  mutate(cap_vol=ifelse(volume>250, 250, volume))%>%
  ggplot(aes(x=volume,y=cap_vol))+
  geom_line()+
  theme_light()
  
  
  
distance%>%
  filter(code_geo=="01390", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(2*norm_vol+4*norm_diversite+ 1*norm_var+5*norm_dist_km + 3*norm_quality)/15
         )%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant de l'Ain",
    subtitle = "Code Geo =01390, score en moyenne pondéréé"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 157,4km, Durée moyenne de trajet= 102 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")

#moyenne normale
distance%>%
  filter(code_geo=="01390", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(norm_vol+norm_diversite+ norm_var+norm_dist_km + norm_quality)/5)%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant de l'Ain",
    subtitle = "Code Geo =01390, score en moyenne "
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 366 km, Durée moyenne de trajet= 214 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")

#normalisation logarithmique pour la distance avec moyenne normale

distance%>%
  filter(code_geo=="01390", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-((log(distance_km)-min(log(distance_km)))/(max(log(distance_km))-min(log(distance_km)))),
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(norm_vol+norm_diversite +norm_dist_km+norm_var+ norm_quality)/5)%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant de l'Ain",
    subtitle = "Code Geo =01390, score en moyenneavec la distance logarithmique"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 87,9km, Durée moyenne de trajet= 64 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")


# moyenne pondérée plus cap à n patients= 250
distance%>%
  filter(code_geo=="01390", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(cap_volume=ifelse(volume>250, 250, volume), norm_vol=(cap_volume-min(cap_volume))/(max(cap_volume)-min(cap_volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(2*norm_vol+4*norm_diversite+ 1*norm_var+6*norm_dist_km + 3*norm_quality)/16
  )%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant de l'Ain",
    subtitle = "Code Geo =01390, score en moyenne pondéréé"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 157,4km, Durée moyenne de trajet= 102 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")


##### codegeo en Alliers #####


#Moyenne
distance%>%
  filter(code_geo=="03460", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate( norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(norm_vol+norm_diversite+ norm_var+norm_dist_km + norm_quality)/5)%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant de l'Alliers",
    subtitle = "Code Geo =03460, score en moyenne"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 311 km, Durée moyenne de trajet= 201 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")


#cap volume et moyenne
distance%>%
  filter(code_geo=="03460", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(cap_volume=ifelse(volume>250, 250, volume), norm_vol=(cap_volume-min(cap_volume))/(max(cap_volume)-min(cap_volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(norm_vol+norm_diversite+ norm_var+norm_dist_km +norm_quality)/5)%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant de l'Alliers",
    subtitle = "Code Geo =03460, score en moyenne avec plafond sur le volume"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 346 km, Durée moyenne de trajet= 215 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")

#Moyenne pondérée avec Cap sur le volume
distance%>%
  filter(code_geo=="03460", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(cap_volume=ifelse(volume>250, 250, volume), norm_vol=(cap_volume-min(cap_volume))/(max(cap_volume)-min(cap_volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(2*norm_vol+4*norm_diversite+ 1*norm_var+6*norm_dist_km + 3*norm_quality)/16
  )%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant de l'Alliers",
    subtitle = "Code Geo =03460, score en moyenne pondéréé"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 301km, Durée moyenne de trajet= 196 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")


##### codegeo en Ardennes #####


#moyenne  avec cap volume
distance%>%
  filter(code_geo=="08090", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(cap_volume=ifelse(volume>250, 250, volume), norm_vol=(cap_volume-min(cap_volume))/(max(cap_volume)-min(cap_volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(norm_vol+norm_diversite+ norm_var+norm_dist_km + norm_quality)/5)%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant des Ardennes",
    subtitle = "Code Geo =08090, score en moyenne"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 244km, Durée moyenne de trajet= 163 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")



#moyenne pondérée avec cap volume
distance%>%
  filter(code_geo=="08090", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(cap_volume=ifelse(volume>250, 250, volume), norm_vol=(cap_volume-min(cap_volume))/(max(cap_volume)-min(cap_volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(2*norm_vol+4*norm_diversite+ 1*norm_var+6*norm_dist_km + 3*norm_quality)/16
  )%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant des Ardennes",
    subtitle = "Code Geo =08090, score en moyenne pondéréé"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 214km, Durée moyenne de trajet= 151 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")




# codegeo en cotes d'or

distance%>%
  filter(code_geo=="21360", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(cap_volume=ifelse(volume>250, 250, volume), norm_vol=(cap_volume-min(cap_volume))/(max(cap_volume)-min(cap_volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(2*norm_vol+4*norm_diversite+ 1*norm_var+6*norm_dist_km + 3*norm_quality)/16
  )%>%
  arrange(-score_final)%>%
  head(10)%>%
  #summarise(mean_dist=mean(distance_km), mean_duree=mean(duration_min))
  select(rs, categ_pmsi, distance_km, duration_min, sc_diversite, volume,variation,score_all_ajust, score_final, DIEP, "grand dorsal", "Gracillis/PAP/lambeau libre", lipomodelage, implant)%>%
  gt()%>%
  tab_header(
    title = "10 premières propositions pour un habitant des Cotes d'or",
    subtitle = "Code Geo =21360, score en moyenne pondéréé"
  ) %>%
  tab_spanner(
    label = md("**Information Etablissement**"),
    columns = c("rs","categ_pmsi")
  ) %>%
  tab_spanner(
    label = md("**Indicateur**"),
    columns = c("distance_km", "duration_min", "sc_diversite", "volume", "variation", "score_all_ajust")
  )%>%
  tab_spanner(
    label = md("**Score**"),
    columns = c("score_final")
  )%>%
  tab_spanner(
    label = md("**Actes effectués**"),
    columns = c("DIEP", "Gracillis/PAP/lambeau libre", "grand dorsal", "lipomodelage", "implant"))%>%
  cols_label(rs="Etablissement",
             categ_pmsi= "Type etablissement", 
             distance_km = "Distance en km", 
             duration_min = "Durée de trajet en min", 
             volume="Volume", variation="Taux de variation d'activité", score_all_ajust="Score de qualité global de l'établissement",
             score_final="Score", 
             DIEP="DIEP", "Gracillis/PAP/lambeau libre" = "Gracillis/PAP", 
             "grand dorsal"="Dorsal", lipomodelage="Lipomodelage", 
             implant="Implant", 
             sc_diversite="Score de diversite")%>%
  tab_source_note(
    source_note = "Distance moyenne= 214km, Durée moyenne de trajet= 151 min"
  ) %>%
  tab_source_note(
    source_note = "Source: Données PMSI 2019")





#####  Répartition géographique #####

library(broom)
library(sf)
library(osmdata)
library(geojsonio)
library(broom)
library(ggsci)
library(mapproj)
library(dplyr)


##### 3.1 layers #####
# les constantes
REGIONS_DOM_TOM = c("01", "02", "03", "04", "06", "TOM")
DEPARTEMENTS_DOM_TOM = c("971", "972", "973", "974", "976")
DEPS_IDF <- c("75", "91", "92", "93", "94", "95", "77", "78")
DEPS_PARIS<-c("75","92","93", "94")
CODE_GEO_INTERET<-c("21360","08090", "03460", "01390")
  
  # Load data
  spdf_departements <- geojson_read(
    file.path("/Users/zoefontier/Desktop/04.93/01.Data/departements-francais-2015.geojson"),
    what = "sp"
  )

spdf_regions <- geojson_read(
  file.path("/Users/zoefontier/Desktop/04.93/01.Data/regions.geojson"),
  what = "sp"
)

spdf_codegeo <- geojson_read(
  file.path("/Users/zoefontier/Desktop/04.93/01.Data/codes_geo.geojson"),
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

code_geo_layer<-tidy(
  spdf_codegeo[ spdf_codegeo@data$code %in% CODE_GEO_INTERET, ],
  region="code"
)


# Ain
#Moyenne normale

distance%>%
  filter(code_geo=="01390", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(2*norm_vol+4*norm_diversite+ 1*norm_var+5*norm_dist_km + 3*norm_quality)/15
  )%>%
  arrange(-score_final)%>%
  head(10)%>%
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
  geom_polygon(data=code_geo_layer[which(code_geo_layer$id=="01390"),],
               aes(x=long,
                   y=lat,
                   group=group),
               color="red",
               alpha=0,
               fill="red",
               size=0.5) +
  geom_point(aes(x=longitude,y=latitude, color=categ_pmsi),
             shape=16,alpha=2, size=4) +
  theme(legend.position = "none")+
  scale_size(name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Méthode de cap et de moyenne pondérée", caption = "Données PMSI 2019")+
  theme(legend.title = element_text( size = 14),
        legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title() 


#Moyenne pondérée
m2<-distance%>%
  filter(code_geo=="03460", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(diversite_score-min(diversite_score))/(max(diversite_score)-min(diversite_score)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         score_final=(0.3*norm_vol+0.1*norm_diversite +0.6*norm_dist_km)) %>%
  arrange(-score_final)%>%
  head(10)%>%
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
  geom_polygon(data=code_geo_layer[which(code_geo_layer$id=="01390"),],
               aes(x=long,
                   y=lat,
                   group=group),
               color="red",
               alpha=0,
               fill="red",
               size=0.5) +
  geom_point(aes(x=longitude,y=latitude, color=categ_pmsi),
                shape=16,alpha=2, size=4) +
  scale_size(name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  theme(legend.position = "none")+
  labs(title=" Méthode 2", caption = "Données PMSI 2019")+
  theme(legend.title = element_text( size = 14),
         legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title() 


#distance log

m3<-distance%>%
  filter(code_geo=="01390", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(diversite_score-min(diversite_score))/(max(diversite_score)-min(diversite_score)), 
         norm_dist_km=1-((log(distance_km)-min(log(distance_km)))/(max(log(distance_km))-min(log(distance_km)))), 
         score_final=(norm_vol+norm_diversite +norm_dist_km)/3) %>%
  arrange(-score_final)%>%
  head(10)%>%
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
  geom_polygon(data=code_geo_layer[which(code_geo_layer$id=="01390"),],
               aes(x=long,
                   y=lat,
                   group=group),
               color="red",
               alpha=0,
               fill="red",
               size=0.5) +
  geom_point(aes(x=longitude,y=latitude, color=categ_pmsi),
             shape=16,alpha=2, size=4) +
  theme(legend.position = "none")+
  scale_size(name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title=" Méthode 3", caption = "Données PMSI 2019")+
  theme(legend.title = element_text( size = 14),
        legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title() 


figure<-ggarrange(m1, m2, m3, ncol=3, nrow=1)
annotate_figure(figure,
                top = text_grob("Propositions de centres pour une communes de l'Ain",  face = "bold", size = 20),
                bottom = text_grob("Data source: Données PMSI 2019",
                                   hjust = 1, x = 1, face = "italic", size = 10),
)


# Alliers
distance%>%
  filter(code_geo=="03460", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(diversite_score-min(diversite_score))/(max(diversite_score)-min(diversite_score)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         score_final=(0.3*norm_vol+0.1*norm_diversite +0.6*norm_dist_km)) %>%
  arrange(-score_final)%>%
  head(10)%>%
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
  geom_polygon(data=code_geo_layer[which(code_geo_layer$id=="03460"),],
               aes(x=long,
                   y=lat,
                   group=group),
               color="red",
               alpha=0,
               fill="red",
               size=0.5) +
  geom_point(aes(x=longitude,y=latitude, color=categ_pmsi),
             shape=16,alpha=2, size=6) +
  scale_size(name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Proposition d'établissement pour un code géo de l'Alliers", subtitle = "Données PMSI 2019")


# Ardennes


#Moyenne normale

m1<-distance%>%
  filter(code_geo=="08090", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(cap_volume=ifelse(volume>250, 250, volume), norm_vol=(cap_volume-min(cap_volume))/(max(cap_volume)-min(cap_volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(norm_vol+norm_diversite+ norm_var+norm_dist_km + norm_quality)/5)%>%
  arrange(-score_final)%>%
  head(10)%>%
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
  geom_polygon(data=code_geo_layer[which(code_geo_layer$id=="08090"),],
               aes(x=long,
                   y=lat,
                   group=group),
               color="red",
               alpha=0,
               fill="red",
               size=0.5) +
  geom_point(aes(x=longitude,y=latitude, color=categ_pmsi),
             shape=16,alpha=2, size=4) +
  theme(legend.position = "none")+
  scale_size(name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title="Méthode 1", caption = "Données PMSI 2019")+
  theme(legend.title = element_text( size = 14),
        legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title() 

m1
#Moyenne pondérée
m2<-distance%>%
  filter(code_geo=="08090", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(cap_volume=ifelse(volume>250, 250, volume), norm_vol=(cap_volume-min(cap_volume))/(max(cap_volume)-min(cap_volume)), norm_diversite=(sc_diversite-min(sc_diversite))/(max(sc_diversite)-min(sc_diversite)), 
         norm_dist_km=1-(distance_km-min(distance_km))/(max(distance_km)-min(distance_km)), 
         norm_var=(variation-min(variation))/(max(variation)-min(variation)),
         norm_quality=(score_all_ajust-min(score_all_ajust, na.rm=T))/(max(score_all_ajust, na.rm=T)-min(score_all_ajust, na.rm=T)),
         score_final=(2*norm_vol+4*norm_diversite+ 1*norm_var+6*norm_dist_km + 3*norm_quality)/16
  )%>%
  arrange(-score_final)%>%
  head(10)%>%
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
  geom_polygon(data=code_geo_layer[which(code_geo_layer$id=="08090"),],
               aes(x=long,
                   y=lat,
                   group=group),
               color="red",
               alpha=0,
               fill="red",
               size=0.5) +
  geom_point(aes(x=longitude,y=latitude, color=categ_pmsi),
             shape=16,alpha=2, size=4) +
  scale_size(name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  theme(legend.position = "none")+
  labs(title=" Méthode 2", caption = "Données PMSI 2019")+
  theme(legend.title = element_text( size = 14),
        legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title() 

m2
#distance log

m3<-distance%>%
  filter(code_geo=="08090", .keep_all=TRUE)%>%
  inner_join(bdd_finale %>% filter(substr(finessGeoDP,1,2) != "97"), by=c("finess_geo"="finessGeoDP"))%>%
  mutate(norm_vol=(volume-min(volume))/(max(volume)-min(volume)), norm_diversite=(diversite_score-min(diversite_score))/(max(diversite_score)-min(diversite_score)), 
         norm_dist_km=1-((log(distance_km)-min(log(distance_km)))/(max(log(distance_km))-min(log(distance_km)))), 
         score_final=(norm_vol+norm_diversite +norm_dist_km)/3) %>%
  arrange(-score_final)%>%
  head(10)%>%
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
  geom_polygon(data=code_geo_layer[which(code_geo_layer$id=="08090"),],
               aes(x=long,
                   y=lat,
                   group=group),
               color="red",
               alpha=0,
               fill="red",
               size=0.5) +
  geom_point(aes(x=longitude,y=latitude, color=categ_pmsi),
             shape=16,alpha=2, size=4) +
  theme(legend.position = "none")+
  scale_size(name="Nombre total de patientes")+
  scale_color_manual(values=COLOR_PMSI, name="Categorie PMSI") +
  labs(title=" Méthode 3", caption = "Données PMSI 2019")+
  theme(legend.title = element_text( size = 14),
        legend.text = element_text(size=12),text = element_text(size=16))+
  ggeasy::easy_center_title() 


figure<-ggarrange(m1, m2, m3, ncol=3, nrow=1)
annotate_figure(figure,
                top = text_grob("Propositions de centres pour une communes des Ardennes",  face = "bold", size = 20),
                bottom = text_grob("Data source: Données PMSI 2019",
                                   hjust = 1, x = 1, face = "italic", size = 10),
)



distance<-read.csv("/Users/zoefontier/Desktop/01.Chirurgie reconstructive/01.data/distance.csv", sep=",")
