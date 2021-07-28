# Ma reconstruction mammaire

## Les bases de données importantes
#### Bases initiales
- RECONSTRUCTIONS_1519.csv : données issues du PMSI des actes de reconstruction entre 2015 et 2019 en France
- distance.csv: bases de données distances établissements/code_communes récupérées par Capgemini
- laposte_hexasmal.csv :base de conversion code_postal/ code_commune
- resultats-esatisca-mco-open-data-2019.csv: indice de qualité des soins en open data depuis la HAS

#### Bases intermédiaires
Pour les variables concernant les établissements
- reconstruction_v0.csv : base processed de l'activité de reconstruction des établissements avec RECONSTRUCTION_1519.csv (regroupements des actes pour l'analyse descriptive)
- bdd_finale: base avec les différentes variables d'intérêt pour chaque établissement pas encore preprocessed, issue de reconstruction_v0.csv (ajout variation volume et score de diversité )

Pour les distances
- code_communes_manquant.txt : liste des communes qui ne sont pas dans la base distance.csv de Capgemini
- pairs_400.csv : paires établissements/communes manquantes dans la base distance.csv
- dossier distance_recup: contient les différentes combinaisons établissements/communes qui manquaient dans la base distance.csv
- distance_recup_v0.csv : merged de toutes les bases récupérées (à remplacer en retournant le code)

#### bases finales 
Ces deux bases sont celles qui doivent être mises sur le drive pour que Théo ou Johan puissent les utiliser directement pour l'application:
-  variable_complete.csv : issues de la base bdd_finale base qui contient  les variables d'intérêt pour l'algorithme processed et normée pour chaque établissement de reconstruction (renommage des variables pour être directement lues dans l'application)
- distance_completed.csv: base avec toutes les distances qui ont été récupérées en plus avec le code ors.py, pour les établissements faisant de la reconstruction mammaire uniquement


# Descriptif du code
Le code de l'algorithme de "Ma reconstruction mammaire" se divise en plusieurs parties:

### Description et processing des données
- "Code_reconstruction.R" dresse un état des lieux de la reconstruction mammaire en France, avec des statistiques descriptives, des cartes et des spider web pour les établissements effectuant de la reconstruction mammaire. Ce code permet d'avoir un aperçu de la répartition de l'activité de reconstruction mammaire en France. Dans ce code on a un début de preprocessing des datas (notamment le regroupement des actes de chirurgies). Ce code peut ne pas être retourné si la base "reconstruction_V0" est déjà chargée.

- Avec le code "algo.R" et la base "reconstruction_V0", on construit la base finale "bdd_finale"  qui sera une des bases utilisées dans la recommandation. Dans ce code vous trouverez la construction des 4 variables utilisées dans l'algorithme. Plus des tests pour les différentes formules de score, mais cette partie peut être ignorée.

-Utils.R : contients les labels et les codes couleurs pour les graphes

### Récupération des distances

-ors.py : ce code est celui d'Eric qui permet en tournant sur le serveur de Curie de récupérer les combinaisons établissements/communes manquantes grâce à la bdd pairs_400.csv
- récupération des villes manquantes.ipynb: permet de merger les différentes bases de données récupérées avec ors.py. Ce code peut être zappé si le code ors a été tourné en une seule fois et que toutes les combinaisons sont récupérées et uniques.

### Algorithme, processing des base finale à donner à Théo et test
- L'algorithme de recommandation est codé et testé dans le code python "algorithme de recommandation des centres de soins". On y utilise les bases "bdd_finale" et la base "distance". On les transforme pour avoir les deux bases définitives qui sont mises sur le drive "Ma reconstruction Mammaire". Les différentes étapes de la mise au point de l'algorithme sont décrites, et une partie du code permet de tester les recommandations sur des villes. Il y a aussi une petite fonction pour tester le rang moyen des établissements dans les recommandations pour un département donné. 

Le code n'est pas directement relié à l'application. Théo Mouchabac a implémenté l'algo (simplement le merge des tables et le calcul du score agrégé) sur SQL. Les données ont toutes été processées avant dans le code python (normalisation surtout et selection des variables d'intérêt)

### Autres
app_reco.py : code brouillon
