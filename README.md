# Ma reconstruction mammaire

Le code de l'algorithme de "Ma reconstruction mammaire" se divise en 3 parties:

- "Code_reconstruction.R" dresse un état des lieux de la reconstruction mammaire en France, avec des statistiques descriptives, des cartes et des spider web pour les établissements effectuant de la reconstruction mammaire. Ce code permet d'avoir un aperçu de la répartition de l'activité de reconstruction mammaire en France. Dans ce code on a un début de preprocessing des datas (notamment le regroupement des actes de chirurgies). Ce code peut ne pas être retourné si la base "reconstruction_V0" est déjà chargée.

- Avec le code "algo.R" et la base "reconstruction_V0", on construit la base finale "bdd_finale"  qui sera une des bases utilisée dans la recommandation. Dans ce code vous trouverez la construction des 4 variables utilisées dans l'algorithme. Plus des tests pour les différentes formules de score, mais cette partie peut être ignorée.

- L'algorithme de recommandation est codé et testé dans le code python "algorithme de recommandation des centres de soins". On y utilise les bases "bdd_finale" et la base "distance". On les transforme pour avoir les deux bases définitives qui sont mises sur le drive "Ma reconstruction Mammaire". Les différentes étapes de la mise au point de l'algorithme sont décrites, et une partie du code permet de tester les recommandations sur des villes. Il y a aussi une petite fonction pour tester le rang moyen des établissements dans les recommandations pour un département donné. 

Le code n'est pas directement relié à l'application. Théo Mouchabac a implémenté l'algo (simplement le merge des tables et le calcul du score agrégé) sur SQL. Les données ont toutes été processées avant dans le code python (normalisation surtout et selection des variables d'intérêt)
