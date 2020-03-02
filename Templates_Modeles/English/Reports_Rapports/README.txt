Dû au design des fichiers Rmd qui importent directement des variables de l'environnement global du projet, on ne peux pas simplement faire rouler les fichiers en mode Knit to HTML (Preview devrait fonctionner, cependant) et espérer de garder les variables storées dans l'environnement global. Donc, j'ai créé ce fichier README pour s'assurer qu'une bonne démarche soit suivie.


1. Ouvrir le ficher CABIN_vv_import.RMD
2. Effacer toutes les variables, fonctions, etc. de l'environnement. (rm(list=ls()) dans la console accomplira la tâche.)
3. Run all chunks du fichier CABIN_vv_import.RMD. (Le raccourci Ctrl+Alt+R accomplira la tâche)
4. Suivre les prompts du fichier CABIN_vv_import.RMD.
5. Clicker sur Knit to HTML.
6. Ouvrir le fichier HTML_Render.Rmd.
7. Run all chunks du fichier CABIN_vv_import.RMD. (Le raccourci Ctrl+Alt+R accomplira la tâche)
8. Allez-vous chercher un café, ça va prendre quelques minutes.

Veuillez-noter que Run All Chunks dans les fichiers CABIN_vv_general, CABIN_vv_habitat, CABIN_vv_spatial et CABIN_vv_biology avant d'utiliser HTML_Render.Rmd pourrait causer des erreurs dans la création des fichiers HTML. Je recommende fortement d'effacer toutes les variables, fonctions, etc. de l'environnement (étape 2 ci-dessus) avant de créér des fichiers HTML tel qu'indiqué ci-dessus. (En gros, veuiller ne pas sauter des étapes ou en rajouter au milieu de la démarche inscrite)