D� au design des fichiers Rmd qui importent directement des variables de l'environnement global du projet, on ne peux pas simplement faire rouler les fichiers en mode Knit to HTML (Preview devrait fonctionner, cependant) et esp�rer de garder les variables stor�es dans l'environnement global. Donc, j'ai cr�� ce fichier README pour s'assurer qu'une bonne d�marche soit suivie.


1. Ouvrir le ficher CABIN_vv_import.RMD
2. Effacer toutes les variables, fonctions, etc. de l'environnement. (rm(list=ls()) dans la console accomplira la t�che.)
3. Run all chunks du fichier CABIN_vv_import.RMD. (Le raccourci Ctrl+Alt+R accomplira la t�che)
4. Suivre les prompts du fichier CABIN_vv_import.RMD.
5. Clicker sur Knit to HTML.
6. Ouvrir le fichier HTML_Render.Rmd.
7. Run all chunks du fichier CABIN_vv_import.RMD. (Le raccourci Ctrl+Alt+R accomplira la t�che)
8. Allez-vous chercher un caf�, �a va prendre quelques minutes.

Veuillez-noter que Run All Chunks dans les fichiers CABIN_vv_general, CABIN_vv_habitat, CABIN_vv_spatial et CABIN_vv_biology avant d'utiliser HTML_Render.Rmd pourrait causer des erreurs dans la cr�ation des fichiers HTML. Je recommende fortement d'effacer toutes les variables, fonctions, etc. de l'environnement (�tape 2 ci-dessus) avant de cr��r des fichiers HTML tel qu'indiqu� ci-dessus. (En gros, veuiller ne pas sauter des �tapes ou en rajouter au milieu de la d�marche inscrite)