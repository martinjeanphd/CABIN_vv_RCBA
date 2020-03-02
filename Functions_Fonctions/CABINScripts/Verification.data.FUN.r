# Verification.data.FUN.R

###################################################################
# FUNCTIONS to perform basic verification on data
#
# FONCTIONS pour réaliser des vérifications de base sur les données
#
# Author / Auteur: Martin Jean
# Version 1.01, 2017-01-10
###################################################################


#==================================================================
# FUNCTION to verifiy if two dataframes have the same objects (visits)
#
# FONCTION pour vérifier si deux dataframes ont les mêmes objets (visites)
#==================================================================

compareRows <- function(a1, a2, name1, name2) {
    a1.vec <- row.names(a1)
    a2.vec <- row.names(a2)
    a1.not.in.a2.rows <- a1[!a1.vec %in% a2.vec,]
    a2.not.in.a1.rows <- a2[!a2.vec %in% a1.vec,]
    nb.a1.not.in.a2.rows <- nrow(a1.not.in.a2.rows)
    nb.a2.not.in.a1.rows <- nrow(a2.not.in.a1.rows)


    # add message to the console if the two dataframes do not correspond
    #
    # ajoute un messge à la console si les deux dataframes ne correspondent pas

    if (nb.a1.not.in.a2.rows > 0) {
        if (Language == "Francais") {
            cat(paste("ATTENTION: ", name1, " (", nrow(a1), " lignes) ne correspond pas à ", name2, " (", nrow(a2), " lignes)", sep=""), "\n")
            cat("Corrigez la situation avant de poursuire l'analyse", "\n")
            cat("Les lignes suivantes sont présentes dans", name1, "mais absentes dans", name2, ":", "\n")
        } else {
            cat(paste("ATTENTION: ", name1, " (", nrow(a1), " lines) does not correspond to ", name2, " (", nrow(a2), " lines)", sep=""), "\n")
            cat("Correct the problem before going further into the analysis", "\n")
            cat("The following lines are present in", name1, "but missing in", name2, ":", "\n")
        }
        cat("SampleID       Site         Date       SampleNumber", "\n")
        cat("---------------------------------------------------", "\n")
        diff <- rownames(a1[!rownames(a1) %in% rownames(a2), ])
        for(i in diff){
            cat(i,  "        ", dataset.NAM[i, ]$Site, "     ", as.character(dataset.NAM[i, ]$SampleDate),  "    ", dataset.NAM[i, ]$SampleNumber, "\n")
        }
        cat("\n")
    } else {
        if (Language == "Francais") {
            cat("Les lignes de", name1, "et de", name2, "correspondent parfaitement", "\n")
        } else {
            cat("The lines of", name1, "and of", name2, "are identical.", "\n")
        }
    }



    if (nb.a2.not.in.a1.rows > 0) {
        if (Language == "Francais") {
            cat(paste("ATTENTION: ", name2, " (", nrow(a2), " lignes) ne correspond pas à ", name1, " (", nrow(a1), " lignes)", sep=""), "\n")
            cat("Corrigez la situation avant de poursuire l'analyse", "\n")
            cat("Les lignes suivantes sont présentes dans", name2, "mais absentes dans", name1, ":", "\n")
        } else {
            cat(paste("ATTENTION: ", name2, " (", nrow(a2), " lines) does not correspond to ", name1, " (", nrow(a1), " lines)", sep=""), "\n")
            cat("Correct the problem before going further into the analysis", "\n")
            cat("The following lines are present in", name2, "but missing in", name1, ":", "\n")
        }
        cat("SampleID       Site         Date       SampleNumber", "\n")
        cat("---------------------------------------------------", "\n")
        diff <- rownames(a2[!rownames(a2) %in% rownames(a1), ])
        for(i in diff){
            cat(i,  "        ", dataset.NAM[i, ]$Site, "     ", as.character(dataset.NAM[i, ]$SampleDate),  "    ", dataset.NAM[i, ]$SampleNumber, "\n")
        }
        cat("\n")
        cat("\n")
    }

} #end function / fin de la fonction


#===============================================================================
# FUNCTION to show some basic characteristics of a dataset
#
#
# FONCTION pour afficher quelques caractéristiques de base sur un jeu de données
#
#===============================================================================

doFirstContact <- function(ds, desc) {
    cat("==============================", "\n", "\n")
    if (Language == "Francais"){
        cat("Aperçu de", desc, "\n")
    } else {
        cat("First look at", desc, "\n")
    }

    cat("==============================", "\n")

    if (Language == "Francais"){
        cat("Le fichier a", nrow(ds), "objet(s) (ou ligne(s))", "et", ncol(ds), "descripteur(s) (ou colonne(s))", "\n")
    } else {
        cat("The file has ", nrow(ds), "object(s) (or line(s))", "and", ncol(ds), "descriptor(s) (ou column(s))", "\n")
    }

    cat("\n", "------------------------------", "\n")

    x <- colnames(ds)

    if (Language == "Francais"){
        cat("Nom des descriteurs :", "\n")		# Étiquette des colonnes (descripteurs)
    } else {
        cat("Descriptors' Name:", "\n")		# Column labels (descriptors)
    }

    for(i in x){
        cat("- ", i, "\n")
    }

    cat("\n", "------------------------------", "\n")

    x <- rownames(ds)

    if (Language == "Francais"){
        cat("\n", "Nom des objets :", "\n")		# Étiquette des lignes (sites)
    } else {
        cat("\n", "Objects' Name:", "\n")		# Row labels (sites)
    }

    for(i in x){
        cat("- ", i, "\n")
    }

    cat("\n")

    cat("------------------------------", "\n")


    # create a summary for numeric and integer descriptors
    #
    # créer un sommaire pour les descripteurs numériques et entiers
    ds_num <- ds[sapply(ds, is.numeric)]
    for(j in 1: ncol(ds_num)){
        if (Language == "Francais"){
            cat("Sommaire du descripteur quantitatif", colnames(ds_num[j]), "\n")
        } else {
            cat("Summary of Quantitative Descriptor", colnames(ds_num[j]), "\n")
        }
        print(summary(ds_num[j], "\n"))

        cat("\n", "\n")
    }
    cat("------------------------------", "\n", "\n", "\n")

    # Create a summary for qualitative descriptors
    #
    # Créer un sommaire pour les descripteurs qualitatifs
    ds_qual <- ds[, !names(ds) %in% names(ds_num)]
    for (k in 1: ncol(ds_qual)){
        if (Language == "Francais"){
            cat("Sommaire du descripteur qualitatif", colnames(ds_qual[k]), "\n")
            cat("Nombre de valeurs manquantes :", sum(is.na(ds_qual[k])), "\n")
        } else {
            cat("Summary of Qualitative Descriptor", colnames(ds_qual[k]), "\n")
        }
        ab <- table(unlist(ds_qual[k]))
        print(ab)
        cat("\n", "\n")

    }
    cat("==============================", "\n", "\n", "\n")

} #end function / fin de la fonction



#===============================================================================
# FONCTION to create a frequency table for a descriptor (column)
#
#
# FONCTION pour créer un tableau de fréquences d'un descripteur (colonne)
#
#===============================================================================
examineString <- function(ds, desc) {
    cat("==============================", "\n", "\n")
    if (Language == "Francais"){
        cat("Tableau de fréquence du descripteur (colonne)", desc, "\n")
    } else {
        cat("Frequency table for descriptor (column)", desc, "\n")
    }
    table(dataset.GEN[,desc])
    cat("==============================", "\n","\n")

}

#===============================================================================
# FONCTION for printing diacriticals on Windows
#           This solves problems using the 'cat' command on
#           This function includes a carriage return
#
# FONCTION pour afficher correctement les accents sous Windows
#           Cette fonction inclut un saut de ligne
#===============================================================================
catDiacritical <- function (theString) {
    a <- paste(theString, "\n", sep="")
    if (Platform == "Windows") {
        ab <- iconv(a, from="UTF-8", to="latin1")
    } else {
        ab <- a
    }
    return(ab)
}
