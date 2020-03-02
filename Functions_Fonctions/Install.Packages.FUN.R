#Install.Packages.FUN

Install.Packages.FUN <- function {

  #===============================================================================
  # Install the required packages
  #
  #  Packages must have been installed on your current version of R previously
  #  if package unavailable, this function will install them
  #
  # Installation des extensions requis
  #
  #  Les extensions doivent avoir été préalablement installées dans votre
  #  version de R.
  #  Si une extension n'est pas disponible, celle fonction les installera
  #===============================================================================

# vegan: community ecology package
if (!require(vegan)) {
    if (Language == "Francais") {
        cat("Installation de l'extension Vegan", "\n")
    } else {
        cat("Installing the Vegan package", "\n")
    }
    install.packages("vegan")
    cat("\n"); cat("----------------------------------------------","\n")
}


#survival: survival analysis
if (!require(survival)) {
    if (Language == "Francais") {
        cat("Installation de l'extension Survival", "\n")
    } else {
        cat("Installing the Survival package", "\n")
    }
    install.packages("survival")
    #REQUIRED FOR NADA, will not load automatically when NADA is loaded from .gz
    cat("\n"); cat("----------------------------------------------","\n")
}


# NADA: Nondetects and Data Analysis for Environmental Data
if(!require(NADA)) {
    if (Language == "Francais") {
        cat("Installation de l'extension NADA", "\n")
    } else {
        cat("Installing the NADA package", "\n")
    }
    install.packages("NADA")
    #removed from CRAN repository (no longer works)
    #install.packages(paste(functiondir, "\\packages\\NADA_1.5-4.tar.gz", sep=""), repos=NULL, type="source" )
    #NOTE: The following object(s) are masked from ‘package:stats’: cor
    cat("\n"); cat("----------------------------------------------","\n")
}


# plyr: Tools for splitting, applying and combining data
if (!require(plyr)) {
    if (Language == "Francais") {
        cat("Installation de l'extension plyr", "\n")
    } else {
        cat("Installing the plyr package", "\n")
    }
    install.packages("plyr")
    cat("\n"); cat("----------------------------------------------","\n")
}


# lubridate: Make dealing with dates a little easier
if (!require(lubridate)) {
    if (Language == "Francais") {
        cat("Installation de l'extension lubridate", "\n")
    } else {
        cat("Installing the lubridate package", "\n")
    }
    install.packages("lubridate")
    cat("\n"); cat("----------------------------------------------","\n")
}




# ggplot2: Create elegant data visualisatkons using the grammar of graphics
if (!require(ggplot2)) {
    if (Language == "Francais") {
        cat("Installation de l'extension ggplot2", "\n")
    } else {
        cat("Installing the ggplot2 package", "\n")
    }
    install.packages("ggplot2")
    cat("\n"); cat("----------------------------------------------","\n")
}


# scales: scale functions for visualisation
if (!require(scales)) {
    if (Language == "Francais") {
        cat("Installation de l'extension scales", "\n")
    } else {
        cat("Installing the scales package", "\n")
    }
    #scales goes with ggplot and adds the needed scale* functions
    install.packages("scales")
    cat("\n"); cat("----------------------------------------------","\n")
}


# Kendall: Kendall rank correlation and Mann-Dendall trend test
if (!require(Kendall)) {
    if (Language == "Francais") {
        cat("Installation de l'extension Kendall", "\n")
    } else {
        cat("Installing the Kendall package", "\n")
    }
    install.packages("Kendall")
    cat("\n"); cat("----------------------------------------------","\n")
}


# Reshape2: Flexibly reshape data
if (!require(reshape2)) {
    if (Language == "Francais") {
        cat("Installation de l'extension reshape2", "\n")
    } else {
        cat("Installing the reshape2 package", "\n")
    }
    install.packages("reshape2")
    cat("\n"); cat("----------------------------------------------","\n")
}

# GridExtra: Miscellaneous functions for "grid" graphics
if (!require(gridExtra)) {
    if (Language == "Francais") {
        cat("Installation de l'extension gridExtra", "\n")
    } else {
        cat("Installing the gridExtra package", "\n")
    }
    install.packages("gridExtra")
    cat("\n"); cat("----------------------------------------------","\n")
}


# Pracma: Practical numerical math functions
if (!require(pracma)) {
    if (Language == "Francais") {
        cat("Installation de l'extension pracma", "\n")
    } else {
        cat("Installing the pracma package", "\n")
    }
    install.packages("pracma")
    cat("\n"); cat("----------------------------------------------","\n")
}


# Tcltk: Tcl/Tk interface
#if (Language == "Francais") {
#   cat("Installation de l'extension tcltk", "\n")
#} else {
#   cat("Installing the tcltk package", "\n")
#if (!require(tcltk)) {install.packages("tcltk")}
#cat("\n"); cat("----------------------------------------------","\n")


#Gplots: Various R programming tools for plotting data
if (!require(gplots)) {
    if (Language == "Francais") {
        cat("Installation de l'extension gplots", "\n")
    } else {
        cat("Installing the gplots package", "\n")
    }
    install.packages("gplots")
    cat("\n"); cat("----------------------------------------------","\n")
}


# TSA: Time series analysis
if (!require(TSA)) {
    if (Language == "Francais") {
        cat("Installation de l'extension TSA", "\n")
    } else {
        cat("Installing the TSA package", "\n")
    }
    install.packages("TSA")
    cat("\n"); cat("----------------------------------------------","\n")
}


#ade4: Analysis of ecological data: exploratory and euclidean methods in environmental sciences
if (!require(ade4)) {
    if (Language == "Francais") {
        cat("Installation de l'extension ade4", "\n")
    } else {
        cat("Installing the ade4 package", "\n")
    }
    install.packages("ade4")
    cat("\n"); cat("----------------------------------------------","\n")
}


# Rgdal: Bindings for the Geospatial data abstraction library
if (!require(rgdal)) {
    if (Language == "Francais") {
        cat("Installation de l'extension rgdal", "\n")
    } else {
        cat("Installing the rgdal package", "\n")
    }
    install.packages("rgdal")
    cat("\n"); cat("----------------------------------------------","\n")
}


# Maptools: Tools for reading and handling spatial objects
if (!require(maptools)) {
    if (Language == "Francais") {
        cat("Installation de l'extension maptools", "\n")
    } else {
        cat("Installing the maptools package", "\n")
    }
    install.packages("maptools")
    cat("\n"); cat("----------------------------------------------","\n")
}

#Maps: Draw geographical maps
if (!require(maps)) {
    if (Language == "Francais") {
        cat("Installation de l'extension maps", "\n")
    } else {
        cat("Installing the maps package", "\n")
    }
    install.packages("maps")
    cat("\n"); cat("----------------------------------------------","\n")
}

  
  #corrgram: Draw corrgrams
  if (!require(corrgram)) {
    if (Language == "Francais") {
      cat("Installation de l'extension corrgram", "\n")
    } else {
      cat("Installing the corrgram package", "\n")
    }
    install.packages("corrgram")
    cat("\n"); cat("----------------------------------------------","\n")
  }
### Note: Use sessionInfo()  to check what packages are installed
### Note: Utiliser sessionInfo() pour vérifier quelles extensions est installées


}#end Function
