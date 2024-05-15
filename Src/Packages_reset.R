
#####################################################################################################################################################################################################################
####################################################################### Packages reset ##############################################################################################################################
#####################################################################################################################################################################################################################
#Packages et TinyTex reset

#avant  
library()       #liste les packages installés
(.packages())   #liste les packages chargés
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Desinstallation de la distribution TinyTex de l'ordinateur-Uninstall TinyTex distribution from computer
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
tinytex::uninstall_tinytex()

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#déchargement des packages chargés-Unloading of all user loaded packages
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Decharge de tous les packages chargés par l'utilisateur, laissant seulement celles par defaut
lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Desinstallation de tous les package installés - uninstall all user installed packages
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a list of all installed packages
ip <- as.data.frame(installed.packages())
head(ip)
# if you use MRO, make sure that no packages in this library will be removed
ip <- subset(ip, !grepl("MRO", ip$LibPath))
# we don't want to remove base or recommended packages either\
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# determine the library where the packages are installed
path.lib <- unique(ip$LibPath)
# create a vector with all the names of the packages you want to remove
pkgs.to.remove <- ip[,1]
head(pkgs.to.remove)
# remove the packages
sapply(pkgs.to.remove, remove.packages, lib = path.lib)

#apres
library()       #liste les packages installés
(.packages())   #liste les packages chargés

