
#############################################################################################################################################################################################################################
####################################################################### Aide Gestion Packages ###############################################################################################################################
#############################################################################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Presentation des packages utilisés 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #renv est un package/librairie permettant la sauvegarde des librairies dans le dossier  de travail, en figeant leur version
  #tidyverse contient les packages: (readxl) pour importer les fichiers xls; (ggplot2) pour afficher les graphs
  #(chron) est un package de gestion du temps,(patchwork) gere les groupes de graphs, (esquisse) est une interface pour graphs
  
  #package "openxlsx" permet de produire des classeurs excel, reconnait les ggplots
  #package "readxl" permettant de lire des fichiers excel
  #package "kableExtra" gestion des tables pour rmarkdown (appel dans le fichier rmd )
  #package package omportant de multiples librairies (Tidyr,pipe,dplyr,ggplot)
  #package "chron" de gestion du temps
  #package "renv" sauvegarde et fige les librairies,
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Installation des packages
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #mes_packages=c("chron","esquisse","renv","openxlsx","readxl","tidyverse","rmarkdown","tinytex","kableExtra")
  #install.packages(mes_packages)
  #remove.packages(mes_packages)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Chargement des packages pour utilisation
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readxl)
library(openxlsx)
library(tidyverse)
library(esquisse)
library(chron)
library(tinytex)
library(knitr)
library(rmarkdown)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Installation de la distribution latex TinyTex pour production de PDF a partir du package "tinytex"-apres installation et chargement des packages
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #tinytex::install_tinytex()       # Installation de la distribution TinyTex a partir du package tinytex (apres l'avoir chargé) [a faire une fois]
  #tinytex::uninstall_tinytex()     # Desinstalle la distribution TinyTex
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Aide memoire pour gestion des packages
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #library()              # permet de lister tous les packages installés sur l'ordinateur [ fonction similaire a require() si on y renseigne le nom d'un package]
  #(.packages())          # permet de lister tous les packages chargés
  

  #Decharge de tous les packages chargés par l'utilisateur, laissant seulement celles par defaut
  # lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
  # invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE))
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Gestion de renv: package chargé de sauvegarder l'etat des versions des librairies pour eviter les inconvenients des MAJ de packages 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Desinstallation de renv
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # renv::deactivate()
  # root = renv::paths$root()
  # unlink(root, recursive = TRUE) 
  # utils::remove.packages("renv") #desinstallation du package renv de l'ordinateur
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #initialisation de renv - chez l'auteur
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #renv::init #initialisation, creation du dossier renv et du fichier renv
    #renv::snapshot()#Sauvegarde les librairies chargées dans le projet 
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #initialisation de renv - chez l'utilisateur
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #renv::restore() #charge la derniere sauvegarde des packages




