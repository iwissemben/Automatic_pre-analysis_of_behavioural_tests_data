
#####################################################################################################################################################################################################################
####################################################################### Analyser ####################################################################################################################################
#####################################################################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#initialisation: Packages requis pour nos besoins et code
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#installation des packages pour utilisation

# mes_packages=c("chron","esquisse","kableExtra","openxlsx","readxl","renv","rmarkdown","tidyverse","tinytex")
# install.packages(mes_packages)

#initialisation de renv-utilisateur-pour installer les librairies 
# renv::restore() #Installe les packages dans la version ou elles ont été sauvegardées par renv::snapshot() [a faire une fois]

# Chargement des packages pour utilisation
library(chron)
library(openxlsx)
library(readxl)
library(rmarkdown)
library(tidyverse)
library(tinytex)
library(knitr)
#library(esquisse)

if(tinytex::is_tinytex()==FALSE){
  print("Distribution TinyTex non trouvé: installation")
  tinytex::install_tinytex()       # Installation de la distribution TinyTex a partir du package tinytex (apres l'avoir chargée) [a faire une fois] 
  #(cf Rmarkdown cookbook 1.1.2)
}else{
  print("Distribution TinyTex trouvée: installée")
}


rm(list=ls()) #Nettoyage de l'environnement: Suppression de toutes les variables existantes
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Importation du fichier xls et preparation du dataset brut
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

fichier=file.choose()#ouvrir fichier extension "xls"

Dataframe_raw=data.frame(read_xls(fichier,col_names =TRUE))#construit l'objet dataframe a partir du dataset

#summary(Dataframe_raw) #test de la bonne importation

#ordonne le dataframe brut selon le genotype, en placant le WT 1er (utile pour l'export des Df)
Dataframe_raw=Dataframe_raw[order(sub("WT","",Dataframe_raw$Subject.Genotype)),] 

#preparer les variables du dataframe (assigner les bons types de variables aux colonnes du dataframe brut)

# Deux colonnes des temps: [198] "ACQ..Timing.Acquisition.Time..HH.MM.SS.00." ; [200] "AN..Time.Interval.SPLIT" 
Dataframe_raw$ACQ..Timing.Acquisition.Time..HH.MM.SS.00.= chron(times.=Dataframe_raw$ACQ..Timing.Acquisition.Time..HH.MM.SS.00.)

Dataframe_raw$AN..Time.Interval.SPLIT[is.na(Dataframe_raw$AN..Time.Interval.SPLIT)]= "00:00:00" #remplace les valeurs nulles par 0
Dataframe_raw$AN..Time.Interval.SPLIT= chron(times.=Dataframe_raw$AN..Time.Interval.SPLIT) #converti les valeurs en valeurs de type temps
#une colonne des temps:[1] "Exp..File.Date", deja en format date

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Controle qualité du dataset: QC
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creation d'une Table de reference pour la reconaissance des test: Exps_mastertable
VarExm1=c("Open Field","Elevated Plus Maze")
VarExm2=c("OF","EPM")
VarExm3=chron(times.=c("00:30:00","00:05:00"))
VarExm4=chron(times.=c("00:05:00","00:00:00"))
VarExm5=c(72,65)
VarExm6=c(72,65)
Exps_mastertable=data.frame(VarExm1,VarExm2,VarExm3,VarExm4,VarExm5,VarExm6)
colnames(Exps_mastertable)=c("DExperiment","Exp.protocole.name","ACQ.time(HH:MM:SS.00)","AN.Time.interval.split","DimV(cm)","DimH(cm)")

#Creation d'un vecteur comportant les caracteristiques experimentales issues du fichier a tester

Cara_Exps_Fichier=list(Dataframe_raw$Exp..Protocol.Name[1],
                    Dataframe_raw$ACQ..Timing.Acquisition.Time..HH.MM.SS.00.[1],
                    Dataframe_raw$AN..Time.Interval.SPLIT[1],
                    Dataframe_raw$ACQ..Calibration.Vertical[1]*Dataframe_raw$ACQ..Image.Height..Pixels.[1],
                    Dataframe_raw$ACQ..Calibration.Vertical[1]*Dataframe_raw$ACQ..Image.Height..Pixels.[1])
#ajuster horizontal

#Test 
#Retorune la ligne de la master table dont les parametres correspondent exactement a ceux issus du fichier brut
Exp_pos= which(
    (Exps_mastertable$Exp.protocole.name == Cara_Exps_Fichier[1])&
    (Exps_mastertable$`ACQ.time(HH:MM:SS.00)` == Cara_Exps_Fichier[2])&
    (Exps_mastertable$AN.Time.interval.split == Cara_Exps_Fichier[3]))
#print(Exps_mastertable[Exp_pos,1])

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# HUB: Manipulation des données- Spécifique aux tests et production des Graphiques associés
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#HUB
if(Exps_mastertable[Exp_pos,1]==Exps_mastertable[1,1]){
  print("Test detecté OF")
  source("Src/Test_OpenField.R")
}else if (Exps_mastertable[Exp_pos,1]==Exps_mastertable[2,1]) {
  print("Test detecté Elevated Plus Maze")
  source("Src/Test_ElevatedPlusMaze.R")
}else{
  stop("Protocole de test non reconnu")
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Manipulation des données- Commune aux tests
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#table infos groupes étudiés
table_cara_animals_precis=select(Dataframe_raw,Subject.Genotype,Subject.Gender,Subject.Age) %>% 
  group_by(Subject.Genotype,Subject.Gender) %>% 
  summarize(n=n(),
            mean_age=mean(Subject.Age))
table_cara_animals_global=aggregate(n ~ Subject.Genotype, data=table_cara_animals_precis, sum)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Export: classeur excel de plusieurs feuilles (graphs + tables), figures et rapport
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
source("Src/Mes_Fonctions.R")#appel du script/librairie "mesfonctions" pour utiliser mes fonctions

#Appel a une fonction perso pour établir une liste des dataframe (tables) 
#et des graphiques sur la base du type des objets (insensible au nom)

  #la fonction list_objects permet de lister les graphiques et les dataframes  
  #et les rend sous forme de liste de 2 vecteurs (vecteur des tables,vecteur des graphs)

#liste_tables=list_objects(ls())[[1]] =>pluck(list_objects(ls()),1) [libriairie purr]
liste_tables=chuck(list_objects(ls()),1) #vecteur des dataframes 
liste_graphs=chuck(list_objects(ls()),2) #vecteur des graphiques

#export excel
name=tools::file_path_sans_ext(basename(fichier))
File_name=str_replace_all(name," ","_")
Output_filename=paste("Data/Output/","Rapport",File_name,".xlsx",sep="")

#Appel à une fonction personalisée pour sauvegarder chaque graphique au format png
graphsave(liste_graphs,".png") #l'extension peut etre une image (ex.png) ou une image vectorielle/sans perte (ex.pdf)


#Appel a une fonction personalisée pour creer un fichier excel contenant les plots et dataframes
Export.rapport.excel(liste_tables,liste_graphs,Output_filename)

#Generation du rapport PDF
rmarkdown::render(input ="Src/Rapport_PDF_Analyser.Rmd",
                  output_format=pdf_document(),
                  output_file=str_replace_all(paste("../Figures/PDFs/Rapport_PDF_Analyse",name)," ","_"))
print(paste("Analyse du fichier",basename(fichier) ,"terminée. Rapport PDF et fichiers générés"))

