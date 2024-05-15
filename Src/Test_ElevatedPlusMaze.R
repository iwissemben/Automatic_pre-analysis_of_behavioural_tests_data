
#####################################################################################################################################################################################################################
####################################################################### Test Elevated Plus Maze #####################################################################################################################
#####################################################################################################################################################################################################################


# les noms des variables dans les tableaux doivent etre séparés par des points ("."),
#   pas des espaces(" " ) ni des underscore ("_")
#les dataframes formées pour obtenir des informations statistiques sur des populations (groupes) 
  #doivent comporter dans leur nom le mot "_stats" (afin de les lister dans le rapport pdf)
#Afin d'associer les tables(dataframes) au bon graphique il est important de les "tagger" a l'aide le la fonction command()

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Modif globale: Calcul des classes d'age et inclusion dans le dataframe global
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
Dataframe_raw=Dataframe_raw %>% 
  mutate(Classe.Age= ifelse(Subject.Age<=4,"Juvenile",
                            ifelse(between(Subject.Age,left =8 ,right=16),"Young Adult",
                                   ifelse(between(Subject.Age,left =32 ,right=40),"Adult",
                                          ifelse(Subject.Age>=70,"Aged","Autre age "))))) %>% #Ajout d'une colonne classe d'age et definition des classes d'age selon l'age
  relocate(Classe.Age,.after = Subject.Age) #deplacement de la colonne classe.age apres Subject.Age (pour la lisibilité)
  

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 1- Distance totale parcourue pendangt le test selon groupe (cm)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

SousDF_Distance_tot_selon_groupe=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Total.Distance) %>%
  group_by(Subject.Genotype)


SousDF_Distance_tot_selon_groupe_stats=SousDF_Distance_tot_selon_groupe %>% 
  summarise(Distance.totale.moyenne=mean(Total.Distance),
            sd.Total.Distance=sd(Total.Distance),
            n.Total.Distance=n()) %>% 
  mutate(SEM.Distance.totale=sd.Total.Distance/sqrt(n.Total.Distance))

Graph01=ggplot(data=SousDF_Distance_tot_selon_groupe_stats,
              aes(x=fct_relevel(Subject.Genotype,c("WT")),y=Distance.totale.moyenne,fill=Subject.Genotype))+
  #fct relevel (librairie forcats) permet de reordonner les facteurs, ici pour mettre WT en 1er, la suite est ordonnée automatiquement
  geom_bar(stat="identity",
           width=0.4,color="black",size=0.8) + #stat=identity permet de representer les obseravation pour y, plutot que de sommer de les sommer
  geom_errorbar(aes(x=Subject.Genotype,ymin=Distance.totale.moyenne-SEM.Distance.totale,
                    ymax=Distance.totale.moyenne+SEM.Distance.totale),
                width=0.1,size=0.8)+
  
  geom_point(data=SousDF_Distance_tot_selon_groupe,
             aes(x=Subject.Genotype,y=Total.Distance),
             color="black",size=1.5)+
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) +
  
  #scale_x_discrete(limits = c("WT","KO"))+
  
  labs(title=paste("EPM: Distance totale moyenne parcourue selon le genoptype"),
       subtitle="Barres d'erreur: SEM",
       x="Subject Genotype",
       y=paste("Mean Total Distance","(cm)"),
       caption="Kourrich lab, WBR")

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph1"
comment(SousDF_Distance_tot_selon_groupe)=append(comment(SousDF_Distance_tot_selon_groupe_stats),"Df.Graph01")
comment(SousDF_Distance_tot_selon_groupe_stats)=append(comment(SousDF_Distance_tot_selon_groupe_stats),"Df.Graph01")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 2- Temps moyen passé dans les bras ouverts (sec)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

SousDF_Temps_Zone_Bras_O_Selon_Groupe=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Time.in.Zone..Seconds....bras.ouverts.tot) %>% 
  group_by(Subject.Genotype)

SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats=SousDF_Temps_Zone_Bras_O_Selon_Groupe %>% 
  summarize(Moyenne.temps.bras.ouvert=mean(Time.in.Zone..Seconds....bras.ouverts.tot),
            Sd.temps.bras.ouvert=sd(Time.in.Zone..Seconds....bras.ouverts.tot),
            n.temps.bras.ouvert=n(),
            SEM.temps.bras.ouvert=Sd.temps.bras.ouvert/sqrt(n.temps.bras.ouvert))

Graph02=ggplot(data=SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats,
              aes(x=fct_relevel(Subject.Genotype,c("WT")),y=Moyenne.temps.bras.ouvert,fill=Subject.Genotype)) +
  geom_bar(stat="identity",
           width=0.4,color="black",size=0.8)+
  geom_errorbar(aes(x=Subject.Genotype,ymin=Moyenne.temps.bras.ouvert-SEM.temps.bras.ouvert,
                    ymax=Moyenne.temps.bras.ouvert+SEM.temps.bras.ouvert),
                width=0.1,size=0.8)+
  geom_point(data=SousDF_Temps_Zone_Bras_O_Selon_Groupe,
             aes(x=Subject.Genotype,y=Time.in.Zone..Seconds....bras.ouverts.tot),
             color="black",size=1.5)+
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))+
  
  labs(title=paste("EPM: Temps moyen passé en bras ouvert selon le genoptype"),
       subtitle="Barres d'erreur: SEM",
       x="Subject Genotype",
       y=paste("Temps moyen en bras ouvert","(s)"),
       caption="Kourrich lab, WBR")
#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph2"
comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe),"Df.Graph02")
comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats),"Df.Graph02")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 3- Nombre d'entrees dans les bras ouverts
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

SousDF_nbre_entree_bo_Selon_Groupe=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Entries.in.Zone...bras.ouverts.tot) %>% 
  group_by(Subject.Genotype)


SousDF_nbre_entree_bo_Selon_Groupe_stats=SousDF_nbre_entree_bo_Selon_Groupe %>% 
  summarize(Moyenne.entrees.bras.ouvert=mean(Entries.in.Zone...bras.ouverts.tot),
            Sd.entrees.bras.ouvert=sd(Entries.in.Zone...bras.ouverts.tot),
            n.entrees.bras.ouvert=n(),
            SEM.entrees.bras.ouvert=Sd.entrees.bras.ouvert/sqrt(n.entrees.bras.ouvert))

Graph03=ggplot(data=SousDF_nbre_entree_bo_Selon_Groupe_stats,
              aes(x=Subject.Genotype,y=Moyenne.entrees.bras.ouvert,fill=Subject.Genotype)) +
  geom_bar(stat="identity",
           width=0.4,color="black",size=0.8)+
  geom_errorbar(aes(x=Subject.Genotype,ymin=Moyenne.entrees.bras.ouvert-SEM.entrees.bras.ouvert,
                    ymax=Moyenne.entrees.bras.ouvert+SEM.entrees.bras.ouvert),
                width=0.1,size=0.8)+
  geom_point(data=SousDF_nbre_entree_bo_Selon_Groupe,
             aes(x=Subject.Genotype,y=Entries.in.Zone...bras.ouverts.tot),
             color="black",size=1.5)+
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))+
  
  labs(title=paste("EPM: Nombre d'entrées moyen dans les bras ouvert selon le genoptype"),
       subtitle="Barres d'erreur: SEM",
       x="Subject Genotype",
       y=paste("Nombre d'entrées en bras ouverts"),
       caption="Kourrich lab, WBR")
#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph3"
comment(SousDF_nbre_entree_bo_Selon_Groupe)=append(comment(SousDF_nbre_entree_bo_Selon_Groupe),"Df.Graph03")
comment(SousDF_nbre_entree_bo_Selon_Groupe_stats)=append(comment(SousDF_nbre_entree_bo_Selon_Groupe_stats),"Df.Graph03")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 4- Temps passé dans les bras ouverts au cours du temps selon le Genotype
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

SousDF_Temps_Zone_Bras_O_Selon_Genotype=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Subject.Age,Classe.Age,Time.in.Zone..Seconds....bras.ouverts.tot)

SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage=SousDF_Temps_Zone_Bras_O_Selon_Genotype %>% 
  group_by(Subject.Genotype,Classe.Age) #on groupe par genotype puis par classe d'age

SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_stats=SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage %>% 
  summarize(Moyenne.temps.bras.ouvert=mean(Time.in.Zone..Seconds....bras.ouverts.tot),
            Sd.temps.bras.ouvert=sd(Time.in.Zone..Seconds....bras.ouverts.tot),
            n.temps.bras.ouvert=n(),
            SEM.temps.bras.ouvert=Sd.temps.bras.ouvert/sqrt(n.temps.bras.ouvert))

Graph04=ggplot(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_stats,
              aes(x=fct_relevel(Classe.Age,c("Juvenile","Young Adult","Adult","Aged")),y=Moyenne.temps.bras.ouvert,group=Subject.Genotype,
                  color=Subject.Genotype))+
  geom_line(position="identity",
            size=1.5)+
  
  geom_errorbar(aes(x=Classe.Age,ymin=Moyenne.temps.bras.ouvert-SEM.temps.bras.ouvert,
                    ymax=Moyenne.temps.bras.ouvert+SEM.temps.bras.ouvert),width=0.1,size=0.6)+
  
  geom_point(color="black")+
  
  geom_point(data=SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage,
             aes(x=Classe.Age,y=Time.in.Zone..Seconds....bras.ouverts.tot,color=Subject.Genotype),size=0.4)+
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))+
  
  labs(title=paste("EPM: Temps moyen passé en bras ouvert selon le genotype à differents ages"),
       subtitle="Barres d'erreur: SEM",
       x="Subject Genotype",
       y=paste("Temps moyen passé en bras ouvert (s)"),
       caption="Kourrich lab, WBR")

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph4"
comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype),"Df.Graph04")
comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage),"Df.Graph04")
comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_stats)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_stats),"Df.Graph04")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 5- nombre entree les bras ouverts au cours du temps selon le Genotype
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

SousDF_nbentrees_Zone_Bras_O_Selon_Genotype=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Subject.Age,Classe.Age,Entries.in.Zone...bras.ouverts.tot)

SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage=SousDF_nbentrees_Zone_Bras_O_Selon_Genotype %>% 
  group_by(Subject.Genotype,Classe.Age) #on groupe par genotype puis par classe d'age

SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_stats=SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage %>% 
  summarize(Moyenne.nentrees.bras.ouvert=mean(Entries.in.Zone...bras.ouverts.tot),
            Sd.nentrees.bras.ouvert=sd(Entries.in.Zone...bras.ouverts.tot),
            n.nentrees.bras.ouvert=n(),
            SEM.nentrees.bras.ouvert=Sd.nentrees.bras.ouvert/sqrt(n.nentrees.bras.ouvert))
Graph05=ggplot(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_stats,
              aes(x=fct_relevel(Classe.Age,c("Juvenile","Young Adult","Adult","Aged")),y=Moyenne.nentrees.bras.ouvert,
                  group=Subject.Genotype,
                  color=Subject.Genotype))+

  geom_line(position="identity",size=1.5)+
  
  geom_errorbar(aes(x=Classe.Age,ymin=Moyenne.nentrees.bras.ouvert-SEM.nentrees.bras.ouvert,
                    ymax=Moyenne.nentrees.bras.ouvert+SEM.nentrees.bras.ouvert),width=0.1,size=0.6)+
  
  geom_point(color="black")+
  
  geom_point(data=SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage,
             aes(x=Classe.Age,y=Entries.in.Zone...bras.ouverts.tot,color=Subject.Genotype),size=0.4)+
  
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))+
  
  labs(title=paste("EPM: Nombre d'entrées en bras ouverts selon le genotype à differents ages"),
       subtitle="Barres d'erreur: SEM",
       x="Subject Genotype",
       y=paste("Nombre d'entrées en bras ouvert"),
       caption="Kourrich lab, WBR")

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph5"
comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype)=append(comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype),"Df.Graph05")
comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage)=append(comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage),"Df.Graph05")
comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_stats)=append(comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_stats),"Df.Graph05")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 6- Distance totale parcourue pendant le test selon groupe (cm) et le genre
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

Graph06=Graph01+  #Le graph 6 est identique au graph 1 sauf qu'il subdivise les données selon le facteur Subject.Gender
  facet_wrap(~Subject.Gender)
# il n'est pas necessaire de produire les dataframes pour chaque genre afin de le generer puisque le genre est contenu dans  le dataframe
# qui produit le Graph1, il est ainsi possible de reprendre l'objet Graph1 pour lui dire de realiser un facetting selon le facteur Genre
# on produit ici les data frames selon le genre a des fins de controle des resultats,(et d'aileurs) parceque lorsqu'on produit des graph 
# notre Programme R est concu pour exiger d'associer les graphs a des tables (dataframes).
SousDF_Distance_tot_selon_groupe_Female=SousDF_Distance_tot_selon_groupe %>% 
  filter(Subject.Gender=="Female")
SousDF_Distance_tot_selon_groupe_Male=SousDF_Distance_tot_selon_groupe %>% 
  filter(Subject.Gender=="Male")

SousDF_Distance_tot_selon_groupe_stats_Female=SousDF_Distance_tot_selon_groupe_Female %>% 
  summarise(Distance.totale.moyenne=mean(Total.Distance),
            sd.Total.Distance=sd(Total.Distance),
            n.Total.Distance=n()) %>% 
  mutate(SEM.Distance.totale=sd.Total.Distance/sqrt(n.Total.Distance))

SousDF_Distance_tot_selon_groupe_stats_Male=SousDF_Distance_tot_selon_groupe_Male %>% 
  summarise(Distance.totale.moyenne=mean(Total.Distance),
            sd.Total.Distance=sd(Total.Distance),
            n.Total.Distance=n()) %>% 
  mutate(SEM.Distance.totale=sd.Total.Distance/sqrt(n.Total.Distance))

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph6"
comment(SousDF_Distance_tot_selon_groupe_Female)=append(comment(SousDF_Distance_tot_selon_groupe_Female),"Df.Graph06")
comment(SousDF_Distance_tot_selon_groupe_Male)=append(comment(SousDF_Distance_tot_selon_groupe_Male),"Df.Graph06")
comment(SousDF_Distance_tot_selon_groupe_stats_Female)=append(comment(SousDF_Distance_tot_selon_groupe_stats_Female),"Df.Graph06")
comment(SousDF_Distance_tot_selon_groupe_stats_Male)=append(comment(SousDF_Distance_tot_selon_groupe_stats_Male),"Df.Graph06")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 7- Temps moyen passé dans les bras ouverts (sec) selon le genotype et le genre
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

Graph07=Graph02+
  facet_wrap(~Subject.Gender)

SousDF_Temps_Zone_Bras_O_Selon_Groupe_Female=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Time.in.Zone..Seconds....bras.ouverts.tot) %>% 
  filter(Subject.Gender=="Female") %>% 
  group_by(Subject.Genotype)

SousDF_Temps_Zone_Bras_O_Selon_Groupe_Male=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Time.in.Zone..Seconds....bras.ouverts.tot) %>% 
  filter(Subject.Gender=="Male") %>% 
  group_by(Subject.Genotype)

SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats_Female=SousDF_Temps_Zone_Bras_O_Selon_Groupe_Female %>% 
  summarize(Moyenne.temps.bras.ouvert=mean(Time.in.Zone..Seconds....bras.ouverts.tot),
            Sd.temps.bras.ouvert=sd(Time.in.Zone..Seconds....bras.ouverts.tot),
            n.temps.bras.ouvert=n(),
            SEM.temps.bras.ouvert=Sd.temps.bras.ouvert/sqrt(n.temps.bras.ouvert))

SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats_Male=SousDF_Temps_Zone_Bras_O_Selon_Groupe_Male %>% 
  summarize(Moyenne.temps.bras.ouvert=mean(Time.in.Zone..Seconds....bras.ouverts.tot),
            Sd.temps.bras.ouvert=sd(Time.in.Zone..Seconds....bras.ouverts.tot),
            n.temps.bras.ouvert=n(),
            SEM.temps.bras.ouvert=Sd.temps.bras.ouvert/sqrt(n.temps.bras.ouvert))

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph7"
comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_Female)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_Female),"Df.Graph07")
comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_Male)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_Male),"Df.Graph07")
comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats_Female)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats_Female),"Df.Graph07")
comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats_Male)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Groupe_stats_Male),"Df.Graph07")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 8- Nombre d'entrees dans les bras ouverts selon le genotype et le genre
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

Graph08=Graph03+
  facet_wrap(~Subject.Gender)
SousDF_nbre_entree_bo_Selon_Groupe_Female=SousDF_nbre_entree_bo_Selon_Groupe %>% 
  filter(Subject.Gender=="Female")

SousDF_nbre_entree_bo_Selon_Groupe_Male=SousDF_nbre_entree_bo_Selon_Groupe %>% 
  filter(Subject.Gender=="Male")

SousDF_nbre_entree_bo_Selon_Groupe_stats_Female=SousDF_nbre_entree_bo_Selon_Groupe_Female %>% 
  summarize(Moyenne.entrees.bras.ouvert=mean(Entries.in.Zone...bras.ouverts.tot),
            Sd.entrees.bras.ouvert=sd(Entries.in.Zone...bras.ouverts.tot),
            n.entrees.bras.ouvert=n(),
            SEM.entrees.bras.ouvert=Sd.entrees.bras.ouvert/sqrt(n.entrees.bras.ouvert))

SousDF_nbre_entree_bo_Selon_Groupe_stats_Male=SousDF_nbre_entree_bo_Selon_Groupe_Male %>% 
  summarize(Moyenne.entrees.bras.ouvert=mean(Entries.in.Zone...bras.ouverts.tot),
            Sd.entrees.bras.ouvert=sd(Entries.in.Zone...bras.ouverts.tot),
            n.entrees.bras.ouvert=n(),
            SEM.entrees.bras.ouvert=Sd.entrees.bras.ouvert/sqrt(n.entrees.bras.ouvert))

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph8"
comment(SousDF_nbre_entree_bo_Selon_Groupe_Female)=append(comment(SousDF_nbre_entree_bo_Selon_Groupe_Female),"Df.Graph08")
comment(SousDF_nbre_entree_bo_Selon_Groupe_Male)=append(comment(SousDF_nbre_entree_bo_Selon_Groupe_Male),"Df.Graph08")
comment(SousDF_nbre_entree_bo_Selon_Groupe_stats_Female)=append(comment(SousDF_nbre_entree_bo_Selon_Groupe_stats_Female),"Df.Graph08")
comment(SousDF_nbre_entree_bo_Selon_Groupe_stats_Male)=append(comment(SousDF_nbre_entree_bo_Selon_Groupe_stats_Male),"Df.Graph08")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 9- Temps passé dans les bras ouverts au cours du temps selon le Genotype et le Genre
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

SousDF_Temps_Zone_Bras_O_Selon_Genotype_genre=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Subject.Age,Classe.Age,Time.in.Zone..Seconds....bras.ouverts.tot)

SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre=SousDF_Temps_Zone_Bras_O_Selon_Genotype_genre %>% 
  group_by(Subject.Genotype,Classe.Age,Subject.Gender) #on groupe par genotype puis par classe d'age

SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre_stats=SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre %>% 
  summarize(Moyenne.temps.bras.ouvert=mean(Time.in.Zone..Seconds....bras.ouverts.tot),
            Sd.temps.bras.ouvert=sd(Time.in.Zone..Seconds....bras.ouverts.tot),
            n.temps.bras.ouvert=n(),
            SEM.temps.bras.ouvert=Sd.temps.bras.ouvert/sqrt(n.temps.bras.ouvert))

Graph09=ggplot(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre_stats,
               aes(x=fct_relevel(Classe.Age,c("Juvenile","Young Adult","Adult","Aged")),y=Moyenne.temps.bras.ouvert,group=Subject.Genotype,
                   color=Subject.Genotype))+
  geom_line(position="identity",
            size=1.5)+
  
  geom_errorbar(aes(x=Classe.Age,ymin=Moyenne.temps.bras.ouvert-SEM.temps.bras.ouvert,
                    ymax=Moyenne.temps.bras.ouvert+SEM.temps.bras.ouvert),width=0.1,size=0.6)+
  
  geom_point(color="black")+
  
  geom_point(data=SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre,
             aes(x=Classe.Age,y=Time.in.Zone..Seconds....bras.ouverts.tot,color=Subject.Genotype),size=0.4)+
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))+
  
  facet_wrap(~Subject.Gender)+

labs(title=paste("EPM: Temps moyen passé en bras ouvert selon le genotype à differents ages"),
     subtitle="Barres d'erreur: SEM",
     x="Subject Genotype",
     y=paste("Temps moyen passé en bras ouvert (s)"),
     caption="Kourrich lab, WBR")

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph9"
comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_genre)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_genre),"Df.Graph09")
comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre),"Df.Graph09")
comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre_stats)=append(comment(SousDF_Temps_Zone_Bras_O_Selon_Genotype_classeage_genre_stats),"Df.Graph09")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 10- nombre entree les bras ouverts au cours du temps selon le Genotype et le genre
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_genre=Dataframe_raw %>% 
  select(Subject.Genotype,Subject.Name,Subject.Gender,Subject.Age,Classe.Age,Entries.in.Zone...bras.ouverts.tot)

SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre=SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_genre %>% 

  group_by(Subject.Genotype,Classe.Age,Subject.Gender) #on groupe par genotype puis par classe d'age

SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre_stats=SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre %>% 
  summarize(Moyenne.nentrees.bras.ouvert=mean(Entries.in.Zone...bras.ouverts.tot),
            Sd.nentrees.bras.ouvert=sd(Entries.in.Zone...bras.ouverts.tot),
            n.nentrees.bras.ouvert=n(),
            SEM.nentrees.bras.ouvert=Sd.nentrees.bras.ouvert/sqrt(n.nentrees.bras.ouvert))
Graph10=ggplot(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre_stats,
               aes(x=fct_relevel(Classe.Age,c("Juvenile","Young Adult","Adult","Aged")),y=Moyenne.nentrees.bras.ouvert,
                   group=Subject.Genotype,
                   color=Subject.Genotype))+
  
  geom_line(position="identity",size=1.5)+
  
  geom_errorbar(aes(x=Classe.Age,ymin=Moyenne.nentrees.bras.ouvert-SEM.nentrees.bras.ouvert,
                    ymax=Moyenne.nentrees.bras.ouvert+SEM.nentrees.bras.ouvert),width=0.1,size=0.6)+
  
  geom_point(color="black")+
  
  geom_point(data=SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre,
             aes(x=Classe.Age,y=Entries.in.Zone...bras.ouverts.tot,color=Subject.Genotype),size=0.4)+
  facet_wrap(~Subject.Gender)+
  
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))+
  
  labs(title=paste("EPM: Nombre d'entrées en bras ouverts selon le genotype à differents ages"),
       subtitle="Barres d'erreur: SEM",
       x="Subject Genotype",
       y=paste("Nombre d'entrées en bras ouvert"),
       caption="Kourrich lab, WBR")

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph5"
comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_genre)=append(comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_genre),"Df.Graph10")
comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre)=append(comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre),"Df.Graph10")
comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre_stats)=append(comment(SousDF_nbentrees_Zone_Bras_O_Selon_Genotype_classeage_genre_stats),"Df.Graph10")
