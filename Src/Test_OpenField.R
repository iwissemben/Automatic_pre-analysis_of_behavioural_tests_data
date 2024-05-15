
#####################################################################################################################################################################################################################
####################################################################### Test OpenField ##############################################################################################################################
#####################################################################################################################################################################################################################


# les noms des variables dans les tableaux doivent etre séparés par des points ("."),
#   pas des espaces(" " ) ni des underscore ("_")
#les dataframes formées pour obtenir des informations statistiques sur des populations (groupes) 
#doivent comporter dans leur nom le mot "_stats" (afin de les lister dans le rapport pdf)
#Afin d'associer les tables(dataframes) au bon graphique il est important de les "tagger" a l'aide le la fonction command()

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 1- Distance totale selon groupe
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
SousDF_Distance_tot_selon_groupe=select(Dataframe_raw,Subject.Genotype,Subject.Name,Subject.Gender,Total.Distance) %>%
  group_by(Subject.Genotype)


SousDF_Distance_tot_selon_groupe_stats=SousDF_Distance_tot_selon_groupe %>% 
  summarise(Distance.totale.moyenne=mean(Total.Distance),sd.Total.Distance=sd(Total.Distance),n.Total.Distance=n()) %>% 
  mutate(SEM.Distance.totale=sd.Total.Distance/sqrt(n.Total.Distance))

Graph01=ggplot(data=SousDF_Distance_tot_selon_groupe_stats,
              aes(x=fct_relevel(Subject.Genotype,c("WT")),y=Distance.totale.moyenne,fill=Subject.Genotype))+
  #fct relevel (librairie forcats) permet de reordonner les facteurs, ici pour mettre WT en 1er, la suite est ordonnée automatiquement
  geom_bar(stat="identity",
           width=0.4,color="black",size=1) + #stat=identity permet d'entrer les données pour y, plutot que de sommer
  
  geom_errorbar(aes(x=Subject.Genotype,ymin=Distance.totale.moyenne-SEM.Distance.totale,
                    ymax=Distance.totale.moyenne+SEM.Distance.totale),
                width=0.1,size=0.8)+
  
  geom_point(data=SousDF_Distance_tot_selon_groupe,
             aes(x=Subject.Genotype,y=Total.Distance),
             color="black",size=1.5)+
  
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) +
  
  #scale_x_discrete(limits = c("WT","KO"))+
  
  labs(title=paste("OpenField: Distance totale moyenne parcourue en Open Field selon le genoptype"),
       subtitle="Barres d'erreur: SEM",
       x="Subject Genotype",
       y=paste("Mean Total Distance","(Units)"),
       caption="Kourrich lab, WBR")

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph1"
comment(SousDF_Distance_tot_selon_groupe)=append(comment(SousDF_Distance_tot_selon_groupe_stats),"Df.Graph01")
comment(SousDF_Distance_tot_selon_groupe_stats)=append(comment(SousDF_Distance_tot_selon_groupe_stats),"Df.Graph01")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 2- Distance totale moyenne selon groupe et groupé par sexe
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
SousDF_Distance_tot_selon_groupe_sexe=select(Dataframe_raw,Subject.Genotype,Subject.Name,Subject.Gender,Total.Distance) %>%
  group_by(Subject.Genotype,Subject.Gender)


SousDF_Distance_tot_selon_groupe_sexe_stats=SousDF_Distance_tot_selon_groupe_sexe %>% 
  summarise(Distance.totale.moyenne=mean(Total.Distance),sd.Total.Distance=sd(Total.Distance),n.Total.Distance=n()) %>% 
  mutate(SEM.Distance.totale=sd.Total.Distance/sqrt(n.Total.Distance))


Graph02=ggplot(data=SousDF_Distance_tot_selon_groupe_sexe_stats,
              aes(x=fct_relevel(Subject.Genotype,c("WT","CTL")),y=Distance.totale.moyenne,fill=Subject.Gender))+
  geom_bar(stat="identity",position=position_dodge(),width=0.4,color="black",size=1) + 
  #stat=identity permet de renseigner les données pour y , plutot que de sommer les occurences
  
  geom_errorbar(aes(x=Subject.Genotype,
                    ymin=Distance.totale.moyenne-SEM.Distance.totale,
                    ymax=Distance.totale.moyenne+SEM.Distance.totale),
                position=position_dodge(.4),size=0.8,width=0.2) +
  
  geom_point(data=SousDF_Distance_tot_selon_groupe_sexe,aes(x=Subject.Genotype,y=Total.Distance),
             color="black",size=2,position=position_dodge(.4))+
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) +
  
  labs(title="OpenField: Distance totale moyenne parcourue selon \nle genoptype et le sexe des sujets",
       subtitle="Barres d'erreur: SEM",
       x="Subject Genotype",
       y=paste("Mean Total Distance","(Units)"),
       caption="Kourrich lab, WBR")

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph.2"
comment(SousDF_Distance_tot_selon_groupe_sexe)=append(comment(SousDF_Distance_tot_selon_groupe_sexe),"Df.Graph02")
comment(SousDF_Distance_tot_selon_groupe_sexe_stats)=append(comment(SousDF_Distance_tot_selon_groupe_sexe_stats),"Df.Graph02")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 3- Distance parcourue totale  moyenne en fonction du temps et des genotypes
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
SousDF_DPT_genotype=select(Dataframe_raw,Subject.Name,Subject.Genotype,
                           Subject.Gender,Subject.Age,starts_with("Total.Distance...T"))

SousDF_DPT_genotype_tidy=SousDF_DPT_genotype %>% 
  pivot_longer(-c(Subject.Name,Subject.Genotype,Subject.Gender,Subject.Age),
               values_to="Distance.totale.parcourue",names_to="Temps") %>%
  mutate(Temps=str_remove(Temps,"Total.Distance...T")) %>% 
  mutate(Temps=as.numeric(Temps)) %>% 
  mutate(Temps=Temps*Dataframe_raw$AN..Time.Interval.SPLIT[1]) %>% 
  mutate(Temps=as.character(Temps)) %>% 
  group_by(Subject.Genotype,Temps) 

SousDF_DPT_genotype_tidy_stats=summarise(SousDF_DPT_genotype_tidy,
                                         Distance.parcourue.moy=mean(Distance.totale.parcourue),
                                         Sd.DPT=sd(Distance.totale.parcourue),
                                         n.DPT=n()) %>% 
  mutate(SEM.DPT=Sd.DPT/sqrt(n.DPT),DPT.Cumul=cumsum(Distance.parcourue.moy))%>% 
  group_by(Subject.Genotype)



Graph03=ggplot(data=SousDF_DPT_genotype_tidy_stats,
              aes(x=Temps,y=DPT.Cumul,group=Subject.Genotype,color=Subject.Genotype))+
  
  geom_line(position="identity",size=1) +
  
  geom_errorbar(aes(x=Temps,
                    ymin=DPT.Cumul-SEM.DPT,
                    ymax=DPT.Cumul+SEM.DPT),
                size=0.8,width=0.2,color="black")+
  geom_point(shape=24, fill="black",size=2)+ 
  
  labs(title="OpenFIeld: Distance moyenne parcourue \nselon le genoptype au cours du temps",
       subtitle="Barres d'erreur: SEM",
       x="Temps (Units)",
       y=paste("Mean Total Distance","(HH:MM:SS)"),
       caption="Kourrich lab, WBR")

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph.3"
comment(SousDF_DPT_genotype)=append(comment(SousDF_DPT_genotype),"Df.Graph03")
comment(SousDF_DPT_genotype_tidy)=append(comment(SousDF_DPT_genotype_tidy),"Df.Graph03")
comment(SousDF_DPT_genotype_tidy_stats)=append(comment(SousDF_DPT_genotype_tidy_stats),"Df.Graph03")
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 4- Distance parcourue moyenne en fonction des genotypes, et de la zone au cours du temps
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
SousDF_Distances_Zones=select(Dataframe_raw,Subject.Name,Subject.Age,
                              Subject.Genotype,Subject.Gender,
                              starts_with("Distance.in.Zone")&contains("...T"))

SousDF_Distances_Zones_tidy=SousDF_Distances_Zones %>% 
  pivot_longer(-c(Subject.Name,Subject.Age,Subject.Genotype,Subject.Gender),
               values_to = "Distance.en.zone",names_to="Zone") %>% 
  mutate(Zone=str_remove(Zone,"Distance.in.Zone...")) %>% 
  mutate(Zone=str_replace(Zone,"...T","_T")) %>%  #etape forcée car le . pose probleme pour la fonction separate
  separate(Zone,c("Zone","Temps"),sep="_") %>% 
  #Gestion des temps: suppression du T de Tn, convertir le n de char a num, produit du num par un temps, donne temps, 
  #reconversion en char pour affichage graph
  mutate(Temps=str_remove(Temps,"T")) %>% 
  mutate(Temps=as.numeric(Temps)) %>% 
  mutate(Temps=Temps*Dataframe_raw$AN..Time.Interval.SPLIT[1]) %>% 
  mutate(Temps=as.character.Date(Temps)) %>% 
  group_by(Subject.Genotype,Zone,Temps)

SousDF_Distances_Zones_Stats= summarise(SousDF_Distances_Zones_tidy,
                                        moy.Dist.en.zone=mean(Distance.en.zone),
                                        sd.Dist.en.zone=sd(Distance.en.zone),
                                        n.Dist.en.zone=n()) %>% 
  mutate(SEM.Distance.zone=sd.Dist.en.zone/sqrt(n.Dist.en.zone))

Graph04=ggplot(data=SousDF_Distances_Zones_Stats,
              aes(x=Temps,y=moy.Dist.en.zone,group=interaction(Subject.Genotype,Zone),
                  color=Zone,linetype=Subject.Genotype))+
  geom_line(position="identity",size=1)+
  #facet_wrap(vars(Subject.Genotype))+
  geom_errorbar(aes(x=Temps,
                    ymin=moy.Dist.en.zone-SEM.Distance.zone,
                    ymax=moy.Dist.en.zone+SEM.Distance.zone),
                size=0.8,width=0.2)+
  labs(title="OpenField: Distance moyenne parcourue selon la zone \net le genoptype au cours du temps ",
       subtitle="Barres d'erreur: SEM",
       x="Temps (HH:MM:SS)",
       y=paste("Distance moyenne","(Units)"),
       caption="Kourrich lab, WBR") 
  #facet_wrap(~Zone,ncol=1)

#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph.4"
comment(SousDF_Distances_Zones)=append(comment(SousDF_Distances_Zones),"Df.Graph04")
comment(SousDF_Distances_Zones_tidy)=append(comment(SousDF_Distances_Zones_tidy),"Df.Graph04")
comment(SousDF_Distances_Zones_Stats)=append(comment(SousDF_Distances_Zones_Stats),"Df.Graph04")
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Graph 5- Distance parcourue moyenne en fonction des genotypes, et de 2 zones au cours du temps
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

SousDF_Distances_2_Zones_tidy=SousDF_Distances_Zones_tidy %>% 
  mutate(Zone=gsub(".*.Periphery","Periphery",Zone))

SousDF_Distances_2_Zones_Stats= summarise(SousDF_Distances_2_Zones_tidy,
                                          moy.Dist.en.zone=mean(Distance.en.zone),
                                          sd.Dist.en.zone=sd(Distance.en.zone),
                                          n.Dist.en.zone=n()) %>% 
  mutate(SEM.Distance.zone=sd.Dist.en.zone/sqrt(n.Dist.en.zone))

Graph05=ggplot(data=SousDF_Distances_2_Zones_Stats,
              aes(x=Temps,y=moy.Dist.en.zone,group=interaction(Subject.Genotype,Zone),
                  color=Zone,linetype=Subject.Genotype))+
  geom_line(position="identity",size=1)+
  #facet_wrap(vars(Subject.Genotype))+
  geom_errorbar(aes(x=Temps,
                    ymin=moy.Dist.en.zone-SEM.Distance.zone,
                    ymax=moy.Dist.en.zone+SEM.Distance.zone),
                size=0.8,width=0.2)+
  labs(title="OpenField: Distance moyenne parcourue selon la zone \net le genoptype au cours du temps ",
       subtitle="Barres d'erreur: SEM",
       x="Temps (HH:MM:SS)",
       y=paste("Distance moyenne","(Units)"),
       caption="Kourrich lab, WBR")
#Pour export:affecter aux dataframes crées pour faire le graph 1 la classe "Df.Graph.5"
comment(SousDF_Distances_2_Zones_tidy)=append(comment(SousDF_Distances_2_Zones_tidy),"Df.Graph05")
comment(SousDF_Distances_2_Zones_Stats)=append(comment(SousDF_Distances_2_Zones_Stats),"Df.Graph05")