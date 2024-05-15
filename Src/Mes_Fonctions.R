
#####################################################################################################################################################################################################################
####################################################################### Mes Fonctions ###############################################################################################################################
#####################################################################################################################################################################################################################

#fonction sauvegarde des graphiques
graphsave=function(liste_de_graphs,extension){
  for(i in liste_graphs){
    print(get(i))
    ggsave(paste("Figures/Graphs/",File_name,"_",i,extension,sep=""),
           units=c("in"),width=12,height=8)
    print(paste(File_name,i,"sauvegarde dans: Figures/Graphs"))
  }
}

#la fonction list_objects permet de lister les graphiques et les dataframes  
#et les rend sous forme de liste de 2 vecteurs (vecteur des tables,vecteur des graphs)

list_objects=function(liste_des_objets){
  ls_tables=c() #vecteur des tables
  ls_graphs=c() #vecteur des graphiques
  for (i in liste_des_objets){
    if(is.data.frame(get(i))){
      ls_tables=append(ls_tables,i)
      #print(paste(i,"est du type",class(get(i))))
    }
    else if(is.ggplot(get(i))){
      ls_graphs=append(ls_graphs,i)
      #print(paste(i,"est du type",class(get(i))))
    }
    #else {print("non")}
  }
  return(list(ls_tables,ls_graphs)) #retourne une liste de 2 vecteurs, des tabkes(en pos 1), des graphs (en pos 2)
}

#test listing des tables associées au graph i
list.tables.comment=function(liste_a_tester,commentaire){
  liste_resultante=c()
  for(i in liste_a_tester){
    if(identical(comment(get(i)),commentaire)){
      liste_resultante=append(liste_resultante,i)
      }
  }
  return(liste_resultante)
}

#Export vers fichier excel
Export.rapport.excel=function(tables_list_to_test,graphs_list,output_path){
  #creation d'un fichier excel sauvegardant les plots et dataframes
  
  MonClasseur=createWorkbook()
  for (y in graphs_list) {
    #print(y)

    print(get(y)) #Affiche le graph pour l'inserer (R ne peut pas produire un ggplot dans une for loop, il faut la print)
    analysis_sheet=addWorksheet(MonClasseur,sheetName = paste("mafeuille_",y,sep=" "))
    #Sys.sleep(3)
    insertPlot(MonClasseur,sheet=analysis_sheet,
               xy=c(1,1)) #insertion du graphique qui vient d'etre affiché (print(get(x))) et positionnement dans le fichier excel
    print(paste("insertion du graphique:",y))
    comment_graphy=paste("Df.",y,sep="") #doit varier selon le meme identifiant que le graph
    #print(comment_graphy)
    
    #pour chaque feuille le graph et les tables a ajouter
    curr_col=3
    for(i in 1:length(list.tables.comment(tables_list_to_test,comment_graphy))){
      #print(paste("iteration:",i))
      print(paste("Excel:insertion de la table statistique du graph :",y))
      datai=list.tables.comment(tables_list_to_test,comment_graphy)[i]
      #print(ncol(datai))
      writeData(MonClasseur,sheet=analysis_sheet, 
                datai,
                xy=c(curr_col,24)) #ecriture du nom de variable de chaque dataframme comme titre de chaque table
      
      writeDataTable(MonClasseur,sheet=analysis_sheet, #insertion d'une table et positionnement initial
                     x=get(datai), #selection du df a ajouter au fichier excel
                     xy=c(curr_col,25)) #positionnement du df a ajouter (col,row)
      curr_col=curr_col+ncol(get(datai))+2 #obtention de la dimension du df
    }
    
  }
  
  saveWorkbook(wb=MonClasseur,file=output_path,overwrite = TRUE) #sauvegarde le classeur excel
  print(paste("Excel workbook",output_path,"created"))
}