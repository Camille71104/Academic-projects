##########################################################################################
####################### Installations packages ###########################################
##########################################################################################
# Vérification et installation des packages nécessaires
packages <- c("ggplot2", "dplyr", "reshape2", "ggcorrplot", "ggpubr", "lsr", "shiny", "scales")
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(packages, install_if_missing))

##########################################################################################
####################### IMPORTATIONS LIBRARIES ###########################################
##########################################################################################
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggcorrplot)
library(ggpubr)
library(lsr)

##########################################################################################
####################### IMPORTATIONS DES DONNEES #########################################
##########################################################################################
rep <- read.csv("data/Bdd_stress_nettoyee.csv", row.names = "X", stringsAsFactors = TRUE)
lapply(rep, class)
rep$Age <- factor(rep$Age,levels = c("<= 17", "18", "19", "20", "21", "22", "23", ">= 24"))
rep$Pratiques_activites_culturelles <- factor(rep$Pratiques_activites_culturelles, levels = c("Non","Rarement","De temps en temps","Régulièrement"))
rep$Satisfaction_aides_financieres <- factor(rep$Satisfaction_aides_financieres, levels = c("Insuffisant", "Juste", "Suffisant", "Largement suffisant"))
rep$Comparaison_aux_autres <- factor(rep$Comparaison_aux_autres, levels = c("Jamais","Rarement","De temps en temps","Souvent","Tout le temps"))
rep$Sentiment_etre_entoure <- factor(rep$Sentiment_etre_entoure, levels = c("Très peu entouré", "Pas suffisamment entouré", "Très bien entouré"))
rep$Renoncement_visite_famille <- factor(rep$Renoncement_visite_famille, levels = c("J'habite chez mes parents", "Rarement", "De temps en temps", "Souvent"))
palettes <- list("Genre" = c("Homme" = "lightgreen", "Femme" = "lightsalmon", "Autre" = "lightblue"),
                 "Age" = c("<= 17" = "#e5f5e0", "18" = "#a1d99b", "19" = "#74c476", "20" = "#41ab5d", "21" = "#238b45","22" = "#006d2c","23" = "#00441b", ">= 24" = "#00210f"),
                 "Niveau_etude" = c("Bac+1" =  "#A7D8F4", "Bac+2" = "#80C4E7", "Bac+3" = "#56A8D8", "Bac+4" ="#2978B5", "Bac+5" = "#1D4E89"),
                 "Statut_social_famille" = c("Classe aisée" = "#2C6B3F", "Classe moyenne" = "#4D9FCE","Classe populaire" = "#D9534F"),
                 "Trait_personnalité" = c("Agréabilité" = "#FFB88C", "Conscience" = "#B6E7A1", "Extraversion" = "#A7D8E9", "Ouverture" = "#D5A4F1", "Soucieux" = "#F2A7C7"),
                 "Tous" = c("lightblue")
)

server <- function(input, output) {
  
###############################################################################################################################
########################################### ONGLET PRESENTATION DE LA BASE ####################################################  
###############################################################################################################################
  
###############################################################################################################################
########################################### Sous-onglet: Situation générale ###################################################  
###############################################################################################################################

################################################### Pie chart genre ###########################################################
  output$repartition_genre <- renderPlot({
    genre_counts <- rep %>%
      group_by(Genre) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)  # Calcul des pourcentages
    
    # Création du graphique à barres avec les pourcentages au-dessus des barres
    ggplot(genre_counts, aes(x = Genre, y = percentage, fill = Genre)) + 
      geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                vjust = -0.5, size = 6, color = "black") +  # Ajouter les pourcentages au-dessus des barres
      labs(title = "Répartition des genres dans les répondants", x = NULL, y = NULL, fill = "Genre") + 
      theme_minimal() +  # Thème minimaliste
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank()) + 
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,60))
  })
################################################### Bar chart âge ############################################################  
  output$hist_age <- renderPlot({
    age_counts <- rep %>%
      group_by(Age) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)  # Calcul des pourcentages
    
    # Création du graphique à barres avec les pourcentages au-dessus des barres
    ggplot(age_counts, aes(x = Age, y = percentage, fill = Age)) + 
      geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                vjust = -0.5, size = 6, color = "black") +  # Ajouter les pourcentages au-dessus des barres
      labs(title = "Répartition des âges dans les répondants", x = "  ", y = "  ", fill = "Âge") + 
      theme_minimal() +  # Thème minimaliste
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank()) +  # Pour améliorer la lisibilité de l'axe X
      ylim(0,50) +
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,25))
  })
########################################### Bar chart niveau etude ############################################################  
  output$bar_classe <- renderPlot({
    niv_counts <- rep %>%
      group_by(Niveau_etude) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)  # Calcul des pourcentages
    
    # Création du graphique à barres avec les pourcentages au-dessus des barres
    ggplot(niv_counts, aes(x = Niveau_etude, y = percentage, fill = Niveau_etude)) + 
      geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                vjust = -0.5, size = 6, color = "black") +  # Ajouter les pourcentages au-dessus des barres
      labs(title = "Répartition des niveaux d'étude dans les répondants", x = "  ", y = "  ") + 
      theme_minimal() +  # Thème minimaliste
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank()) +  # Pour améliorer la lisibilité de l'axe X
      ylim(0,50) +
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,35))
  })

########################################### Pie chart Classe sociale ############################################################  

  output$class_soc <- renderPlot({
    class_counts <- rep %>%
      group_by(Statut_social_famille) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)  # Calcul des pourcentages
    
    # Création du graphique à barres avec les pourcentages au-dessus des barres
    ggplot(class_counts, aes(x = Statut_social_famille, y = percentage, fill = Statut_social_famille)) + 
      geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                vjust = -0.5, size = 6, color = "black") +  # Ajouter les pourcentages au-dessus des barres
      labs(title = "Répartition des classes sociales dans les répondants", x = NULL, y = NULL, fill = "Classes sociales") + 
      theme_minimal() +  # Thème minimaliste
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank()) +  # Pour améliorer la lisibilité de l'axe X
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,80))
  })
 
########################################### Bar chart trait personnalité ############################################################  

  output$trait_perso <- renderPlot({
    perso_counts <- rep %>%
      group_by(Trait_personnalité) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)  # Calcul des pourcentages
    
    # Création du graphique à barres avec les pourcentages au-dessus des barres
    ggplot(perso_counts, aes(x = Trait_personnalité, y = count, fill = Trait_personnalité)) + 
      geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                vjust = -0.5, size = 6, color = "black") +  # Ajouter les pourcentages au-dessus des barres
      labs(title = "Répartition des traits de personnalité dans les répondants", x = "   ", y = "  ", fill = "Trait de personnalité :") + 
      theme_minimal() +  # Thème minimaliste
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank()) + # Pour améliorer la lisibilité de l'axe X
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,60))
  })

########################################### Boxplot interactif stress ############################################################  
  

  
###############################################################################################################################
########################################## Sous-onglet: Santé physique, habitudes #############################################  
###############################################################################################################################

  ########################################################## Texte premier graphique #################################################
  
  output$texte_sante_physique <- renderUI({
    if (input$radio == 'Santé physique et habitudes quotidiennes') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Les données montrent que les femmes déclarent davantage de problèmes de santé que les autres genres, ce qui peut refléter une vulnérabilité plus marquée.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>On observe une tendance selon laquelle les problèmes de santé augmentent avec l’âge, suggérant que les étudiants plus âgés sont davantage concernés par ce type de difficultés.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>La classe aisée semble avoir moins de problèmes de santé, ce qui peut être lié à un meilleur accès aux soins de santé, à une alimentation plus équilibrée et à un mode de vie globalement plus favorable.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Les individus avec un trait de personnalité marqué par l’agréabilité semblent avoir moins de soucis de santé, ce qui pourrait être lié à des comportements plus positifs et moins stressants dans leur quotidien.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Ce sont les individus ayant un Bac +5 qui déclarent avoir le plus de soucis de santé, ce qui pourrait être lié à un stress élevé associé à des études avancées.
</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>Une grande majorité des individus de notre base de données ne déclarent aucun problème de santé, ce qui suggère un bon état de santé global au sein de l’échantillon.
</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ########################################################## Texte second graphique #################################################
  output$texte_sante_physique_graph2 <- renderUI({
    if (input$radio == 'Santé physique et habitudes quotidiennes') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>En moyenne, les hommes déclarent pratiquer davantage de sport que les femmes, mettant en évidence une différence de niveau d’activité physique selon le genre.</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>En moyenne, les étudiants âgés de 21 ans déclarent pratiquer davantage d’activité physique, tandis que ceux âgés de 19 ans sont ceux qui en font le moins.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>La classe aisée est celle qui pratique le plus de sport, ce qui pourrait refléter un accès plus facile à des infrastructures sportives ou à du temps libre pour l’activité physique.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Les individus avec un trait de personnalité extravertie font en moyenne plus de sport hebdomadaire, ce qui peut être dû à leur sociabilité et à leur tendance à participer à des activités physiques en groupe ou à rechercher des environnements dynamiques.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Les individus ayant un Bac +5 font en moyenne plus de sport, ce qui pourrait être lié à une meilleure gestion du temps ou à une plus grande priorité accordée à la santé et au bien-être après plusieurs années d'études intensives.
</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>En moyenne, les étudiants pratiquent une activité physique au moins une fois par semaine, bien que certains atteignent jusqu’à 6 fois par semaines, témoignant d’une hétérogénéité des habitudes d’activité physique dans la population.
 </p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ########################################################## Texte troisième graphique #################################################
  
  output$texte_sante_physique_graph3 <- renderUI({
    if (input$radio == 'Santé physique et habitudes quotidiennes') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Une proportion légèrement plus élevée d’hommes déclare avoir de mauvaises habitudes alimentaires, bien que cette différence ne soit pas particulièrement flagrante.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Les étudiants de 23 ans et de plus de 24 ans semblent avoir les habitudes alimentaires les moins saines, comparativement aux autres groupes d’âge.</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>La classe populaire semble avoir les meilleures habitudes alimentaires, ce qui pourrait être dû à des choix alimentaires plus simples et peut-être à un moindre accès à des produits transformés ou coûteux.</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Les individus ayant le trait de personnalité d'agréabilité semblent avoir de moins bonnes habitudes alimentaires, ce qui pourrait être lié à une tendance à rechercher l'harmonie sociale, parfois au détriment de choix alimentaires sains.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Les individus ayant un Bac +5 semblent avoir des habitudes alimentaires plus saines, ce qui peut être dû à une meilleure connaissance des enjeux nutritionnels ou à un mode de vie plus stable et organisé après des années d'études.
</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>Les habitudes alimentaires des étudiants apparaissent globalement neutres, sans tendance marquée ni vers une alimentation particulièrement saine, ni vers des comportements alimentaires mauvais.
 </p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })

########################################################## Texte heatmap graphique #################################################
  output$texte_sante_physique_heatmap <- renderUI({
    if (input$radio == 'Santé physique et habitudes quotidiennes') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'> En majorité, tant les hommes que les femmes déclarent participer rarement à des pratiques culturelles, indiquant un faible engagement dans ce type d’activités pour les deux genres.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'> Les étudiants de 20 ans sont ceux qui participent le moins fréquemment à des pratiques culturelles par rapport aux autres tranches d’âge.</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>La classe moyenne participe rarement à des pratiques culturelles, ce qui peut indiquer un manque de temps ou d'accès à des activités culturelles.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'> Les individus ayant le trait de personnalité d'agréabilité participent rarement à des pratiques culturelles, ce qui pourrait être dû à une préférence pour des activités sociales plus orientées vers la convivialité et l'interaction avec les autres plutôt qu'à des engagements culturels individuels.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'> En majorité, les individus ayant un Bac +4 participent rarement à des activités culturelles, ce qui pourrait être lié à des emplois du temps chargés.
</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'> Les étudiants déclarent pratiquer rarement des activités culturelles, ce qui suggère une faible fréquence d’engagement dans ce type de loisirs au sein de l’échantillon.
</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ########################################################## Texte cinq graphique #################################################
  
  output$texte_sante_physique_graph5 <- renderUI({
    if (input$radio == 'Santé physique et habitudes quotidiennes') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'> Les personnes non binaires se démarquent par un temps d’écran moyen plus élevé que celui observé chez les hommes et les femmes, suggérant une utilisation plus intensive des écrans, ou un biais lié au peu de données collectées sur les personnes non-binaires.</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Une grande majorité des étudiants déclarent un temps d’écran quotidien avoisinant les 5 heures, ce qui reflète une exposition numérique importante dans leur routine quotidienne.
 </p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'> La classe populaire ne dépasse généralement pas 9 à 10 heures de temps d'écran par jour, ce qui pourrait être lié à un accès limité à des appareils numériques.
 </p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'> On observe un léger pic du temps d'écran à 10 heures chez les individus ayant un trait de personnalité extravertie, ce qui est très élevé et pourrait indiquer une forte consommation de médias sociaux ou d'activités en ligne.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'> On observe une densité élevée parmi les Bac +5 ayant un temps d'écran moyen supérieur à 10 heures par jour, ce qui pourrait être lié à des exigences professionnelles ou académiques.
</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'> Une grande majorité des étudiants déclarent un temps d’écran quotidien avoisinant les 5 heures, ce qui reflète une exposition numérique importante dans leur routine quotidienne.</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })

  ########################################################## Texte six graphique #################################################
  
  output$texte_sante_physique_graph6 <- renderUI({
    if (input$radio == 'Santé physique et habitudes quotidiennes') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Pour chacun des genres, le temps de sommeil semble se concentrer autour de 7 heures par nuit, indiquant une tendance générale à un sommeil modéré au sein de l’échantillon. Ce constat est loin de correspondre aux recommendations médiacale qui suggèrent que les femmes doivent dormir 2 heures de plus en moyenne que les hommes.</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Le temps de sommeil des étudiants se concentre majoritairement entre 7 et 8 heures par nuit, ce qui correspond aux recommandations habituelles pour un repos optimal.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'> Certains individus de la classe populaire dorment moins de 4 heures par nuit, ce qui pourrait être dû à des horaires de travail contraignants ou à des conditions de vie stressantes.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'> On observe une densité assez élevée chez les individus ayant le trait de personnalité soucieux, avec un temps de sommeil de 5 heures ou moins, ce qui pourrait indiquer que ces individus, malgré leur souci de bien faire, peuvent souffrir de stress.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'> Bien que faible, la proportion de Bac +4 ayant un temps de sommeil inférieur à 5 heures reste plus élevée comparée aux autres niveaux d’études.
</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'> Le temps de sommeil des étudiants se concentre majoritairement entre 7 et 8 heures par nuit, ce qui correspond aux recommandations habituelles pour un repos optimal. </p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
########################################### Bar chart soucis de santé ############################################################  
  
  output$pb_sante <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      data_counts <- rep %>%
        group_by(Soucis_sante) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Soucis_sante, y = percentage, fill = Soucis_sante)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") +  
        labs(title = "Répartition des soucis de santé dans la population", 
             x = "Soucis de santé", y = NULL, fill = "Soucis de santé") + 
        theme_minimal() +
        theme(legend.position = "none",
              panel.grid.major = element_line(color = "grey40", size = 0.5),
              panel.grid.minor = element_line(color = "grey70", size = 0.25))+
        scale_y_continuous(labels = scales::percent_format(scale = 1),
                           breaks = seq(0,100, 10))
    } else {
      data_counts <- rep %>%
        group_by(Soucis_sante, .data[[variable]]) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>% 
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Soucis_sante, y = percentage, fill = .data[[variable]])) + 
        geom_bar(stat = "identity", position = "dodge", color = "black") +  
        labs(title = paste("Répartition des soucis de santé par", variable), 
             x = "Soucis de santé", y = NULL, fill = variable) + 
        theme_minimal() +
        theme(legend.position = "none",
              panel.grid.major = element_line(color = "grey40", size = 0.5),  # Grille principale plus foncée
              panel.grid.minor = element_line(color = "grey70", size = 0.25)) +
        scale_fill_manual(values = palette)+
        scale_y_continuous(labels = scales::percent_format(scale = 1),
                           breaks = seq(0,max(data_counts$percentage)+5, 10))
    }
  })
 
########################################### Boxplot nb jour de sport ############################################################  
  output$jour_sport <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      ggplot(rep, aes(y = Nb_jour_sport_par_semaine, fill = palette)) +
        geom_boxplot(outlier.colour = "black", outlier.size = 2) +  
        labs(title = "Nombre de jours de sport par semaine", 
             x = "Nombre de jours de sport", y = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), 
              legend.position = "none")+
        scale_fill_manual(values = palette)
    } else {
      ggplot(rep, aes(x = get(variable), y = Nb_jour_sport_par_semaine, fill = get(variable))) +
        geom_boxplot(outlier.colour = "black", outlier.size = 2) +  
        labs(title = paste("Nombre de jours de sport par semaine \n en fonction du", variable), 
             x = variable, y = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              legend.position = "none")+
        scale_fill_manual(values = palette)
    }
  })
  
########################################### Bar chart alimentation ############################################################  
  
  
  output$alimentation <- renderPlot({
    variable <- input$checkGroup
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      data_counts <- rep %>%
        group_by(Habitudes_alimentaires) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Habitudes_alimentaires, y = percentage)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") +  
        labs(title = "Répartition des habitudes alimentaires \n dans la population", 
             x = "Habitudes alimentaires", y = NULL, fill = "Habitudes alimentaires") + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_y_continuous(
          labels = scales::percent_format(scale = 1),
          breaks = seq(0, 100, 10)
        )
      
    } else {
      data_counts <- rep %>%
        group_by(Habitudes_alimentaires, .data[[variable]]) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>% 
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Habitudes_alimentaires, y = percentage, fill = .data[[variable]])) + 
        geom_bar(stat = "identity", position = "dodge", color = "black")  +  
        labs(title = paste("Répartition des habitudes alimentaires par", variable), 
             x = " Habitudes Alimentaires ", y = NULL, fill = variable) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_fill_manual(values = palette) +
        scale_y_continuous(
          labels = scales::percent_format(scale = 1),
          breaks = seq(0, 100, 10)
        )
    }
  })

########################################### Heatmap pratiques culturelles ############################################################  
  
  output$pratiques_culturelles <- renderPlot({
    variable <- input$checkGroup
    
    if (variable == "Tous") {
      data_counts <- rep %>%
        group_by(Pratiques_activites_culturelles) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Pratiques_activites_culturelles, y = percentage)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
        geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                  vjust = -0.5, size = 6, color = "black") +  # Ajouter les pourcentages au-dessus des barres
        labs(title = "Fréquence des pratiques culturelles dans la population", x = "   ", y = "  ", fill = "Trait de personnalité :") + 
        theme_minimal() +  # Thème minimaliste
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_blank(),
              legend.position = "none",
              axis.ticks.y = element_blank()) + # Pour améliorer la lisibilité de l'axe X
        ylim(0,50)
    } else {
        data_counts <- rep %>%
          group_by(.data[[variable]], Pratiques_activites_culturelles) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(.data[[variable]]) %>%  # regroupement par modalité
          mutate(percentage = count / sum(count) * 100) %>%
          ungroup()
        
        ggplot(data_counts, aes(x = Pratiques_activites_culturelles, y = .data[[variable]], fill = percentage)) +
          geom_tile(color = "black") +  
          geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black", size = 5) +
          scale_fill_gradient(low = "white", high = "grey40") +  
          labs(title = paste("Fréquence des pratiques culturelles selon le", variable), 
               x = "Pratiques culturelles", y = variable, fill = "Part des réponses") + 
          theme_minimal() +
          theme(legend.position = "none")
    }
  })
  
########################################### Hist area temps_ecran ############################################################  
  
  output$temps_ecran <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      # Histogramme lorsque "Tous" est sélectionné
      ggplot(rep, aes(x = Temps_ecran_moyen_par_jour, fill = palette)) + 
        geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +  # Histogramme
        labs(title = "Distribution du temps d'écran moyen par jour dans la population", subtitle = "(en heures)", 
             x = "   ", 
             y = "  ", fill = "Temps d'écran") + 
        theme_minimal() +
        scale_fill_manual(values = palette) +
        theme(legend.position = "none")
    } else {
      # Densité empilée si une variable spécifique est choisie
      ggplot(rep, aes(x = Temps_ecran_moyen_par_jour, fill = get(variable))) + 
        geom_density(alpha = 0.7, position = "stack") +  
        labs(title = paste("Distribution du temps d'écran moyen par jour selon le ", variable), 
             x = "  ",
             subtitle = "(En heures)",
             y = "   ", fill = variable) + 
        theme_minimal() +
        scale_fill_manual(values = palette)
    }
  })
  
 
########################################### Hist area temps_sommeil ############################################################  
  
  output$temps_sommeil <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      # Histogramme lorsque "Tous" est sélectionné
      ggplot(rep, aes(x = Temps_moy_sommeil, fill = Temps_moy_sommeil)) + 
        geom_histogram(binwidth = 0.5,color = "black" ,fill = "lightblue", alpha = 0.7) +  # Histogramme
        labs(title = "Distribution du temps de sommeil moyen par nuit dans la population", subtitle = "(en heures)", 
             x = " ", 
             y = "  ", fill = "Temps de sommeil") + 
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      # Densité empilée si une variable spécifique est choisie
      ggplot(rep, aes(x = Temps_moy_sommeil, fill = get(variable))) + 
        geom_density(alpha = 0.7, position = "stack") +  
        labs(title = paste("Distribution du temps de sommeil moyen par nuit selon le ", variable), 
             x = "   ", 
             subtitle = "(en heures)",
             y = "   ", fill = variable) + 
        theme_minimal() +
        theme(legend.position = "none") +
        scale_fill_manual(values = palette)
    }
  })
  
  
  
###############################################################################################################################
##################################### Sous-onglet: Santé mentale, familiale et financière #####################################  
###############################################################################################################################
  
  ##########################################Texte santé mental graphique 1##############################################################
  
  output$texte_sante_mental_1 <- renderUI({
    if (input$radio == 'Santé mentale, vie familiale et finances') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Le niveau de confiance en soi chez les femmes est plus dispersé, ce qui signifie que les réponses varient davantage entre les femmes.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Les étudiants de 23 ans semblent avoir davantage confiance en eux, ce qui peut s’expliquer par une plus grande maturité et une meilleure connaissance de leurs capacités à ce stade de leur parcours.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>Les étudiants de classe populaire ont en moyenne légèrement moins confiance en eux, ce qui pourrait être dû à des facteurs sociaux ou économiques influençant leur perception de leurs capacités et de leurs opportunités.</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Ce sont les étudiants ayant le trait de personnalité 'ouverture' qui semblent avoir le plus confiance en eux, ce qui peut être lié à leur curiosité et leur capacité à s'adapter à de nouvelles situations, renforçant ainsi leur assurance.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'> Les étudiants ayant un Bac +5 semblent avoir plus confiance en eux, ce qui peut être lié à une plus grande maturité.
</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>En moyenne, sur une échelle de 1 à 10, le niveau de confiance des étudiants est proche de 6, ce qui indique une confiance modérée en eux-mêmes et en leurs capacités. </p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ##########################################Texte santé mental graphique 2##############################################################
  
  
  output$texte_sante_mental_2 <- renderUI({
    if (input$radio == 'Santé mentale, vie familiale et finances') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>En moyenne, le niveau d’exigence personnelle est plus élevé chez les femmes, ce qui peut refléter une pression accrue qu’elles se mettent pour réussir.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Les étudiants de 23 ans semblent avoir une exigence personnelle plus faible, ce qui pourrait être dû à un équilibre plus serein entre les attentes académiques et les attentes des étudiants.</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'> Les étudiants de classe populaire ont en moyenne moins d'exigence personnelle, ce qui pourrait être lié à des ressources limitées.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Les étudiants ayant le trait de personnalité 'ouverture' semblent avoir un niveau d'exigence plus bas, ce qui pourrait être dû à leur capacité à accepter la flexibilité et à explorer différentes options sans se mettre trop de pression.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Les étudiants de Bac +3 et Bac +5 ont un niveau d'exigence personnelle plus élevé, ce qui peut être dû à des attentes académiques plus importantes et à la pression de réussir dans des parcours avancés.</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>En moyenne, le niveau d'exigence des étudiants est de 8 sur 10, ce qui indique qu'ils se fixent des attentes relativement élevées pour eux-mêmes.
</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ##########################################Texte santé mental graphique 3##############################################################
  
  
  output$texte_sante_mental_3 <- renderUI({
    if (input$radio == 'Santé mentale, vie familiale et finances') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Une plus grande proportion de femmes déclare avoir peur de l’échec, ce qui peut être lié à une pression plus forte ressentie dans le cadre académique ou personnel.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Les étudiants de plus de 24 ans ou de moins de 17 ans semblent avoir une plus grande peur de l'échec, ce qui pourrait être lié à des attentes sociales ou académiques spécifiques.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>Les étudiants de classe populaire sont plus nombreux à avoir peur de l'échec, ce qui peut être dû à des pressions sociales ou économiques accrues.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Les étudiants ayant le trait de personnalité 'soucieux' semblent avoir le plus peur de l'échec, ce qui peut être lié à leur tendance à anticiper les problèmes et à se préoccuper des conséquences négatives</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>La peur de l'échec semble diminuer à mesure que le niveau d'étude augmente, ce qui pourrait être dû à une plus grande confiance en soi et une meilleure gestion du stress au fur et à mesure des progrès académiques.</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>Une forte majorité des étudiants semble avoir peur de l'échec, ce qui peut être attribué aux pressions académiques, aux exigences de notre formation.
</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ##########################################Texte santé mental graphique 4##############################################################
  
  
  output$texte_sante_mental_4 <- renderUI({
    if (input$radio == 'Santé mentale, vie familiale et finances') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>20 % des femmes déclarent se comparer tout le temps aux autres, ce qui peut refléter un sentiment d’insécurité ou une forte pression sociale.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Les étudiants de 19 ans seraient ceux qui se comparent le plus souvent aux autres, ce qui peut être lié à une période de transition. Entre les attentes et la vie lycéenne comparées à celles de l'université.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>Les étudiants de classe aisée se comparent souvent aux autres, ce qui peut être lié à des attentes sociales élevées et à un environnement compétitif.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Une grande majorité des étudiants ayant le trait de personnalité 'extraversion' se compare tout le temps aux autres, ce qui peut être lié à leur besoin de se valoriser dans des interactions sociales.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Les étudiants en début d’études ont plus tendance à se comparer aux autres, ce qui peut être lié à une période d’adaptation où ils cherchent à s'affirmer et à se situer par rapport à leurs pairs.</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'> La majorité des étudiants se compare souvent aux autres, ce qui peut être lié à la pression sociale et académique.</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ##########################################Texte santé mental graphique 5##############################################################
  
  
  output$texte_sante_mental_5 <- renderUI({
    if (input$radio == 'Santé mentale, vie familiale et finances') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Les personnes non binaires déclarent ne pas se sentir suffisamment entourées, ce qui peut refléter un isolement social ou un manque de soutien adapté à leur situation.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Les étudiants plus jeunes (moins de 21 ans) se sentent globalement très bien entourés, ce qui peut refléter un soutien familial et social fort durant ces années de transition vers l'indépendance.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>Les étudiants de classe populaire se sentent majoritairement mal entourés, ce qui pourrait être lié à un manque de soutien social ou à des difficultés à accéder à des réseaux de soutien adaptés à leurs besoins.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Les étudiants ayant le trait de personnalité 'extraversion' se sentent très bien entourés, ce qui est logique étant donné leur sociabilité.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Les étudiants de Bac +1 et Bac +3 se sentent insuffisamment entourés, ce qui pourrait être dû à des périodes de transition où ils manquent de soutien social ou ont du mal à établir des liens solides au début de leur parcours universitaire.
</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>Une grande majorité des étudiants se sentent bien entourés, ce qui suggère un solide réseau de soutien social, qu'il soit familial, amical ou académique.</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ##########################################Texte santé mental graphique 6##############################################################
  
  
  output$texte_sante_mental_6 <- renderUI({
    if (input$radio == 'Santé mentale, vie familiale et finances') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Il y a légèrement plus de femmes que d’hommes qui vivent seules, ce qui peut refléter une plus grande autonomie ou des choix de vie différents.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Une grande majorité des étudiants de 24 ans ou plus vivent seuls, ce qui peut être lié à une plus grande autonomie.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>La répartition des situations de vie seule est assez homogène entre les différentes classes sociales, bien que les étudiants des classes ouvrières vivent légèrement moins seuls.</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Les étudiants ayant le trait de personnalité 'conscience' ne vivent majoritairement pas seuls, ce qui pourrait être lié à leur préférence pour la stabilité.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Les étudiants en début d'études supérieures vivent moins seuls, ce qui peut être dû à des choix de logement influencés par des facteurs financiers ou familiaux.</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>Il y a légèrement plus d'étudiants vivant seuls que d'étudiants vivant entourés, ce qui pourrait refléter une tendance à l'indépendance.
</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ##########################################Texte santé mental graphique 7##############################################################
  
  
  output$texte_sante_mental_7 <- renderUI({
    if (input$radio == 'Santé mentale, vie familiale et finances') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Les hommes et les femmes estiment majoritairement que les aides financières sont suffisantes, ce qui suggère un ressenti globalement positif vis-à-vis du soutien économique reçu.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Les étudiants de 21 ans estiment en majorité que les aides financières sont suffisantes, ce qui peut refléter un équilibre entre leurs besoins et le soutien disponible à ce stade de leurs études.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>Les étudiants de classe moyenne trouvent globalement les aides financières suffisantes, ce qui suggère que ces aides couvrent bien leurs besoins de manière équilibrée à ce niveau socio-économique.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Une majorité des étudiants ayant le trait de personnalité 'agréabilité' jugent leurs aides financières suffisantes, ce qui peut refléter une gestion équilibrée de leurs ressources.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Les étudiants en Bac +4 trouvent majoritairement que les aides financières sont suffisantes, ce qui peut refléter un équilibre entre leurs besoins financiers et le soutien dont ils disposent à ce stade de leurs études.</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'> Une majorité des étudiants considère que les aides financières sont suffisantes, ce qui indique que la plupart parviennent à gérer leurs dépenses avec les soutiens disponibles. Néanmoins, le questionnaire ne précisait pas si l'aspect financier concerné était les aides familiales, les bourses... Ce qui peut apporter un biai lors de la lecture de la question.
</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })
  
  ##########################################Texte santé mental graphique 8##############################################################
  
  
  output$texte_sante_mental_8 <- renderUI({
    if (input$radio == 'Santé mentale, vie familiale et finances') {
      # Condition dynamique pour afficher le texte en fonction des choix (genre, âge, etc.)
      if (input$checkGroup == "Genre") {
        HTML("<p style='font-size: 13px;'>Pour la majorité des hommes et des femmes, il est rare qu’ils renoncent à des visites familiales, ce qui montre l’importance qu’ils accordent aux liens familiaux malgré les contraintes de la vie étudiante.
</p>")
      } else if (input$checkGroup == "Age") {
        HTML("<p style='font-size: 13px;'>Les étudiants de 19 ans estiment généralement qu'il n'y a pas de renoncement aux visites familiales, ce qui indique que les liens familiaux restent importants.
</p>")
      } else if (input$checkGroup == "Statut_social_famille") {
        HTML("<p style='font-size: 13px;'>Les étudiants de la classe moyenne estiment qu'ils renoncent peu à visiter leur famille, ce qui indique que les liens familiaux restent importants.
</p>")
      } else if (input$checkGroup == "Trait_personnalité") {
        HTML("<p style='font-size: 13px;'>Les étudiants ayant le trait de personnalité 'agréabilité' renoncent rarement à visiter leur famille, ce qui peut être dû à leur désir de maintenir des liens forts.
</p>")
      } else if (input$checkGroup == "Niveau_etude") {
        HTML("<p style='font-size: 13px;'>Les étudiants en Bac +4 renoncent souvent à rendre visite à leur famille, ce qui pourrait être dû à une charge de travail accrue.</p>")
      } else if (input$checkGroup == "Tous") {
        HTML("<p style='font-size: 13px;'>Une majorité d'étudiants renonce rarement, peu ou pas à rendre visite à leur famille, ce qui montre qu’ils gardent un certain lien social malgré la vie étudiante.
</p>")
      } else {
        HTML("<p style='font-size: 13px;'>Aucune information disponible pour la catégorie sélectionnée.</p>")
      }
    }
  })  
  
  
  
  output$niv_conf <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    # Vérifier si "Tous" est sélectionné
    if (variable == "Tous") {
      ggplot(rep, aes(y = Niveau_confiance_en_soi)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = "Distribution du niveau de confiance \nen soi dans la population", 
             x = NULL, y = NULL) +
        theme_minimal() + theme(axis.text.x = element_blank())
    } else {
      ggplot(rep, aes(x = get(variable), y = Niveau_confiance_en_soi, fill = get(variable))) +
        geom_boxplot() +
        labs(title = paste("Niveau de confiance en soi selon \n le", variable), 
             x = "  ", y = "  ", fill = variable) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_blank()) +
        scale_fill_manual(values = palette)
    }
  })
  
  
  output$exigence_perso <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      ggplot(rep, aes(y = Niveau_exigence_perso)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = "Distribution du Niveau d'exigence \n personnelle dans la population", x = "   ", y = "   ") +
        theme_minimal() + theme(axis.text.x = element_blank())
    } else {
      ggplot(rep, aes(x = .data[[variable]], y = Niveau_exigence_perso, fill = .data[[variable]])) +
        geom_boxplot() +
        labs(title = paste("Niveau d'exigence personnelle \n selon", variable), x = NULL , y = NULL) +
        theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_blank()) +
        scale_fill_manual(values = palette)
    }
  })
  
  
  output$peur_echec <- renderPlot({
    variable <- input$checkGroup
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      data_counts <- rep %>%
        group_by(Peur_echec) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Peur_echec, y = percentage)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") + 
        labs(title = "Répartition peur de l'échec dans la population", 
             x = "  ", y = NULL) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_y_continuous(
          labels = scales::percent_format(scale = 1),
          breaks = seq(0, 100, 10)
        )
      
    } else {
      data_counts <- rep %>%
        group_by(Peur_echec, .data[[variable]]) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>% 
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Peur_echec, y = percentage, fill = .data[[variable]])) + 
        geom_bar(stat = "identity", position = "dodge", color = "black") +  
        labs(title = paste("Répartition peur de l'échec par", variable), 
             x = "  ", y = NULL, fill = variable) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_fill_manual(values = palette) +
        scale_y_continuous(
          labels = scales::percent_format(scale = 1),
          breaks = seq(0, 100, 10)
        )
    }
  })
  
  
  
  output$comparaison_autres <- renderPlot({
    variable <- input$checkGroup
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      data_counts <- rep %>%
        group_by(Comparaison_aux_autres) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Comparaison_aux_autres, y = percentage)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue")+  
        labs(title = "Répartition des comparaisons aux autres \n dans la population", 
             x = "  ", y = NULL) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, 10))
      
    } else {
      data_counts <- rep %>%
        group_by(Comparaison_aux_autres, .data[[variable]]) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>% 
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Comparaison_aux_autres, y = percentage, fill = .data[[variable]])) + 
        geom_bar(stat = "identity", position = "dodge", color = "black") +  
        labs(title = paste("Répartition des comparaisons aux autres par", variable), 
             x = "  ", y = NULL, fill = variable) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_fill_manual(values = palette) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, 10))
    }
  })
  
  
  
  output$sentiment_entourage <- renderPlot({
    variable <- input$checkGroup
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      data_counts <- rep %>%
        group_by(Sentiment_etre_entoure) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Sentiment_etre_entoure, y = percentage)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") +  
        labs(title = "Répartition du sentiment d'être entouré dans la population", 
             x = "  ", y = NULL) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, 10))
      
    } else {
      data_counts <- rep %>%
        group_by(Sentiment_etre_entoure, .data[[variable]]) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>% 
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Sentiment_etre_entoure, y = percentage, fill = .data[[variable]])) + 
        geom_bar(stat = "identity", position = "dodge", color = "black") +  
        labs(title = paste("Répartition du sentiment d'être entouré par", variable), 
             x = NULL , y = NULL, fill = variable) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_fill_manual(values = palette) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, 10))
    }
  })
  
  
  output$vie_seul <- renderPlot({
    variable <- input$checkGroup
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      data_counts <- rep %>%
        group_by(Vie_seul) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Vie_seul, y = percentage)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") +  
        labs(title = "Répartition de la situation de vie seule dans la population", 
             x = NULL , y = NULL) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, 10))
      
    } else {
      data_counts <- rep %>%
        group_by(Vie_seul, .data[[variable]]) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>% 
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Vie_seul, y = percentage, fill = .data[[variable]])) + 
        geom_bar(stat = "identity", position = "dodge", color = "black") +  
        labs(title = paste("Situation de vie seule en fonction de", variable), 
             x = NULL, y = NULL, fill = variable) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey40", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25)
        ) +
        scale_fill_manual(values = palette) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, 10))
    }
  })
  
  
  
  output$visite_famille <- renderPlot({
    variable <- input$checkGroup
    
    if (variable == "Tous") {
      famille_counts <- rep %>%
        group_by(Renoncement_visite_famille) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100)  # Calcul des pourcentages
      
      # Création du graphique à barres avec les pourcentages au-dessus des barres
      ggplot(famille_counts, aes(x = Renoncement_visite_famille, y = count)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
        labs(title = "Répartition des non-visites à la famille dans les répondants", x = "   ", y = "  ", fill = "Trait de personnalité :") + 
        theme_minimal() +  # Thème minimaliste
        theme(legend.position = "none",
              panel.grid.major = element_line(color = "grey40", size = 0.5),
              panel.grid.minor = element_line(color = "grey70", size = 0.25)) + # Pour améliorer la lisibilité de l'axe X
        ylim(0,80) +scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0,100,10))
    } else {
      data_counts <- rep %>%
        group_by(.data[[variable]], Renoncement_visite_famille) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>%  # Pour que chaque ligne fasse 100 %
        mutate(percentage = count / sum(count) * 100) %>%
        ungroup()
      
      ggplot(data_counts, aes(x = Renoncement_visite_famille, y = .data[[variable]], fill = percentage)) +
        geom_tile(color = "black") + 
        geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black", size = 5) +
        scale_fill_gradient(low = "white", high = "grey55") +
        labs(title = paste("Carte de chaleur du renoncement à la visite \nfamiliale par", variable), 
             x = NULL, y = NULL, fill = "Part des réponses") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    }
  })
  
  
  output$stabilite_finance <- renderPlot({
    variable <- input$checkGroup
    
    if (variable == "Tous") {
      finance_counts <- rep %>%
        group_by(Satisfaction_aides_financieres) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100)  # Calcul des pourcentages
      
      # Création du graphique à barres avec les pourcentages au-dessus des barres
      ggplot(finance_counts, aes(x = Satisfaction_aides_financieres, y = count)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
        labs(title = "Répartition des satisfactions financières \ndans la population", x = "   ", y = "  ", fill = "Trait de personnalité :") + 
        theme_minimal() +  # Thème minimaliste
        theme(legend.position = "none",
              panel.grid.major = element_line(color = "grey40", size = 0.5),
              panel.grid.minor = element_line(color = "grey70", size = 0.25)) + # Pour améliorer la lisibilité de l'axe X
        ylim(0,80) +scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0,100,10))
    } else {
      data_counts <- rep %>%
        group_by(.data[[variable]], Satisfaction_aides_financieres) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>%  # Pour un total de 100 % par ligne
        mutate(percentage = count / sum(count) * 100) %>%
        ungroup()
      
      ggplot(data_counts, aes(x = Satisfaction_aides_financieres, y = .data[[variable]], fill = percentage)) +
        geom_tile(color = "black") + 
        geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black", size = 5) +
        scale_fill_gradient(low = "white", high = "grey55") +
        labs(title = paste("Carte de chaleur de la satisfaction des \naides financières par", variable), 
             x = NULL, y = NULL, fill = "Part des réponses") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    }
  })
  
  

###############################################################################################################################
########################################## Sous-onglet: vie etudiante  #############################################  
###############################################################################################################################

  ############################################ Texte graphique 1 #########################################################
  
  
  output$texte_vie_etudiante_1 <- renderUI({
    if (input$radio == 'Vie étudiante') {
      if (input$checkGroup == "Genre") {
        "Le niveau de pression académique semble plus élevé chez les femmes, ce qui pourrait être lié à des attentes sociales ou personnelles accrues."
      } else if (input$checkGroup == "Age") {
        "Les étudiants de 23 ans semblent avoir un niveau de pression académique plus faible en moyenne, ce qui pourrait être lié à une meilleure gestion de leurs études.
"
      } else if (input$checkGroup == "Statut_social_famille") {
        "La classe populaire semble avoir en moyenne un niveau de pression académique plus élevé, ce qui pourrait être lié à des défis financiers supplémentaires."
      } else if (input$checkGroup == "Trait_personnalité") {
        "Les étudiants ayant le trait de personnalité 'extraversion' semblent ressentir moins de pression académique, tandis que ceux ayant le trait de personnalité 'soucieux' en ressentent beaucoup plus.
"
      } else if (input$checkGroup == "Niveau_etude") {
        "Les étudiants en bac +5 semblent ressentir une pression académique plus élevée, ce qui peut s’expliquer par l’enjeu de fin d’études, les exigences des projets de fin de cursus ou encore la préparation à l’insertion professionnelle.
"
      } else if (input$checkGroup == "Tous") {
        "Le niveau de pression académique des étudiants est globalement modéré, avec une moyenne proche de 6 sur 10, ce qui suggère qu'ils ressentent une pression notable, mais qu'elle reste gérable pour la majorité.
"
      } else {
        "Aucune information disponible pour la catégorie sélectionnée."
      }
    }
  })
  
  ############################################ Texte graphique 2 #########################################################
  
  
  output$texte_vie_etudiante_2 <- renderUI({
    if (input$radio == 'Vie étudiante') {
      if (input$checkGroup == "Genre") {
        "La moyenne des heures de cours est beaucoup plus faible pour les étudiants non binaires."
      } else if (input$checkGroup == "Age") {
        "Les étudiants de plus de 24 ans semblent avoir beaucoup plus d'heures de cours, probablement en raison de l'alternance, qui combine études et expérience professionnelle.
"
      } else if (input$checkGroup == "Statut_social_famille") {
        "Le nombre d'heures de cours est réparti de manière assez similaire entre les différentes classes sociales, ce qui suggère que la charge académique n'est pas influencée de manière significative par le statut social des étudiants.
"
      } else if (input$checkGroup == "Trait_personnalité") {
        "La moyenne d’heures de cours est globalement bien répartie entre les différents traits de personnalité, même si les étudiants ayant le trait de 'conscience' semblent en avoir légèrement plus.
"
      } else if (input$checkGroup == "Niveau_etude") {
        "La moyenne des heures de cours est beaucoup plus élevée pour les étudiants en bac +5, ce qui s’explique probablement par le rythme soutenu de l’alternance."
      } else if (input$checkGroup == "Tous") {
        "En moyenne, le nombre d'heures de cours pour l'ensemble des étudiants est de 26 heures, ce qui reflète un emploi du temps relativement chargé.
"
      } else {
        "Aucune information disponible pour la catégorie sélectionnée."
      }
    }
  })
  
  ############################################ Texte graphique 3 #########################################################
  
  
  output$texte_vie_etudiante_3 <- renderUI({
    if (input$radio == 'Vie étudiante') {
      if (input$checkGroup == "Genre") {
        "Les étudiants non binaires sont proportionnellement plus nombreux à avoir un job étudiant, ce qui peut être lié à un besoin accru de soutenir financièrement leurs études.
"
      } else if (input$checkGroup == "Age") {
        "Les étudiants plus âgés semblent être légèrement plus nombreux à avoir un job étudiant, ce qui peut être dû à des besoins financiers plus importants."
      } else if (input$checkGroup == "Statut_social_famille") {
        "Les étudiants de la classe sociale populaire sont plus nombreux à avoir un job étudiant, ce qui peut être dû à un besoin financier plus important."
      } else if (input$checkGroup == "Trait_personnalité") {
        "Les étudiants ayant le trait de personnalité 'soucieux' semblent être plus nombreux à avoir un job étudiant, ce qui peut refléter une volonté de sécuriser leur avenir 
"
      } else if (input$checkGroup == "Niveau_etude") {
        "La proportion d’étudiants ayant un job étudiant est globalement bien répartie selon les niveaux d’études, ce qui suggère que le besoin ou le choix de travailler concerne l’ensemble des étudiants.
"
      } else if (input$checkGroup == "Tous") {
        "Une grande majorité des étudiants n'ont pas de job étudiant, ce qui peut indiquer qu'ils privilégient leurs études.
"
      } else {
        "Aucune information disponible pour la catégorie sélectionnée."
      }
    }
  })
  
  ############################################ Texte graphique 4 #########################################################
  
  
  output$texte_vie_etudiante_4 <- renderUI({
    if (input$radio == 'Vie étudiante') {
      if (input$checkGroup == "Genre") {
        "Pour les hommes et les femmes, une majorité d'entre eux mettent moins de 30 minutes pour se rendre à la fac, ce qui suggère que la plupart bénéficient d'une proximité géographique.
"
      } else if (input$checkGroup == "Age") {
        "Les étudiants de 20 ans mettent en majorité moins de 30 minutes pour se rendre à la fac, ce qui peut indiquer qu'ils vivent généralement près de leur établissement.
"
      } else if (input$checkGroup == "Statut_social_famille") {
        "Une majorité d’étudiants issus de la classe moyenne mettent moins de 30 minutes pour aller à la fac, ce qui peut indiquer qu’ils résident généralement à proximité de leur lieu d’étude.
"
      } else if (input$checkGroup == "Trait_personnalité") {
        "Les étudiants ayant le trait de personnalité 'agréabilité' mettent en général moins de 30 minutes pour aller à la fac, ce qui peut indiquer qu’ils organisent leur quotidien de manière à limiter les contraintes de déplacement.
"
      } else if (input$checkGroup == "Niveau_etude") {
        "Les étudiants en bac +4 ont généralement moins de 30 minutes de trajet pour se rendre à la faculté, ce qui peut indiquer qu’ils vivent majoritairement à proximité du campus.
"
      } else if (input$checkGroup == "Tous") {
        "Une majorité d'étudiants mettent moins de 30 minutes pour se rendre à la fac, ce qui montre que la plupart d'entre eux bénéficient d'une proximité géographique.
"
      } else {
        "Aucune information disponible pour la catégorie sélectionnée."
      }
    }
  })
  output$niv_pres <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      ggplot(rep, aes(y = Niveau_pression_academique, fill = Niveau_pression_academique)) +
        geom_boxplot(fill = "lightblue", color = "black") +
        labs(title = "Ressenti du niveau de pression académique dans la population", 
             y = "  ", x = NULL,fill = "Niveau de pression") +
        theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_blank())
    } else {
      ggplot(rep, aes(x = get(variable), y = Niveau_pression_academique, fill = get(variable))) +
        geom_boxplot() +
        labs(title = paste("Ressenti du niveau de pression académique selon le", variable), 
             y = "   ",x = NULL ,fill = variable) +
        theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_blank())+
        scale_fill_manual(values = palette)
    }
  })
  
    
  output$moy_cours <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      ggplot(rep, aes(y = Moy_heure_cours, fill = Moy_heure_cours)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = "Moyenne des heures de cours dans la population", 
             y = "  ", x = NULL,fill = "Heures de cours") +
        theme_minimal() + theme(axis.text.x = element_blank())
    } else {
      ggplot(rep, aes(x = get(variable), y = Moy_heure_cours, fill = get(variable))) +
        geom_boxplot() +
        labs(title = paste("Moyenne des heures de cours selon le", variable), 
             y = "  ",x = NULL, fill = variable) +
        theme_minimal()+ theme(axis.text.x = element_blank())+
        scale_fill_manual(values = palette)
    }
  })
  
  output$job <- renderPlot({
    variable <- input$checkGroup
    
    palette <- palettes[[variable]]
    
    if (variable == "Tous") {
      data_counts <- rep %>%
        group_by(Job_etudiant) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(percentage = count / sum(count) * 100)
      
      ggplot(data_counts, aes(x = Job_etudiant, y = percentage, fill = Job_etudiant)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue")+  
        labs(title = "Part des personnes ayant un job étudiant dans la popualtion", 
             x = "  ", y = NULL, fill = "Job étudiant") + 
        theme_minimal()+
        theme( panel.grid.major = element_line(color = "grey40", size = 0.5),
               panel.grid.minor = element_line(color = "grey70", size = 0.25))+
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0,100,10))
    } else {
      data_counts <- rep %>%
        group_by(Job_etudiant, .data[[variable]]) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>% 
        mutate(percentage = count / sum(count) * 100) 
      
      ggplot(data_counts, aes(x = Job_etudiant, y = percentage, fill = .data[[variable]])) + 
        geom_bar(stat = "identity", position = "dodge", color = "black") +  
        labs(title = paste("Part des personnes ayant un job étudiant ou non \n en fonction de", variable), 
             x = "  ", y = NULL, fill = variable) + 
        theme_minimal()+
        theme( panel.grid.major = element_line(color = "grey40", size = 0.5),
               panel.grid.minor = element_line(color = "grey70", size = 0.25))+
        scale_fill_manual(values = palette)+
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0,100,10))
    }
  })
  
  output$temps_trajet <- renderPlot({
    variable <- input$checkGroup
    
    if (variable == "Tous") {
      trajet_counts <- rep %>%
        group_by(Temps_trajet_minutes) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100)  # Calcul des pourcentages
      
      # Création du graphique à barres avec les pourcentages au-dessus des barres
      ggplot(trajet_counts, aes(x = Temps_trajet_minutes, y = count)) + 
        geom_bar(stat = "identity", color = "black", fill = "lightblue") +  # Créer les barres
        labs(title = "Répartition du temps de trajet domicile-fac dans les répondants", x = "   ", y = "  ", fill = "Trait de personnalité :") + 
        theme_minimal() +  # Thème minimaliste
        theme( panel.grid.major = element_line(color = "grey40", size = 0.5),
               panel.grid.minor = element_line(color = "grey70", size = 0.25),
              legend.position = "none") + # Pour améliorer la lisibilité de l'axe X
        ylim(0,80)+
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0,100,10))
    } else {
      data_counts <- rep %>%
        group_by(.data[[variable]], Temps_trajet_minutes) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(.data[[variable]]) %>%  # Total à 100 % par modalité
        mutate(percentage = count / sum(count) * 100) %>%
        ungroup()
      
      ggplot(data_counts, aes(x = Temps_trajet_minutes, y = .data[[variable]], fill = percentage)) +
        geom_tile(color = "black") + 
        geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black", size = 5) +
        scale_fill_gradient(low = "white", high = "gray55") +
        labs(title = paste("Carte de chaleur du temps de trajet des étudiants \nen fonction de", variable), 
             x = "Temps de trajet", y = variable, fill = "Part des réponses") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    }
  })
  
  
###############################################################################################################################
##################################################### ONGLET corrélations ######################################################
###############################################################################################################################
  output$mat_corr <- renderPlot({
    # Calcul de la matrice des corrélations
    corr_matrix <- cor(rep %>% select_if(is.numeric), use = "complete.obs")
    
    # Sélectionner uniquement les corrélations avec Niveau_stress
    corr_stress <- as.data.frame(corr_matrix["Niveau_stress", , drop = FALSE])
    
    # Transformer en format long
    corr_stress_long <- reshape2::melt(corr_stress)
    colnames(corr_stress_long) <- c("Variable", "Corrélation")
    
    # Supprimer la ligne où la variable est "Niveau_stress"
    corr_stress_long <- corr_stress_long[corr_stress_long$Variable != "Niveau_stress", ]
    
    # Trier les variables de la plus corrélée positivement à la plus négativement
    corr_stress_long <- corr_stress_long %>%
      arrange(Corrélation) %>%
      mutate(Variable = factor(Variable, levels = Variable))  # fixer l’ordre des facteurs
    
    # Tracer la heatmap
    ggplot(corr_stress_long, aes(x = "", y = Variable, fill = Corrélation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "red", high = "dodgerblue", mid = "white", midpoint = 0) +
      geom_text(aes(label = round(Corrélation, 2)), color = "black", size = 5) +
      labs(title = "Corrélation entre le stress et les autres variables",
           x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none")
    
  })
  
  output$plot1 <- renderPlot({
    ggplot(rep, aes(x = Genre, y = Niveau_stress, fill = Genre)) +
      geom_boxplot(color = "black", fill = "gray")  +
      labs(title = paste("Influence du genre sur le stress"),
           x = NULL , y = "Niveau de stress") +
      theme_minimal()
  })
  
  output$plot2 <- renderPlot({
    ggplot(rep, aes(x = Niveau_pression_academique, y = Niveau_stress)) +
      geom_point(alpha = 0.6, color = "black") +
      labs(title = paste("Relation entre le niveau de pression académique \net le niveau de stress"),
           x = "Niveau de pression académique", y = "Niveau de stress") +
      theme_minimal() +
      geom_smooth(method = "lm", se = FALSE, color = "#E74C3C")
  })
  
  output$plot3 <- renderPlot({
    ggplot(rep, aes(x = Temps_moy_sommeil, y = Niveau_stress)) +
      geom_point(alpha = 0.6, color = "black") +
      labs(title = paste("Relation entre le temps de sommeil et le niveau de stress"),
           x = "Temps de sommeil (en heure)", y = "Niveau de stress") +
      theme_minimal()+
      geom_smooth(method = "lm", se = FALSE, color = "#E74C3C")
  })
  
  output$plot4 <- renderPlot({
    ggplot(rep, aes(x = Niveau_etude, y = Niveau_stress, fill = Niveau_etude)) +
      geom_boxplot(color = "black", fill = "gray")  +
      labs(title = paste("Influence du niveau d'étude sur le stress"),
           x = NULL , y = "Niveau de stress") +
      theme_minimal()
  })
  
  # Explorateur interactif
  output$plot_stress <- renderPlot({
    variable <- input$var_x
    if (is.numeric(rep[[variable]])) {
      p <-  ggplot(rep, aes_string(x = variable, y = "Niveau_stress")) +
        geom_point(alpha = 0.6, color = "#2C3E50") +
        labs(title = paste("Relation entre", variable, "et le niveau de stress"),
             x = variable, y = "Niveau de stress") +
        theme_minimal()
      # Ajout de la régression si le bouton est activé
      if (input$ajout_regression == TRUE) {  
        p <- p + geom_smooth(method = "lm", se = input$conf_int, color = "#E74C3C")
      }
      
      # Calcul du modèle de régression
      model <- lm(as.formula(paste("Niveau_stress ~", variable)), data = rep)
      coeff <- coef(model)  # Coefficients de la régression
      r2 <- summary(model)$r.squared  # R²
      eq <- paste0("y = ", round(coeff[2], 2), "x + ", round(coeff[1], 2)) 
      r2_label <- paste0("R² = ", round(r2, 3))
      
      # Ajout de l'équation si activé
      if (input$eq_droite == TRUE) {
        p <- p + 
          annotate("text", x = max(rep[[variable]], na.rm = TRUE), 
                   y = max(rep$Niveau_stress, na.rm = TRUE) + 0.5, 
                   label = eq, 
                   hjust = 1, vjust = 0, size = 7, color = "black")
      }
      
      # Ajout du R² si activé
      if (input$r2_button == TRUE) {
        p <- p + 
          annotate("text", x = max(rep[[variable]], na.rm = TRUE), 
                   y = max(rep$Niveau_stress, na.rm = TRUE) + 1, 
                   label = r2_label, 
                   hjust = 1, vjust = 0, size = 7, color = "blue")
      }
      
      p
    } else {
      ggplot(rep, aes_string(x = variable, y = "Niveau_stress", fill = variable)) +
        geom_boxplot(color = "black", fill = "gray")  +
        labs(title = paste("Influence de", variable, "sur le stress"),
             x = variable, y = "Niveau de stress") +
        theme_minimal()
    }
  })
  
  observeEvent(input$explain_regression, {
    showModal(
      modalDialog(
        title = "Explication de la régression linéaire",
        p("Une régression linéaire est un modèle statistique permettant de représenter la relation entre une variable dépendante (ici, le niveau de stress) et une variable explicative (par exemple, le nombre d'heures de sommeil)."),
        p("L'équation de la droite de régression est de la forme :"),
        tags$code("y = ax + b"),
        p("où :"),
        tags$ul(
          tags$li("y : la variable dépendante (niveau de stress)"),
          tags$li("x : la variable explicative"),
          tags$li("a : le coefficient directeur (l'impact de x sur y)"),
          tags$li("b : l'ordonnée à l'origine")
        ),
        p("Le R² indique la qualité de l'ajustement du modèle : plus il est proche de 1, plus la relation entre x et y est forte."),
        easyClose = TRUE,
        footer = modalButton("Fermer")
      )
    )
  })
  
###############################################################################################################################
###################################################### ONGLET DONNEES ######################################################### 
###############################################################################################################################
  
  output$table <- DT::renderDT({
    rep
  })
}

###############################################################################################################################
############################################################ UI ################################################################ 
###############################################################################################################################
