library(shiny)
library(plotly)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(leaflet)
library(rsconnect)

# Define server logic required to draw a histogram
function(input, output,session) {
  # importation base de données
  pisa_score <- read.csv("./Data/pisa_final.csv", header = TRUE)
  world <- ne_countries(scale = "medium", returnclass = "sf") # pays au format sf
  
  #### ONGLET 2 : QU'EST CE QUE PISA ?
  # carte des pays participants
  output$carte_pisa <- renderPlot({
    
    
    # Garder seulement les pays PISA 2018
    pisa_map <- pisa_score |> 
      filter(gender == "TOT", year == 2018) |> 
      rename(iso_a3_eh = country)
    
    # garder que les pays présents
    world_pisa <- world |> inner_join(pisa_map, by = "iso_a3_eh")
    
    ggplot() +
      # Pays PISA
      geom_sf(data = world_pisa, fill = "darkblue", color = "white", size = 0.1) +
      # Pays restants (gris clair)
      geom_sf(data = anti_join(world, pisa_map, by = c("iso_a3_eh")), 
              fill = "grey90", color = "white", size = 0.1) +
      coord_sf(ylim = c(-55, 90)) +   # couper Antarctique
      theme_void() +
      labs(title = "Pays participants à PISA - 2018")
    
  })
  ### ONGLET 3 : HISTORIQUE PISA (CÔTÉ SERVER)
  
  # GRAPHIQUE DES COURBES 
  output$pisaPlot <- renderPlotly({ 
    req(input$country_choice, input$matiere_choice, input$gender_choice)
    
    # Base de données
    df <- pisa_score |> mutate(moyenne = (maths + reading + sciences) / 3)
    
    # Filtre Pays
    if (input$country_choice != "all") {
      df <- df |> filter(name_long == input$country_choice)
    }
    
    # Initialisation des variables de contrôle
    afficher_legende <- FALSE
    mode_faceting <- FALSE
    nom_legende <- "Légende"
    
    # Filtrage 
    
    # Double Comparaison
    if (input$matiere_choice == "Comparaison_matiere" && input$gender_choice == "Comparaison_genre") {
      df_plot <- df |>
        filter(gender %in% c("GIRL", "BOY")) |>
        select(year, gender, maths, reading, sciences) |>
        pivot_longer(cols = c(maths, reading, sciences), names_to = "Groupe", values_to = "score") |>
        group_by(year, gender, Groupe) |>
        summarise(score = mean(score, na.rm = TRUE), .groups = "drop")
      mode_faceting <- TRUE
      afficher_legende <- TRUE
      nom_legende <- "Matières"
    }
    # Comparaison des genres uniquement
    else if (input$gender_choice == "Comparaison_genre") {
      df_plot <- df |>
        filter(gender %in% c("GIRL", "BOY")) |>
        mutate(valeur = if(input$matiere_choice == "Globale") { moyenne } else { .data[[input$matiere_choice]] }) |>
        group_by(year, gender) |>
        summarise(score = mean(valeur, na.rm = TRUE), .groups = "drop") |>
        rename(Groupe = gender)
      afficher_legende <- TRUE
      nom_legende <- "Genre"
    } 
    # Comparaison des matières uniquement
    else if (input$matiere_choice == "Comparaison_matiere") {
      df_plot <- df |>
        filter(gender == input$gender_choice) |>
        select(year, maths, reading, sciences) |>
        pivot_longer(cols = c(maths, reading, sciences), names_to = "Groupe", values_to = "score") |>
        group_by(year, Groupe) |>
        summarise(score = mean(score, na.rm = TRUE), .groups = "drop")
      afficher_legende <- TRUE
      nom_legende <- "Matières"
    }
    # Une seule courbe 
    else {
      colonne <- if(input$matiere_choice == "Globale") { "moyenne" } else { input$matiere_choice }
      df_plot <- df |>
        filter(gender == input$gender_choice) |>
        group_by(year) |>
        summarise(score = mean(.data[[colonne]], na.rm = TRUE), .groups = "drop") |>
        mutate(Groupe = if(input$gender_choice == "TOT") input$matiere_choice else input$gender_choice) 
      afficher_legende <- FALSE
    }
    
    validate(need(nrow(df_plot) > 0, "Aucune donnée trouvée pour cette sélection."))
    
    # Titre adaptatif 
    titre_final <- paste("Évolution PISA :", if(input$country_choice == "all") "Monde" else input$country_choice)
    
    # Courbe 
    p <- ggplot(df_plot, aes(x = year, y = score, color = Groupe, group = Groupe,
                             text = paste("Année:", year, "<br>Score:", round(score, 1)))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = seq(2000, 2018, by = 3)) +
      coord_cartesian(ylim = c(350, 575)) + 
      scale_color_manual(name = nom_legende, 
                         values = c("maths"="red", "reading"="orange", "sciences"="darkgreen", 
                                    "Globale"="darkblue", "GIRL"="deeppink", "BOY"="dodgerblue", "TOT"="darkblue"),
                         labels = c("GIRL"="Filles", "BOY"="Garçons", "maths"="Maths", 
                                    "reading"="Lecture", "sciences"="Sciences", "Globale"="Moyenne", "TOT"="Mixte")) +
      theme_minimal() +
      labs(title = titre_final, y = "Score PISA", x = "Année") +
      theme(legend.position = "bottom")
    
    if (mode_faceting) {
      p <- p + facet_wrap(~gender, labeller = as_labeller(c("GIRL" = "FILLES", "BOY" = "GARÇONS")))
    }
    
    ggplotly(p, tooltip = "text") |> layout(showlegend = afficher_legende)
  })
  
  # GRAPHIQUE TOP 10 
  output$top10 <- renderPlotly({
    req(input$annee_choice, input$matiere_choice, input$gender_choice)
    
    # Message d'erreur si mode comparaison activé
    validate(
      need(input$matiere_choice != "Comparaison_matiere", "Le classement n'est pas disponible en mode 'Comparer les matières'."),
      need(input$gender_choice != "Comparaison_genre", "Le classement n'est pas disponible en mode 'Comparer les genres'.")
    )
    
    matiere_map <- c("Globale" = "moy_gen", "maths" = "maths", "reading" = "reading", "sciences" = "sciences")
    matiere <- matiere_map[input$matiere_choice]
    
    df <- pisa_score |>
      filter(year == input$annee_choice, gender == input$gender_choice) |>
      mutate(score = .data[[matiere]]) |>
      filter(!is.na(score)) |>
      slice_max(score, n = 10) |>
      arrange(score)
    
    p <- ggplot(df, aes(x = score, y = reorder(name_long, score),
                        text = paste("Pays :", name_long, "<br>Score :", round(score,1)))) +
      geom_col(fill = "forestgreen") +
      labs(title = paste("Top 10 PISA -", input$annee_choice), x = "Score", y = "") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> layout(showlegend = FALSE)
  })
  
  # GRAPHIQUE FLOP 10 
  output$flop10 <- renderPlotly({
    req(input$annee_choice, input$matiere_choice, input$gender_choice)
    
    # Message d'erreur si mode comparaison activé
    validate(
      need(input$matiere_choice != "Comparaison_matiere", "Le classement n'est pas disponible en mode 'Comparer les matières'."),
      need(input$gender_choice != "Comparaison_genre", "Le classement n'est pas disponible en mode 'Comparer les genres'.")
    )
    
    matiere_map <- c("Globale" = "moy_gen", "maths" = "maths", "reading" = "reading", "sciences" = "sciences")
    matiere <- matiere_map[input$matiere_choice]
    
    df <- pisa_score |>
      filter(year == input$annee_choice, gender == input$gender_choice) |>
      mutate(score = .data[[matiere]]) |>
      filter(!is.na(score)) |>
      slice_min(score, n = 10) |>
      arrange(desc(score))
    
    p <- ggplot(df, aes(x = score, y = reorder(name_long, score),
                        text = paste("Pays :", name_long, "<br>Score :", round(score,1)))) +
      geom_col(fill = "darkred") + 
      labs(title = paste("Flop 10 PISA -", input$annee_choice), x = "Score", y = "") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> layout(showlegend = FALSE)
  })
  
  #### ONGLET 4 : CLUSTERING 
  
  # Pop up explicatif sur le clustering (inspiré du pop up sur la régression dans l'application : https://camille-laignel.shinyapps.io/shiny/ )
  observeEvent(input$popupClustering, {
    showModal(modalDialog(
      title = "Introduction au clustering et à la CAH",
      size = "l", # large
      easyClose = TRUE,
      footer = modalButton("Fermer"),
      
      tags$div(
        # Ligne principale : texte à gauche, image à droite
        style = "display: flex; flex-direction: row; align-items: flex-start; margin-bottom: 15px;",
        
        # Colonne texte
        tags$div(
          style = "flex: 2; padding-right: 15px;",
          tags$p(strong("- Qu’est-ce que le clustering ?")),
          tags$p("Le clustering consiste à regrouper des données similaires. 
               C’est une technique de statistique ou de machine learning non supervisé : 
               on n’a pas besoin de connaître les groupes à l’avance. 
               Exemple : regrouper des élèves selon leur profil de scores PISA."),
          
          tags$p(strong("- Pourquoi faire du clustering ?")),
          tags$p("Pour découvrir des structures ou patterns cachés dans les données et identifier des groupes homogènes.
               Utile pour adapter l’enseignement ou analyser les performances."),
          
          tags$p(strong("- La Classification Ascendante Hiérarchique (CAH)")),
          tags$p("La CAH est une méthode hiérarchique de clustering :
               chaque élève commence dans son propre groupe,
               les groupes les plus proches sont fusionnés progressivement jusqu’à former un seul groupe. On peut représenter ce processus par un arbre (dendrogramme) comme celui ci-contre.")
        ),
        
        # Colonne image
        tags$div(
          style = "flex: 1; text-align: right;",
          tags$img(src = "dendrogramme.png", width = "100%", style = "max-width: 250px; border: 1px solid #ccc;")
        )
      ),
      
      # Texte explicatif sous la ligne principale
      tags$div(
        tags$p(strong("- Comment on décide des groupes ?")),
        tags$p("On mesure la similarité entre les élèves.
             On peut ensuite couper l’arbre à un certain niveau pour obtenir le nombre de groupes souhaité.")
      )
    ))
  })
  
  # Clustering en fonction de k
  cluster_reactive <- reactive({
    req(input$k)
    
    # préparation de la base 
    data_clust <- pisa_score |>
      filter(year == 2018, gender == "TOT") |>
      select(country, maths, reading, sciences,
             GDP, student_teacher_ratio, gov_educ_exp, out_of_school,
             GNI_per_capita, life_expectancy, expected_schooling_years, hdi) |>
      drop_na(maths, reading, sciences)  
    
    # standardisation 
    scores_mat <- data_clust |>
      select(maths, reading, sciences) |>
      scale()
    
    # CAH
    dist_mat <- dist(scores_mat, method = "euclidean")
    cah <- hclust(dist_mat, method = "ward.D2")
    clusters <- cutree(cah, k = input$k)
    
    data_clust$cluster <- as.factor(clusters)
    return(data_clust)
  })
  
  # Carte intéractive
  output$map_cluster <- renderLeaflet({   # Crée une carte interactive qui se met à jour automatiquement
    data <- cluster_reactive()            # Récupère les données avec les clusters recalculés
    world_clust <- world |>
      left_join(data, by = c("iso_a3_eh" = "country"))
    pal <- colorFactor("Set1", domain = world_clust$cluster, na.color = "grey")
    popup_content <- paste0(
      "<strong>", world_clust$name, "</strong><br>",
      "Maths : ", round(world_clust$maths, 1), "<br>",
      "Lecture : ", round(world_clust$reading, 1), "<br>",
      "Sciences : ", round(world_clust$sciences, 1)
    )
    leaflet(world_clust) |>
      addTiles() |>
      addPolygons(
        fillColor   = ~pal(cluster),
        weight      = 1,
        color       = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE),
        popup       = popup_content,
        label       = ~name
      ) |>
      addLegend("bottomleft", pal = pal, values = ~cluster, title = "Cluster")
  })
  
  # Scores PISA moyens par cluster
  output$scores_cluster <- renderPlot({
    data <- cluster_reactive()
    
    # Calcul de la moyenne générale par cluster 
    scores_mean <- data |>
      group_by(cluster) |>
      summarise(score_moyen = mean(c(maths, reading, sciences), na.rm = TRUE))
    
    # Palette de couleurs pour les clusters
    nb_clusters <- length(unique(scores_mean$cluster))  
    if(nb_clusters == 1) colors <- c("red")
    if(nb_clusters == 2) colors <- c("red", "blue")
    if(nb_clusters == 3) colors <- c("red", "blue", "#689d71")
    if(nb_clusters == 4) colors <- c("red", "blue", "#689d71", "purple")
    if(nb_clusters == 5) colors <- c("red", "blue", "#689d71", "purple", "orange")
    if(nb_clusters == 6) colors <- c("red", "blue", "#689d71", "purple", "orange", "brown")
    if(nb_clusters == 7) colors <- c("red", "blue", "#689d71", "purple", "orange", "brown", "pink")
    if(nb_clusters == 8) colors <- c("red", "blue", "#689d71", "purple", "orange", "brown", "pink", "gray")
    names(colors) <- levels(scores_mean$cluster)  
    
    ggplot(scores_mean, aes(x = cluster, y = 1, label = round(score_moyen, 1))) +
      geom_text(aes(color = cluster), size = 10, fontface = "bold") +
      scale_color_manual(values = colors) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )  
  })
  
  # Graphiques
  
  # Boxplot GNI per capita
  output$box_gni <- renderPlot({
    data <- cluster_reactive()
    ggplot(data, aes(x = cluster, y = GNI_per_capita, fill = cluster)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Revenu National Brut \npar habitant (USD)", y = "", x = "") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Bar chart de l'IDH moyen par cluster
  output$bar_hdi <- renderPlot({
    data <- cluster_reactive()
    hdi_mean <- data |>
      group_by(cluster) |>
      summarise(hdi = mean(hdi, na.rm = TRUE))
    ggplot(hdi_mean, aes(x = cluster, y = hdi, fill = cluster)) +
      geom_col() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "IDH \n(moyenne)", y = "", x = "") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Boxplot Dépenses publiques éducation
  output$box_gov <- renderPlot({
    data <- cluster_reactive()
    ggplot(data, aes(x = cluster, y = gov_educ_exp, fill = cluster)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Dépenses publiques éducation \n(% PIB)", y = "", x = "") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Boxplot Années de scolarisation attendues 
  output$box_school <- renderPlot({
    data <- cluster_reactive()
    ggplot(data, aes(x = cluster, y = expected_schooling_years, fill = cluster)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Années de \nscolarisation attendues", y = "", x = "") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Bar chart out_of_school (moyenne par cluster)
  output$bar_out <- renderPlot({
    data <- cluster_reactive()
    out_mean <- data |>
      group_by(cluster) |>
      summarise(out = mean(out_of_school, na.rm = TRUE))
    ggplot(out_mean, aes(x = cluster, y = out, fill = cluster)) +
      geom_col() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Enfants non scolarisés \n(% , moyenne)", y = "", x = "") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Ratio élèves/prof (moyenne par cluster) 
  output$key_ratio <- renderPlot({
    data <- cluster_reactive()
    ratio_mean <- data |>
      group_by(cluster) |>
      summarise(ratio = mean(student_teacher_ratio, na.rm = TRUE)) |>
      # chiffres sur 2 lignes pour enlever la superposition
      mutate(
        pos_x = ((as.numeric(cluster) - 1) %% 4) * 1.25, 
        pos_y = ifelse(as.numeric(cluster) <= 4, 2, 1)
      )
    
    nb_clusters <- length(unique(ratio_mean$cluster))  
    if(nb_clusters == 1) colors <- c("red")
    if(nb_clusters == 2) colors <- c("red", "blue")
    if(nb_clusters == 3) colors <- c("red", "blue", "#689d71")
    if(nb_clusters == 4) colors <- c("red", "blue", "#689d71", "purple")
    if(nb_clusters == 5) colors <- c("red", "blue", "#689d71", "purple", "orange")
    if(nb_clusters == 6) colors <- c("red", "blue", "#689d71", "purple", "orange", "brown")
    if(nb_clusters == 7) colors <- c("red", "blue", "#689d71", "purple", "orange", "brown", "pink")
    if(nb_clusters == 8) colors <- c("red", "blue", "#689d71", "purple", "orange", "brown", "pink", "gray")
    names(colors) <- levels(ratio_mean$cluster) 
    
    ggplot(ratio_mean, aes(x = pos_x, y = pos_y)) +
      geom_text(aes(label = round(ratio, 1), color = cluster), 
                size = 8, fontface = "bold") +
      scale_color_manual(values = colors) +
      labs(title = "Ratio élève/enseignant \n(moyenne)", x = "", y = "") +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      coord_cartesian(xlim = c(-0.3, 4.05), ylim = c(0.5, 2.5), clip = "off")
  })
  
}
