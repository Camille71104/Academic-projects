##########################################################################################
######################### INSTALLATION PACKAGES  #########################################
##########################################################################################
# Vérification et installation des packages nécessaires pour l'UI
packages <- c("shiny", "DT")
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(packages, install_if_missing))

##########################################################################################
####################### IMPORTATIONS DES DONNEES #########################################
##########################################################################################
library(shiny)
library(DT)
library(shinydashboard)

rep <- read.csv("data/Bdd_stress_nettoyee.csv", row.names = "X", stringsAsFactors = TRUE)

ui <- navbarPage("Le stress en MIASHS/MAS",
                 
                 tags$head(
                   tags$style(HTML("
                     .navbar {
                       position: fixed;
                       top: 0;
                       width: 100%;
                       z-index: 1000;
                     }
                     body {
                       padding-top: 70px; 
                       background-color: #e9f1f7; /* Fond blanc cassé */
                     }
                     .custom-title {
                       font-size: 30px; 
                       font-weight: bold; 
                       text-align: center;
                       margin-bottom: 20px;
                       color: #333;
                     }
                     .justified-text {
                       text-align: justify;
                       font-size: 16px;
                       color: #555;
                       margin-top: 10px;
                       line-height: 1.6;
                     }
                     .section-title {
                       color: #333;
                       font-size: 24px;
                       font-weight: bold;
                       margin-top: 30px;
                       text-align: center;
                     }
                     .highlight {
                       color: #007BFF;
                       font-weight: bold;
                     }
                     .domain-container {
                       display: flex;
                       justify-content: center;
                       gap: 40px;
                       background-color: #ffffff;
                       padding: 20px;
                       border-radius: 10px;
                       box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);
                       max-width: 900px;
                       margin: auto;
                       margin-top: 20px;
                     }
                     .domain {
                       font-size: 18px;
                       font-weight: bold;
                       color: black;
                       text-align: center;
                     }
                   "))
                 ),
                 
                 tabPanel("Accueil", 
                          div(style = "text-align: center; margin-bottom: 20px;",
                              h1("Analyse du stress étudiant en MIASHS/MAS", class = "custom-title"),
                              p("Nous avons étudié les facteurs de stress des étudiants à travers plusieurs dimensions, allant de la santé physique et mentale à la vie universitaire et financière.", 
                                style = "font-size: 18px; color: #555; margin-top: 10px;"),
                              p("Cette application vous permet d'explorer les résultats obtenus grâce aux réponses des étudiants.", 
                                style = "font-size: 18px; color: #555;")
                          ),
                          
                          div(style = "text-align: center; margin-bottom: 20px;",
                              h3(class = "section-title", "Présentation de l'étude"),
                              div(style = "text-align: center; max-width: 800px; margin: 0 auto;",
                              p("Cette étude a été réalisée pour mieux comprendre les causes du stress chez les étudiants. Nous avons exploré cinq grands domaines influençant le stress en milieu universitaire.", 
                                style = "text-align: center; font-size: 16px; color: #555; line-height: 1.6;")),
                              
                              
                              # Bloc des 5 domaines étudiés
                              div(class = "domain-container",
                                  p("🧠 Santé mentale", class = "domain"),
                                  p("💪 Santé physique", class = "domain"),
                                  p("🎓 Vie académique", class = "domain"),
                                  p("💰 Situation financière", class = "domain"),
                                  p("🤝 Relations sociales", class = "domain")
                              )
                          ),
                          
                          
                          div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 20px; text-align: center;",
                              h3("📊 Quelques tendances générales", style = "color: #007BFF;"),
                              p("🔹 Notre enquête repose sur", nrow(rep), "réponses.","🔹 Le niveau moyen de stress est de", round(mean(rep$Niveau_stress, na.rm = TRUE), 0), "sur une échelle de 1 à 10."),
                              p("🔹", round(mean(rep$Job_etudiant == "Oui", na.rm = TRUE) * 100, 1), "% des étudiants ont un job en parallèle de leurs études.","🔹 En moyenne, les étudiants dorment", round(mean(rep$Temps_moy_sommeil, na.rm = TRUE), 1), "heures par nuit."),
                              ),
                          div(style = "text-align: center; margin-top: 100px; font-size: 15px; color: #333;",
                              p(em("Cette application a été créée par :Thy Lan Etesse, Camille Laignel, Zoé Orlandi et Audrey Poirier"))
                          )
                 ),

                 
                 tabPanel("Visualisation", 
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(width = 2,
                                           h3("Choix des catégories :"),
                                           radioButtons("radio", "Choisir un thème :", 
                                                        choices = list("Situation générale","Santé physique et habitudes quotidiennes", "Santé mentale, vie familiale et finances", "Vie étudiante")),
                                           radioButtons("checkGroup", "Choisir une catégorie :", 
                                                        choices = list("Total" = "Tous","Genre" = "Genre", "Âge"="Age","Classe sociale"="Statut_social_famille", "Trait de personnalité"="Trait_personnalité", "Niveau d'étude"="Niveau_etude"))
                              ),
                              mainPanel(
                                h2("Présentation de la base"),
                                p(paste("Grâce à vos",length(rep$Identifiant) ,"réponses à notre questionnaire, nous avons pu explorer les caractéristiques de nos différentes promotions sur plusieurs thèmes ")),
                                p(span("(Pour explorer les données, choisissez un thème dans la barre latérale)"), style = "font-size: 12px; color: black;font-style: italic"),
                                conditionalPanel(
                                  condition = "input.radio == 'Situation générale'",
                                  tabsetPanel(
                                    tabPanel("Situation générale", 
                                             p(" "),
                                             # 🔷 Bloc d’intro général
                                             div(style = "background-color: white; border: 1px solid #ccc; padding: 15px; margin-bottom: 20px; border-radius: 6px;",
                                                 h4("🎯 Objectifs de l’étude :"),
                                                 p("L'objectif principal de notre étude est d'analyser les facteurs de stress chez les étudiants, en identifiant leurs causes, manifestations et conséquences dans la vie universitaire. Nous cherchons à comprendre l'impact du stress sur le bien-être des étudiants en explorant les corrélations entre différentes variables, telles que les caractéristiques individuelles et les environnements d'étude."),
                                                 p("Notre but est d'obtenir une vision complète et nuancée des facteurs qui influencent le stress étudiant et d'ouvrir des pistes pour mieux gérer cette problématique dans le cadre académique.")
                                             ),
                                             
                                             # 🧊 Première ligne de graphiques
                                             fluidRow(
                                               column(5, plotOutput("hist_age")),
                                               column(3, plotOutput("repartition_genre")),
                                               column(4, plotOutput("bar_classe"))
                                             ),
                                             p("  "),
                                             
                                             # 🧊 Dernière ligne de graphiques
                                             fluidRow(
                                               column(5, plotOutput("class_soc")),
                                               column(7, plotOutput("trait_perso"))
                                             ),
                                             
                                             # 🔶 Bloc d’explication intermédiaire
                                             div(style = "background-color: white; border: 1px solid #ccc; padding: 15px; margin-top: 20px; margin-bottom: 20px; border-radius: 6px;",
                                                 h4("🧾 Répartition des répondants :"),
                                                 p("La majorité des répondants ont entre 18 et 22 ans, sont en Bac +4, et se déclarent issus de la classe moyenne. La répartition hommes/femmes est équilibrée, mais les personnes non binaires et les étudiants en Bac +5 sont peu représentés.

Côté personnalité, l’agréabilité est le trait dominant, tandis que l’extraversion est plus rare.

Malgré sa solidité, l’échantillon présente quelques biais* : sous-représentation des milieux populaires ou aisés, autosélection des répondants les plus concernés, et forte présence d’étudiants en début de master."),
                                                 p("*Biai : terme utilisé pour décrire des statistiques qui ne fournissent pas une représentation précise de la population")
                                             )
                                    )
                                    
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.radio == 'Santé physique et habitudes quotidiennes'",
                                  tabsetPanel(
                                    tabPanel("Santé physique et habitudes quotidiennes",
                                             p(" "),
                                             # Message explicatif conditionnel
                                             conditionalPanel(
                                               condition = "input.checkGroup == 'Trait_personnalité'", 
                                               div(style = "background-color:#f9f9f9; padding:10px; border-left:4px solid #007bff; margin-bottom:15px;",
                                                   strong("💬 Explication du trait de personnalité :"),
                                                   p("Cette variable correspond au trait de personnalité dominant que chaque étudiant s’est attribué parmi cinq grands profils issus de la psychologie des traits (modèle des Big Five). Ces profils permettent de mieux cerner certains aspects de la personnalité susceptibles d’influencer la gestion du stress. Voici les traits proposés :"),
                                                   tags$ul(
                                                     tags$li("Ouverture : créativité, imagination, curiosité et goût pour la nouveauté. Ces individus aiment explorer de nouveaux environnements et idées."),
                                                     tags$li("Conscience : organisation, rigueur, sens des responsabilités et maîtrise de soi. Ce trait peut influencer la manière de faire face aux exigences académiques."),
                                                     tags$li("Extraversion : sociabilité, expressivité émotionnelle, aisance en groupe. Ce profil peut moduler la gestion du stress par le recours au soutien social."),
                                                     tags$li("Agréabilité : empathie, altruisme, coopération. Ces personnes privilégient l’harmonie dans leurs relations et peuvent mieux vivre les tensions collectives."),
                                                     tags$li("Souci (ou névrosisme) : sensibilité accrue au stress, instabilité émotionnelle, tendance à l’anxiété. Les individus concernés peuvent être plus exposés aux effets négatifs du stress.")
                                                   ),
                                                   p("Ce trait subjectif, bien qu’auto-déclaré, apporte un éclairage complémentaire sur la manière dont chaque étudiant vit et perçoit le stress dans le cadre universitaire.")
                                               )
                                             ),
                                             p(" "),
                                             fluidRow(
                                               column(4, plotOutput("pb_sante")),
                                               column(3, plotOutput("jour_sport")),
                                               column(5, plotOutput("alimentation"))
                                             ),
                                             fluidRow(
                                               column(4, 
                                                      plotOutput("sante_plot", height = "0px"),
                                                      wellPanel(
                                                        h4("Note :"),
                                                        uiOutput("texte_sante_physique"))),
                                               column(3, 
                                                      plotOutput("sport_plot", height = "0px"),
                                                      wellPanel(
                                                        h4("Note :"),
                                                        uiOutput("texte_sante_physique_graph2")
                                                      )
                                               ),
                                               column(5, 
                                                      plotOutput("sante_plot", height = "0px"),
                                                      wellPanel(
                                                        h4("Note :"),
                                                        uiOutput("texte_sante_physique_graph3")))
                                               ),
                                             fluidRow(
                                               column(6, plotOutput("pratiques_culturelles")),
                                               div(style = "text-align: center; margin-top: 50px; font-size: 15px; color: #333;"))
                                             ,
                                             fluidRow(
                                               column(6, 
                                                      plotOutput("sante_plot", height = "0px"),
                                                      wellPanel(
                                                        h4("Note :"),
                                                        uiOutput("texte_sante_physique_heatmap")))),
                                             fluidRow(
                                               column(7, plotOutput("temps_ecran")),
                                               column(5, plotOutput("temps_sommeil"))
                                             ),
                                             fluidRow(
                                               column(7, 
                                                      plotOutput("sante_plot", height = "0px"),
                                                      wellPanel(
                                                        h4("Note :"),
                                                        uiOutput("texte_sante_physique_graph5"))),
                                               column(5, 
                                                      plotOutput("sport_plot", height = "0px"),
                                                      wellPanel(
                                                        h4("Note :"),
                                                        uiOutput("texte_sante_physique_graph6")
                                                      )
                                               ))
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.radio == 'Santé mentale, vie familiale et finances'",
                                  tabsetPanel(
                                    tabPanel("Santé mentale, vie familiale et finances",
                                             p(" "),
                                             # Message explicatif conditionnel
                                             conditionalPanel(
                                               condition = "input.checkGroup == 'Trait_personnalité'", 
                                               div(style = "background-color:#f9f9f9; padding:10px; border-left:4px solid #007bff; margin-bottom:15px;",
                                                   strong("💬 Explication du trait de personnalité :"),
                                                   p("Cette variable correspond au trait de personnalité dominant que chaque étudiant s’est attribué parmi cinq grands profils issus de la psychologie des traits (modèle des Big Five). Ces profils permettent de mieux cerner certains aspects de la personnalité susceptibles d’influencer la gestion du stress. Voici les traits proposés :"),
                                                   tags$ul(
                                                     tags$li("Ouverture : créativité, imagination, curiosité et goût pour la nouveauté. Ces individus aiment explorer de nouveaux environnements et idées."),
                                                     tags$li("Conscience : organisation, rigueur, sens des responsabilités et maîtrise de soi. Ce trait peut influencer la manière de faire face aux exigences académiques."),
                                                     tags$li("Extraversion : sociabilité, expressivité émotionnelle, aisance en groupe. Ce profil peut moduler la gestion du stress par le recours au soutien social."),
                                                     tags$li("Agréabilité : empathie, altruisme, coopération. Ces personnes privilégient l’harmonie dans leurs relations et peuvent mieux vivre les tensions collectives."),
                                                     tags$li("Souci (ou névrosisme) : sensibilité accrue au stress, instabilité émotionnelle, tendance à l’anxiété. Les individus concernés peuvent être plus exposés aux effets négatifs du stress.")
                                                   ),
                                                   p("Ce trait subjectif, bien qu’auto-déclaré, apporte un éclairage complémentaire sur la manière dont chaque étudiant vit et perçoit le stress dans le cadre universitaire.")
                                               )
                                             ),
                                             p(" "),
                                             fluidRow(
                                               column(3, plotOutput("niv_conf"),wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_sante_mental_1")
                                               )),
                                               column(3, plotOutput("exigence_perso"),wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_sante_mental_2")
                                               )),
                                               column(6, plotOutput("peur_echec"),wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_sante_mental_3")
                                               ))
                                             ),
                                             fluidRow(
                                               column(6, plotOutput("comparaison_autres"),wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_sante_mental_4")
                                               )),
                                               column(6, plotOutput("sentiment_entourage"),wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_sante_mental_5")
                                               ))
                                             ),
                                             fluidRow(
                                               column(4, plotOutput("vie_seul"),wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_sante_mental_6")
                                               )),
                                               column(4, plotOutput("stabilite_finance"),wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_sante_mental_7")
                                               )),
                                               column(4, plotOutput("visite_famille"),wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_sante_mental_8")
                                               ))
                                             )
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.radio == 'Vie étudiante'",
                                  tabsetPanel(
                                    tabPanel("Vie étudiante", 
                                             p(" "),
                                             # Message explicatif conditionnel
                                             conditionalPanel(
                                               condition = "input.checkGroup == 'Trait_personnalité'", 
                                               div(style = "background-color:#f9f9f9; padding:10px; border-left:4px solid #007bff; margin-bottom:15px;",
                                                   strong("💬 Explication du trait de personnalité :"),
                                                   p("Cette variable correspond au trait de personnalité dominant que chaque étudiant s’est attribué parmi cinq grands profils issus de la psychologie des traits (modèle des Big Five). Ces profils permettent de mieux cerner certains aspects de la personnalité susceptibles d’influencer la gestion du stress. Voici les traits proposés :"),
                                                   tags$ul(
                                                     tags$li("Ouverture : créativité, imagination, curiosité et goût pour la nouveauté. Ces individus aiment explorer de nouveaux environnements et idées."),
                                                     tags$li("Conscience : organisation, rigueur, sens des responsabilités et maîtrise de soi. Ce trait peut influencer la manière de faire face aux exigences académiques."),
                                                     tags$li("Extraversion : sociabilité, expressivité émotionnelle, aisance en groupe. Ce profil peut moduler la gestion du stress par le recours au soutien social."),
                                                     tags$li("Agréabilité : empathie, altruisme, coopération. Ces personnes privilégient l’harmonie dans leurs relations et peuvent mieux vivre les tensions collectives."),
                                                     tags$li("Souci (ou névrosisme) : sensibilité accrue au stress, instabilité émotionnelle, tendance à l’anxiété. Les individus concernés peuvent être plus exposés aux effets négatifs du stress.")
                                                   ),
                                                   p("Ce trait subjectif, bien qu’auto-déclaré, apporte un éclairage complémentaire sur la manière dont chaque étudiant vit et perçoit le stress dans le cadre universitaire.")
                                               )
                                             ),
                                             
                                             p(" "),
                                             
                                             fluidRow(
                                               column(6, plotOutput("niv_pres"), wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_vie_etudiante_1")
                                               )),
                                               column(6, plotOutput("moy_cours"), wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_vie_etudiante_2")
                                               ))
                                             ),
                                             
                                             p(" "),
                                             
                                             fluidRow(
                                               column(6, plotOutput("job"), wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_vie_etudiante_3")
                                               )),
                                               column(6, plotOutput("temps_trajet"), wellPanel(
                                                 h4("Note :"),
                                                 uiOutput("texte_vie_etudiante_4")
                                               ))
                                             )
                                    )
                                    
                                  )
                                ),
                                width = 10
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Corrélations",  
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(width = 2,
                                           h3("Analyse des variables :"),
                                           radioButtons("onglet", "Choisir une analyse :",
                                                        choices = list("Analyse des corrélations" = "heatmap",
                                                                       "Conclusions de notre étude" = "facteur",
                                                                       "Explorateur de relations" = "explorateur")),
                                           h4("Pour l'explorateur d'analyse :"),
                                           selectInput("var_x", "Choisir la variable \n explicative (X) :", 
                                                       choices = list(
                                                         "Genre" = "Genre",
                                                         "Âge" = "Age",
                                                         "Classe sociale" = "Statut_social_famille",
                                                         "Trait de personnalité"="Trait_personnalité",
                                                         "Problèmes de santé" = "Soucis_sante",
                                                         "Fréquence de sorties culturelles" = "Pratiques_activites_culturelles",
                                                         "Habitudes Alimentaires" = "Habitudes_alimentaires",
                                                         "Niveau d'exigence personnelle" = "Niveau_exigence_perso",
                                                         "Niveau de confiance en soi" = "Niveau_confiance_en_soi",
                                                         "Peur de l'échec" = "Peur_echec",
                                                         "Comparaison aux autres" = "Comparaison_aux_autres",
                                                         "Niveau d'étude" = "Niveau_etude",
                                                         "Job étudiant" = "Job_etudiant",
                                                         "Temps moyen de sommeil" = "Temps_moy_sommeil",
                                                         "Nombre de jours de sport par semaine" = "Nb_jour_sport_par_semaine",
                                                         "Vie seul" = "Vie_seul",
                                                         "Temps moyen passé sur écran par jour" = "Temps_ecran_moyen_par_jour",
                                                         "Moyenne des heures de cours" = "Moy_heure_cours",
                                                         "Niveau de pression académique" = "Niveau_pression_academique"
                                                       ), selected = "Niveau_pression_academique"),
                                           checkboxInput("ajout_regression", "Ajouter une régression linéaire", value = TRUE),
                                           checkboxInput("eq_droite", "Ajouter l'équation de la droite", value = FALSE),
                                           checkboxInput("r2_button", "Afficher le R²", value = FALSE),
                                           checkboxInput("conf_int", "Afficher l'intervalle \n de confiance", value = FALSE),
                                           # 🔥 Bouton conditionnel pour afficher l'explication SEULEMENT si régression activée
                                           conditionalPanel(
                                             condition = "input.ajout_regression == true",
                                             actionButton("explain_regression", "🔎 La régression ?", class = "btn-success")
                                           ),
                              ),
                              mainPanel(
                                h2("Exploration de nos résultats"),
                                p("Cet onglet permet d'observer l'effet de nos différentes variables sur le stress des étudiants,"),
                                p("il permet également de mettre en lumière les facteurs qui influencent le plus le niveau de stress chez les étudiants"),
                                conditionalPanel(
                                  condition = "input.onglet == 'heatmap'",
                                  tabsetPanel(
                                    tabPanel("Analyse des correlations avec le niveau de stress", 
                                                     "Matrice des corrélations de nos variables numériques sur le niveau de stress",
                                                     
                                                     # Ligne 1 : graphique + 1er encadré
                                                     fluidRow(
                                                       column(7, plotOutput("mat_corr")),
                                                       column(5,
                                                              div(style = "background-color: white; border: 1px solid #ddd; padding: 15px; border-radius: 5px;",
                                                                  strong("Comprendre la corrélation avec le niveau de stress :"),
                                                                  p(" Le tableau ci-contre présente les corrélations entre différentes variables et le niveau de stress chez les étudiants. La corrélation est une mesure statistique qui indique dans quelle mesure deux variables évoluent ensemble. Elle varie entre -1 et +1 :"),
                                                                  tags$ul(
                                                                    tags$li("Une valeur positive signifie que les deux variables augmentent ensemble."),
                                                                    tags$li("Une valeur négative indique qu'à mesure que l'une augmente, l'autre diminue."),
                                                                    tags$li("Une valeur proche de 0 suggère peu ou pas de lien direct entre les deux.")
                                                                  )
                                                              )
                                                       )
                                                     ),
                                                     
                                                     # Ligne 2 : 2 encadrés en-dessous sur toute la largeur
                                                     fluidRow(
                                                       column(12,
                                                              # Encadré 2 - Points clés
                                                              div(style = "background-color: white; border: 1px solid #ddd; padding: 15px; margin-top: 20px; border-radius: 5px;",
                                                                  strong("Ce qu’il faut retenir de cette analyse :"),
                                                                  tags$ul(
                                                                    tags$li("Pression académique (+0.43) : Il s’agit du lien positif le plus fort. Les étudiants qui ressentent une forte pression académique sont généralement plus stressés."),
                                                                    tags$li("Confiance en soi (-0.39) : Une bonne confiance en soi est associée à un stress plus faible. Cela suggère qu’aider les étudiants à renforcer leur estime personnelle pourrait contribuer à réduire leur stress."),
                                                                    tags$li("Sommeil moyen (-0.33) : Dormir davantage est lié à un stress moins élevé, ce qui souligne l'importance d'une bonne hygiène de sommeil."),
                                                                    tags$li("Pratique sportive régulière (-0.22) : Faire du sport plusieurs jours par semaine semble également aider à réduire le niveau de stress, même si le lien est un peu moins marqué."),
                                                                    tags$li("Temps d’écran (-0.03) et heures de cours (-0.009) : Ces deux facteurs n'ont que très peu de lien avec le stress, selon les données recueillies.")
                                                                                                                                        ),
                                                                  p("💡 Conclusion : Cette analyse met en lumière certains leviers potentiels de réduction du stress, notamment la confiance en soi, le sommeil et l’activité physique. Ces éléments peuvent être des pistes d’action intéressantes, tant pour les étudiants que pour les enseignants dans un cadre de prévention ou d’accompagnement.")
                                                              ),
                                                              
                                                              # Encadré 3 - Variables non numériques
                                                              div(style = "background-color: white; border: 1px solid #ddd; padding: 15px; margin-top: 20px; border-radius: 5px;",
                                                                  strong("Et les variables non numériques ?"),
                                                                  p("Même si les variables qualitatives ne figurent pas dans la matrice de corrélation ci-dessus (car la corrélation s’applique aux variables numériques), nos analyses montrent des tendances intéressantes. Par exemple, les étudiants ayant un job étudiant ou en alternance présentent un niveau de stress plus élevé. De même, le niveau d’étude joue un rôle : le stress tend à augmenter avec l’avancement dans le cursus. Enfin, des différences notables apparaissent selon le genre, avec un niveau de stress généralement plus élevé chez les femmes que chez les hommes.
"),
                                                                  p("👉 Pour celles et ceux qui souhaitent explorer plus en détail ces résultats, l’onglet d’exploration des corrélations de l’application vous permet de naviguer librement parmi les différentes variables étudiées.")
                                                              )
                                                       )
                                                     )
                                    )
                                    
                                    
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.onglet == 'facteur'",
                                  tabsetPanel(
                                    tabPanel("Conclusions de nos recherches",
                                             
                                             # Bloc 1 : Rappel de l'étude
                                             div(style = "background-color: white; border: 1px solid #ccc; padding: 15px; margin-bottom: 20px; border-radius: 6px;",
                                                 h4("🎯 Rappel de l'objectif :"),
                                                 p("Cette étude approfondit l’analyse des facteurs liés au stress chez les étudiants de MIASHS/MAS, en s’appuyant sur un ensemble de variables telles que la pression académique, le sommeil ou encore le niveau d’études. L’objectif est d’identifier les éléments les plus déterminants afin de dégager des pistes concrètes d’intervention et de prévention adaptées au contexte universitaire.

")
                                             ),
                                             
                                             #  Première ligne de graphiques
                                             fluidRow(
                                               column(6, plotOutput("plot1")),
                                               column(6, plotOutput("plot2"))
                                             ),
                                             p(" "),
                                             # Deuxième ligne de graphiques
                                             fluidRow(
                                               column(6, plotOutput("plot3")),
                                               column(6, plotOutput("plot4"))
                                             ),
                                             
                                             
                                             # Bloc 2 : Analyse intermédiaire
                                             div(style = "background-color: white; border: 1px solid #ccc; padding: 15px; margin-top: 20px; margin-bottom: 20px; border-radius: 6px;",
                                                 h4("🔎 Comprendre les causes du stress étudiant"),
                                                 p("Nos résultats révèlent des liens forts entre stress et plusieurs dimensions de la vie étudiante. Mais au-delà des chiffres, que nous disent-ils vraiment ?"),
                                                 tags$ul(
                                                   tags$li("La pression académique, fortement corrélée au stress, reflète une inquiétude face à la performance, aux échéances rapprochées, à l’incertitude de l’avenir. Ce stress peut s’amplifier dans un environnement compétitif ou peu soutenant. (graphique 2)"),
                                                   tags$li("Le genre semble également jouer un rôle : les femmes déclarent en moyenne un stress plus élevé, ce qui pourrait s’expliquer par une plus grande charge mentale (familiale, sociale, émotionnelle), un perfectionnisme accru ou une plus grande propension à verbaliser leur mal-être.(graphique 1)"),
                                                   tags$li("Le temps de sommeil est clairement associé au niveau de stress : un sommeil réduit, souvent causé par un emploi du temps surchargé, une mauvaise hygiène numérique ou de l’anxiété, affaiblit la régulation émotionnelle et aggrave le ressenti du stress.(graphique 3)"),
                                                   tags$li("Enfin, le niveau d’étude semble aussi jouer : les étudiants en Bac+3, souvent à un tournant charnière (orientation, pression du diplôme, stages...), apparaissent plus fragilisés. Cette transition académique peut accroître la pression et la sensation d’instabilité.(graphique 4)")
                                                 ),
                                                 p("En résumé, ces résultats ne sont pas de simples constats : ils soulignent des mécanismes à l’œuvre dans le quotidien étudiant, et appellent à des réponses structurelles. Mieux dormir, alléger la pression académique, valoriser l’écoute et adapter les accompagnements selon les profils, ce sont autant de leviers à activer pour favoriser un environnement plus sain.

")
                                             )
                                    )
                                    
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.onglet == 'explorateur'",
                                  tabsetPanel(
                                    tabPanel("Exporateur des correlations entre variables", 
                                             "Interface pédagogique pour appréhender les notions de régresion, de corrélation...",
                                             p(" "),
                                             plotOutput("plot_stress")
                                    )
                                  )
                                ),
                                width = 10
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Données",
                          h2("Données"),
                          DT::DTOutput("table")
                 )
)