#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(colourpicker)
library(plotly)
library(leaflet)
library(shiny)

pisa_score <- read.csv("./Data/pisa_final.csv", header = TRUE)
fluidPage(
  # navbarPage
  navbarPage("PISA score",
             
             #### ANNEXE : CSS PERSONNALISÉ
             # optimiser les div/style pour éviter répétitions de style
             tags$head( 
               tags$style(HTML("
                      .domain-container {
                            display: flex;  /* Aligne sur une seule ligne  */
                            justify-content: center;     /* Centre horizontalement */
                            gap: 40px;       /* Espace entre éléments */
                            background-color: #ffffff;   /* Fond blanc */
                            padding: 20px;       /* Espace intérieur */
                            border-radius: 10px;         /* Coins arrondis */
                            box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1); /* Ombre pour effet “carte” */
                            max-width: 900px;   /* Largeur maximale par rapport aux 12000 px de largeur de page*/
                            margin: auto;  /* Centre le bloc dans la page */
                            margin-top: 20px;    /* Espace au-dessus du bloc */
                        }
                      .journal-title {
                            font-family: 'Georgia', serif;
                            font-size: 38px;
                            font-weight: bold;
                            text-align: center;
                            margin-bottom: 5px;
                        }
      
                      .journal-subtitle {
                            text-align: center;
                            font-style: italic;
                            color: #555;
                            margin-bottom: 30px;
                        }
      
                      .journal-text {
                            text-align: justify;
                            text-indent: 30px;
                            font-size: 16px;
                            line-height: 1.7;
                            color: #333;
                        }
      
                      .section-title {
                            font-family: 'Georgia', serif;
                            font-size: 22px;
                            margin-top: 25px;
                            border-bottom: 1px solid #ccc;
                            padding-bottom: 5px;
                        }
      
                      .info-box {
                            background-color: #f8f9fa;
                            padding: 20px;
                            border-radius: 10px;
                            box-shadow: 0px 4px 10px rgba(0,0,0,0.1);
                            text-align: center;
                            margin-bottom: 20px;
                        }
      
                      .info-number {
                            font-size: 32px;
                            font-weight: bold;
                            color: #003366;
                        }
      
                      .info-label {
                            font-size: 14px;
                            color: #555;
                        }")
               )
             ),
             
             #### ONGLET 1 : PRÉSENTATION (format inspiré d'une précédente app shiny : https://camille-laignel.shinyapps.io/shiny/)
             tabPanel(
               "Présentation",
               
               # Titre et sous-titre
               div(style = "text-align: center; margin-bottom: 30px;",
                   h1("Visualisation des résultats PISA"),
                   p(
                     "Cette application propose une exploration interactive des performances scolaires à l’échelle internationale, à partir des données issues de l’enquête PISA.",
                     style = "font-size: 18px; color: #555; margin-top: 10px;"
                   ),
                   p(
                     "Elle a été développée dans un objectif pédagogique, dans le cadre d’un projet de visualisation de données.",
                     style = "font-size: 18px; color: #555;"
                   )
               ),
               
               # Présentation du projet
               div(style = "text-align: center; margin-bottom: 30px;",
                   h3("Contexte du projet"),
                   div(style = "text-align: center; max-width: 850px; margin: 0 auto;",
                       p(
                         "Nous nous plaçons dans une démarche de maquette exploratoire, comme si cette application était destinée aux équipes en charge de l’enquête PISA (Programme for International Student Assessment) de l’OCDE.",
                         style = "font-size: 16px; color: #555; "
                       ),
                       p(
                         "L’objectif est de proposer un outil interactif permettant de visualiser, comparer et analyser les principaux résultats par pays, tout en explorant certains déterminants sociodémographiques susceptibles d’expliquer les écarts observés.",
                         style = "font-size: 16px; color: #555; "
                       )
                   )
               ),
               
               # Axes d’analyse
               div(style = "text-align: center; margin-bottom: 30px;",
                   h3("Axes d’analyse explorés"),
                   div(class = "domain-container",
                       p("🌍 Comparaisons internationales"),
                       p("📊 Scores et performances scolaires"),
                       p("👥 Facteurs sociodémographiques"),
                       p("📈 Évolutions temporelles"),
                       p("🔍 Lecture et interprétation des écarts")
                   )
               ),
               
               # Données et limites
               div(
                 style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 20px; text-align: center;",
                 h3("📂 Données et périmètre de l’application", style = "color: #007BFF;"), # Pour emojis : écrire un faux mail, trouver emoji souhaité, copier coller
                 div(style = "max-width: 800px; margin: 0 auto;",
                     p(
                       "Les données utilisées dans cette application couvrent les cycles de l’enquête PISA disponibles sur Kaggle jusqu’en 2018, enrichient de 
                       plusieurs autres indicateurs socio-démographiques recueillis sur l'open data de la Banque Mondiale.",
                       style = "font-size: 16px; color: #555;"
                     ),
                     p(
                       "À l'avenir, cette application pourrait être enrichie avec les nouvelles éditions de l’enquête, notamment si cette maquette venait à susciter de l’intérêt.",
                       style = "font-size: 16px; color: #555;"
                     )
                 )
               )
             ),
             
             #### ONGLET 2 : QU'EST CE QUE PISA ?
             tabPanel("Qu'est ce que l'enquête PISA ? ", 
                      fluidRow(
                        
                        # colonne de gauche avec présentation et déroulement de l'épreuve
                        
                        column(
                          width = 8,
                          
                          div(class = "journal-title", "PISA : Comprendre l’évaluation internationale"),
                          div(class = "journal-subtitle",
                              "Une étude comparative des systèmes éducatifs à l’échelle mondiale"),
                          
                          
                          
                          div(class="section-title", "Présentation"),
                          
                          # création div parents/enfants pour alignement texte/carte #IA
                          div(style = "display: flex; align-items: flex-start; margin-bottom: 20px;",
                              
                              div(style = "flex: 1; padding-right: 20px;",
                                  div(class="journal-text",
                                      HTML("<br><br>"),
                                      p("    Le Programme international pour le suivi des acquis des élèves (PISA) est une évaluation créée par l’OCDE, qui vise à tester les compétences des élèves de 15 ans en lecture, mathématiques et sciences."),
                                      p("    Cette étude, menée tous les trois ans depuis 2000 (et tous les quatre ans à partir de 2025), constitue la plus grande évaluation internationale dans le domaine de l’éducation.")
                                  )
                              ),
                              
                              # carte des pays participants
                              div(style = "flex: 2;",
                                  plotOutput("carte_pisa", height = "400px")
                              )
                          ),
                          
                          div(class="journal-text",
                              p("PISA mesure la capacité des élèves à mobiliser leurs connaissances scolaires dans des situations proches de la vie quotidienne et permet de comparer l’efficacité des systèmes éducatifs.")
                          ),
                          
                          div(class="section-title", "Déroulement de l’évaluation"),
                          
                          div(class="journal-text",
                              p("L’évaluation dure environ 3h30 et comprend quatre séquences :"),
                              tags$ul(
                                tags$li("2 heures d’épreuves en lecture, mathématiques, sciences et compétences numériques"),
                                tags$li("Un questionnaire de 35 minutes portant sur le contexte socio-culturel et le bien-être scolaire")
                              ),
                              p("Les résultats sont anonymes et analysés à l’échelle des systèmes éducatifs, et non des élèves individuellement.")
                          ),
                          
                          
                        ),
                        
                        # Colonne de droite avec chiffres clés/objectifs et hyperliens
                        column(
                          width = 4,
                          # Chiffres clés
                          div(class="info-box",
                              div(class="info-number", "92"),
                              div(class="info-label", "Pays participants")
                          ),
                          
                          div(class="info-box",
                              div(class="info-number", "3"),
                              div(class="info-label", "Domaines évalués")
                          ),
                          
                          div(class="info-box",
                              div(class="info-number", "3h30"),
                              div(class="info-label", "Durée de l’évaluation")
                          ),
                          
                          div(class="info-box",
                              div(class="info-number", "15 ans"),
                              div(class="info-label", "Âge des élèves évalués")
                          ),
                          
                          # Objectifs
                          div(class="section-title", "Objectifs"),
                          
                          div(class="journal-text",
                              tags$ul(
                                tags$li("Mesurer les performances des élèves"),
                                tags$li("Étudier leur préparation à la vie adulte"),
                                tags$li("Identifier les facteurs socio-économiques influençant les résultats"),
                                tags$li("Comparer l’équité et l’efficacité des systèmes éducatifs")
                              )
                          ),
                          
                          # Hyperliens
                          div(class="section-title", "En savoir plus"),
                          div(
                            style = "margin-top: 10px; font-size: 14px; line-height: 1.6;",
                            tags$ul(
                              tags$li(tags$a(href="https://www.oecd.org/fr/about/programmes/pisa.html", target="_blank", "Page officielle PISA (OCDE)")),
                              tags$li(tags$a(href="https://www.education.gouv.fr/pisa-programme-international-pour-le-suivi-des-acquis-des-eleves-41558", target="_blank", "Page officielle PISA (Ministère de l'éducation nationale)")),
                              tags$li(tags$a(href="https://www.oecd.org/fr/publications/resultats-du-pisa-2018-volume-i_ec30bc50-fr.html", target="_blank", "Résultats PISA 2018 (Volume I)"))
                            )
                          )
                        )
                      )
             ),
             
             ### ONGLET 3 : HISTORIQUE PISA 
             tabPanel("Historique du Score PISA",
                      
                      fluidRow(
                        # COLONNE DE FILTRES 
                        column(width = 3,
                               wellPanel(
                                 # 1. Année :
                                 selectInput("annee_choice", "Choisir une année pour le classement :",
                                             choices = sort(unique(pisa_score$year)),
                                             selected = 2018
                                 ),
                                 
                                 hr(),
                                 
                                 # 2. Pays
                                 selectInput("country_choice", "Choisir un pays pour les courbes :",
                                             choices = c("Monde entier" = "all", sort(unique(pisa_score$name_long))),
                                             selected = "all"),
                                 
                                 hr(),
                                 
                                 
                                 # 3. Matière
                                 radioButtons("matiere_choice", "Matière :",
                                              choices = c("Moyenne Globale" = "Globale",
                                                          "Mathématiques" = "maths",
                                                          "Lecture" = "reading",
                                                          "Sciences" = "sciences",
                                                          "Comparer les matières" = "Comparaison_matiere"),
                                              selected = "Globale"),
                                 
                                 hr(),
                                 
                                 # 4. Genre
                                 radioButtons("gender_choice", "Genre :",
                                              choices = c("Mixte" = "TOT",
                                                          "Filles" = "GIRL", 
                                                          "Garçons" = "BOY",
                                                          "Comparer les genres" = "Comparaison_genre"),
                                              selected = "TOT")
                               )
                        ), 
                        
                        # COLONNE GRAPHIQUE
                        column(width = 9, 
                               div(
                                 style = "margin-bottom: 15px;",
                                 
                                 div(strong("Explorez l'évolution des scores PISA"), style = "font-size: 20px; margin-bottom: 5px;"),
                                 
                                 div(
                                   "Utilisez les filtres pour sélectionner l'année, le pays, la matière et le genre, et découvrez les tendances, comparaisons et performances des élèves.",
                                   style = "font-size: 14px; margin-bottom: 5px;"
                                 ),
                                 
                                 div(
                                   "Visualisez les scores globaux, les classements des 10 meilleurs et 10 derniers, ou examinez les courbes détaillées pour chaque pays et matière.",
                                   style = "font-size: 14px;"
                                 )
                               ),
                               
                               wellPanel(
                          tabsetPanel(id = "viz",
                                      tabPanel("Les 10 premiers",
                                               plotlyOutput("top10")),
                                      tabPanel("Les 10 derniers",
                                               plotlyOutput("flop10")),
                                      tabPanel("Courbes",
                                               plotlyOutput("pisaPlot"))
                          )
                               
                        ) 
                        
                      )
                      )
             ),
             
             #### ONGLET 4 : CLUSTERING 
             tabPanel("Clustering",
                      fluidRow(
                        column(5,
                               sliderInput("k", "Nombre de clusters",
                                           min = 1, max = 8, value = 3, step = 1,
                                           width = "100%"),
                               actionButton("popupClustering", "Qu'est-ce que le clustering ?", icon = icon("info-circle"))
                        ),
                        column(7,
                               h4(HTML("<b>📊 Scores PISA moyens par cluster</b>")),
                               plotOutput("scores_cluster", height = "120px")
                        )
                      ),  
                      fluidRow(
                        column(5,
                               h4(style = "margin-top: 0px;",HTML("<b>🗺️ Carte interactive des clusters</b>")),
                               leafletOutput("map_cluster", height = "550px")
                        ),
                        column(7,
                               h4(style = "margin-top: 0px;",HTML("<b>💰 Profil économique </b>")),
                               fluidRow(
                                 column(4, plotOutput("box_gni", height = "250px")),
                                 column(4, plotOutput("bar_hdi", height = "250px")),
                                 column(4, plotOutput("box_gov", height = "250px"))
                               ),
                               h4(HTML("<b>📚 Indicateurs scolaires </b>")),
                               fluidRow(
                                 column(4, plotOutput("box_school", height = "250px")),
                                 column(4, plotOutput("bar_out", height = "250px")),
                                 column(4, plotOutput("key_ratio", height = "250px"))
                               )
                        )
                      )  
             )
  )
  
)

