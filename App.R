# --------------------------------------------------------------------------- #
#                                                                             #
#                   DEPLOIEMENT DE LA PEREQUATION                             #
#                         APPLICATION DE SUIVI                                #
#                                                                             #
# --------------------------------------------------------------------------- #

# Lignes pour import des documents de nommage (si besoin de ré-import)
# library(xlsx)
# nomenclature_programmes <- read.xlsx(file = "C:/Users/22195/Documents/02 - Perequation - Methode de calcul/05 - Deploiement/01 - Shiny App dev/programmes_region.xlsx", sheetIndex = 1, header = TRUE)
# save(nomenclature_programmes, file = "C:/Users/22195/Documents/02 - Perequation - Methode de calcul/05 - Deploiement/01 - Shiny App dev/nomenclature_programmes.RData")
# directions_services <- read.xlsx(file = "C:/Users/22195/Documents/02 - Perequation - Methode de calcul/05 - Deploiement/01 - Shiny App dev/directions_services.xlsx", sheetIndex = 1, header = TRUE)
# save(directions_services,file = "C:/Users/22195/Documents/02 - Perequation - Methode de calcul/05 - Deploiement/01 - Shiny App dev/directions_services.RData")


# Opition 
options(scipen = 999)

# --- Chargement des packages
library(shiny)
library(RPostgres)
# library(tidyverse)
library(DBI)
library(markdown)
library(sqldf)
library(tidyr)
library(ggplot2)
library(dplyr)
library(forcats)
library(openxlsx)
library(DT)
library(shinythemes)
library(shinyjs)
library(RColorBrewer)
library(stringr)
library(labelled)
library(lubridate)




# --- Ouveture de la base de données "formulaires" en lecture et écriture

# Ecriture
db <- 'formulaire'
host_db <- 'postsig'
db_port <- '5434'
db_user <- 'form_admin'
db_password <- 'citron!'

writing_form <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db,
                          port=db_port, user=db_user, password=db_password)


# Lecture
db <- 'formulaire'
host_db <- 'postsig'
db_port <- '5434'
db_user <- 'form_user'
db_password <- 'orange'

con_form <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db,
                      port=db_port, user=db_user, password=db_password)




# ------------------------------ PARTIE UI ---------------------------------- #

ui <- navbarPage("Péréquation - Fomulaires pour suivi", theme = shinytheme("sandstone"),
                 
                 # Onglet n°2 - Déploiement dans une politique
                 tabPanel("Déploiement effectif",
                          
                          # Texte d'introduction
                          fluidRow(
                            column(width = 5, offset = 0,
                                   tags$b("Ce questionnaire vise à tracer les cas d'application de la péréquation dans uen politique régionale"),
                                   tags$p("Il est destiné à n'être complété qu'une seule fois par la personne référente du dispositif"),
                                   tags$em("Pour toute modification, merci de solliciter le SCODYT : observatoire@bretagne.bzh"),
                                   hr()
                            )
                          ),
                          
                          
                          fluidRow(
                            
                            column(width = 3, offset = 1,
                                   
                                   # Titre
                                   tags$h4("Dispositif péréqué"),
                                   hr(),
                                   
                                   # Date de validation en CP
                                   dateInput("date_deploi", label = "Date de validation en Commission / Session du conseil régional", format = "dd/mm/yyyy", value = Sys.Date()),
                                   
                                   # N° mission
                                   fluidRow(
                                     column(width = 3,
                                            selectInput("mission_deploi", label = "N° de la mission dont le dispositif fait partie", choices = c("Mission 1 - Aménagement", "Mission 2 - Économie", "..."))
                                     ),
                                     column(width = 9,
                                            tags$b("Libellé de la mission"),
                                            em(textOutput("mission_lib"))
                                     )
                                   ),

                                   
                                   
                                   # N° programme
                                   fluidRow(
                                     column(width = 3,
                                            selectInput("programme_deploi", label = "N° du programme dont le dispositif fait partie", choices = c("101 - Contractualisation avec les territoires", "102 - Observer, accompagner, anticiper les mutations territoriales", "..."))
                                            ),
                                     column(width = 9,
                                            tags$b("Libellé du programme"),
                                            em(textOutput("programme_lib"))
                                            )
                                   ),
                                   
                                   
                                   # Nom du dispositif
                                   textInput("lib_disp_deploi", label = "Dénomination du dispositif concerné"),
                                   
                                   
                                   # Montant total
                                   numericInput("montant_disp_deploi", label = "Budget du dispositif", value = NULL),
                                   
                                   # Durée d'intervention du dispositif
                                   dateRangeInput("periode_disp_deploi", label = "Période de mise en oeuvre du dispositif", format = "dd/mm/yyyy")
                                   
                            ),
                            
                            column(width = 3, offset = .5,
                                   
                                   # Titre
                                   tags$h4("Entité administrative référente"),
                                   hr(),
                                   
                                   # Direction référente
                                   selectInput("dir_deploi", label = "Direction portant de ldispositif", choices = c("Direction 1", "Direction 2", "...")),
                                   
                                   # Service ayant sollicité l'échange
                                   selectInput("service_deploi", label = "Service portant le dispositif", choices = c("Service 1", "Service 2", "...")),
                                   
                                   # Personne ayant sollicité la consultation
                                   textInput("indiv_deploi", label = "Personne référente du dispositif", value = "Nom, Prénom")
                                   
                            ),
                            
                            column(width = 3,
                                   
                                   # Titre
                                   tags$h4("Méthode retenue"),
                                   hr(),
                                   
                                   # Méthode d'application envisagée de la péréquation
                                   checkboxInput("meth_env_epci_deploi", label  = "Application à une enveloppe par EPCI"),
                                   checkboxInput("meth_env_pop_deploi", label  = "Application à une enveloppe par individu/unité"),
                                   checkboxInput("meth_tx_deploi", label  = "Application à un taux d'intervention"),
                                   checkboxInput("meth_select_gpe_deploi", label  = "Sélection/Priorisation d'un sous-ensemble de territoires"),
                                   checkboxInput("meth_note_deploi", label  = "Application à une notation / expertise de projet"),
                                   checkboxInput("meth_autre_deploi", label  = "Autre"),
                                   
                                   # Autre méthode d'application et/ou précision
                                   textInput("meth_pereq_prec_deploi", label = "Précision sur la méthode retenue", value = NULL),
                                   
                            )
                            
                          ),
                          
                          
                          # Vérifiation et validation
                          hr(),
                          

                          fluidRow(
                            # Vérfication de doulons
                            column(width = 7, offset = 1,
                                   tags$b("Recherche de doublons"),
                                   p("Le tableau présente des enregistrement qui pourraient être des doublons. Avant d'enregistrer les données, veuillez vous assurer que le dispositif que vous voulez saisir n'existe pas déjà en base de données."),
                                   em("En cas de besoin, vous pouvez contacter le SCODYT : observatoire@bretagne.bzh"),
                                   br(),br(),
                                   DTOutput("tab_doublon_poss"),
                                   br(), br(),
                            ),
                            # Bouton de validation
                            column(width = 3, offset = 1,
                                   br(),br(),
                                   actionButton("valid_deploi", "Enregistrer le formulaire"),
                                   br(), br()
                                   )
                            ),
                          
                 )
                 
)




# ---------------------------- PARTIE SERVER -------------------------------- #

server <- function(input, output, session){
  
  
  # Tablature des programmes budgétaires de la Région Bretagne
  nomenclature_tab <- reactive({
    
    load(file = "nomenclature_programmes.RData")
    nomenclature_programmes
    
  })
  
  
  # Import de la liste des directions et services de la Région
  tab_dir_serv <- reactive({
    
    load(file = "directions_services.RData")
    directions_services
    
  })
  
  
  
  # ----------------------------- FORMULAIRE DE DEPLOIEMENT DE LA PEREQUATION --------------------------------------- #
  
  
  # --- Création des listes déroulantes conditionnelles
  
  # Missions
  observe({
    
    # Propsition de toutes les mission existantes
    x <- nomenclature_tab()
    
    # Can also set the label and select items
    updateSelectInput(session, "mission_deploi",
                      label = "Mission",
                      choices = sort(x$mission)
    )
  })
  
  # Programmes
  observe({
    
    # Liste des programme de la mission choisie (si première consultation)
    x <- nomenclature_tab() %>% filter(mission %in% input$mission_deploi)
    
    # Can also set the label and select items
    updateSelectInput(session, "programme_deploi",
                      label = "Programme",
                      choices = sort(x$programme)
    )
    
  })
  
  # Liste déroulante des directions
  observe({
    x <- tab_dir_serv()
    
    # Can also set the label and select items
    updateSelectInput(session, "dir_deploi",
                      label = "Direction",
                      choices = sort(x$dir)
    )
  })
  
  # Liste déroulante des services
  observe({
    x <- tab_dir_serv() %>% filter(dir %in% input$dir_deploi)
    
    # Can also set the label and select items
    updateSelectInput(session, "service_deploi",
                      label = "Service",
                      choices = sort(x$service)
    )
  })
  
  # --- Affichage de l'intitulé de la mission et du programme choisis
  
  # Mission
  output$mission_lib <- renderText({
    
    temp <- nomenclature_tab() %>% 
      filter(mission == input$mission_deploi & is.na(mission_lib) ==  FALSE) %>%
      select(mission_lib)
    
    temp$mission_lib
    
  })
  
  
  # Programme
  output$programme_lib <- renderText({
    
    temp <- nomenclature_tab() %>% 
      filter(mission == input$mission_deploi, programme == input$programme_deploi) %>%
      select(programme_lib)
    
    temp$programme_lib
    
  })
  
  
  
  
  
  # --- Création de la ligne (pour intégration)
  rowform_deploi <- reactive({
    
    # Création ligne de données
    temp <- data.frame("date_valid" = input$date_deploi, 
                       "dir_resp"=input$dir_deploi,
                       "service_resp"=input$service_deploi,
                       "indiv_resp"=input$indiv_deploi,
                       "mission"=input$mission_deploi,
                       "programme"=input$programme_deploi,
                       "lib_disp"=input$lib_disp_deploi,
                       "date_deb_disp" = input$periode_disp_deploi[1],
                       "date_fin_disp" = input$periode_disp_deploi[2],
                       "meth_env_epci" = input$meth_env_epci_deploi, 
                       "meth_env_pop" = input$meth_env_pop_deploi, 
                       "meth_tx" = input$meth_tx_deploi, 
                       "meth_select_gpe" = input$meth_select_gpe_deploi, 
                       "meth_note" = input$meth_note_deploi, 
                       "meth_autre" = input$meth_autre_deploi,
                       "meth_pereq_prec"=input$meth_pereq_prec_deploi,
                       "ref_saisie"=Sys.Date(),
                       "montant_disp"=input$montant_disp_deploi
    )
    
  })
  
  
  
  # --- Import de la table de données existante
  data_form_deploi <- reactive({
    dbGetQuery(con_form, "SELECT * FROM interne.perequation_form_deploiement")
  })
  
  
  # Intégration des données en table
  observeEvent(input$valid_deploi, {
    
    temp <- data_form_deploi()
    
    if(is.na(input$lib_disp_deploi) == TRUE | is.na(input$montant_disp_deploi) == TRUE | is.na(input$meth_pereq_prec_deploi) == TRUE){
      
      # Affichage notification
      showNotification("Attention : Merci de compléter tous les champs", type = "message")
      
    }else{
      
      # Ajout de la ligne dans la table
      dbGetQuery(writing_form, 
                 paste0("INSERT INTO interne.perequation_form_deploiement VALUES (", 
                        max(dbGetQuery(con_form, "SELECT id FROM interne.perequation_form_deploiement"))+1,",",
                        "'",input$date_deploi,"'",  "," , 
                        "'", input$dir_deploi,"'",  ",",
                        "'", input$service_deploi,"'",  ",",
                        "'", str_replace_all(input$indiv_deploi,"'","-"),"'",  ",",
                        "'", input$mission_deploi,"'",  ",",
                        "'", input$programme_deploi,"'",  ",",
                        "'", str_replace_all(input$lib_disp_deploi,"'","-"), "'", ",",
                        "'",input$periode_disp_deploi[1],"'", ",",
                        "'",input$periode_disp_deploi[2],"'", ",",
                        "'", input$meth_env_epci_deploi,"'",  ",",
                        "'", input$meth_env_pop_deploi,"'", ",",
                        "'", input$meth_tx_deploi,"'", ",",
                        "'", input$meth_select_gpe_deploi,"'", ",",
                        "'", input$meth_note_deploi,"'", ",",
                        "'", input$meth_autre_deploi,"'", ",",
                        "'", str_replace_all(input$meth_pereq_prec_deploi,"'","-"),"'", ",",
                        "'",Sys.Date(),"'", ",",
                        input$montant_disp_deploi,
                        ")"
                 ))
      
      
      # Affichage notification
      showNotification("Données intégrées en base", type = "message")
      
    }
    
  })
  
  
  # --- Test de doublon
  output$tab_doublon_poss <- renderDataTable(rownames = FALSE, options = list(searching = FALSE, paging = FALSE),{
    
    # récup données en base
    temp <- dbGetQuery(con_form, "SELECT * FROM interne.perequation_form_deploiement")
    
    # Sélection des lignes ayant certaines informations identiques
    temp %>% filter(date_valid == input$date_deploi, service_resp == input$service_deploi, programme == input$programme_deploi) %>%
      select(date_valid, service_resp, indiv_resp, programme, lib_disp, montant_disp) %>%
      rename("Date de CP" = date_valid, "Service" = service_resp, "Référent·e" = indiv_resp, "Programme" = programme, "Nom du dispositif" = lib_disp, "Budget du dispositif" = montant_disp)
    
  })
  
  
}






# ---------------------------- LANCEMENT APP -------------------------------- #

shinyApp(ui = ui, server = server)
