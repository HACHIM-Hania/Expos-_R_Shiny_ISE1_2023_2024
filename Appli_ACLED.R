#Groupe9 : 
library(shiny) # Pour creer l'Application Rshiny
library(shinydashboard) #Pour avoir le tableau de bord
library(DT) # les data tables interactifs (afichage ,trie de la base) 
library(gt) # pour les tableaux personnalisés
library(gtsummary) # pour les resumés de tableaux 
#library(tools)
library(readxl) # ficher excel
library(ggplot2) # les graphiques et cartes
library(dbscan) # pour faire clusters
library(leaflet) # carte interactives
library(sp) # objet spatiale
library(sf) # objet spatiale version ameliorer
library(here) # chemin relatif pour le repertoire de tra
library(dplyr) # pour les calculs

#Importation des données

ACLED = read.csv(here("ACLED-Western_Africa.csv"))

###Regroupement des données en clusters nous pemrettant de les mettre dans de groupe homogènes
clusters <- dbscan(ACLED[, c("latitude", "longitude")], eps = 0.1, minPts = 5)
ACLED$cluster <- as.factor(clusters$cluster)



#Transformer la base en une base spatiale

ACLED_sp = ACLED
coordinates(ACLED_sp) = ~longitude+latitude

proj4string(ACLED_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs
+ellps=WGS84 +towgs84=0,0,0")
ACLED_sf = st_as_sf(ACLED_sp,coords=c(longitude,latitude))


# Calcule du nombre d'évènement par pays
base_nombre_evenement = ACLED_sf %>%
  group_by(pays) %>%
  summarise("nombre_evenement" = n())



#importation des shapes files pour tracer les limites des pays de l'Afrique de l'ouest

Afrique = read_sf(here("Africa_Boundaries-shp/Africa_Boundaries.shp"))

# Extraction des pays de l'Afrique de l'ouest
Pays_ACLED = filter(Afrique,ISO %in% c("BEN", "BFA","CIV","MLI","TGO","GHA",
                                       "LBR","SEN","NER","MRT","GNB","NGA","SLE","CPV","GMB","GIN"))


# Merge des deux bases pour avoir les limites et les évènements
Base = st_join(Pays_ACLED,base_nombre_evenement)


# Renommer la modalité "Côte d'Ivoire" en "Ivory Coast" dans la colonne NAME_0
Base_new <- Base %>%
  mutate(NAME_0 = ifelse(NAME_0 == "Côte d'Ivoire", "Ivory Coast", NAME_0))

# Renommer la variable NAME_0
Base_new = Base_new |> 
  filter(NAME_0 == pays)


# Ajout de la projection

ACLED_sf <- st_transform(ACLED_sf, crs = 4326)


var = unique(ACLED_sf$type)


#l'interface utilisateur

ui = dashboardPage( skin = "yellow",
  dashboardHeader(title = "Cartographie des évènements de l' Afrique de l'Ouest",titleWidth = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Statistique descriptive",tabName = "stat_des",icon = icon("chart-bar")),
      menuItem("Statistique Spatiale",tabName = "stat_spatiale",icon = icon("globe"),
               menuSubItem ("Nombre d'évènement par pays ", tabName = "nbre_evenement"),
               menuSubItem("Localisation des évènements",tabName = "local_evenement"),
               menuSubItem("Selection des évenements", tabName = "filter_tab")
    ))
  ),
  dashboardBody( 
    tabItems(
      tabItem(
        
        tabName = "stat_des",
        tabBox(
          
          width = 12, height = "500px",
          title = p("Statistiques",
                    style="font-size:20px;
                          font-family: Fira Sans, Arial, sans-serif;
                          monospace;
                          color:white; 
                          background-color:red;
                          padding:12px"),
          
          # Affichage de la base
          tabPanel("Data View", icon = icon("table"),
                   
                   fluidPage(
                     titlePanel(" Information sur la base de données"),
                     tabsetPanel(
                       tabPanel("5 Premières Observations", value = "first_obs",
                                tableOutput("table_first_obs")),
                       tabPanel("5 Dernières Observations", value = "last_obs",
                                tableOutput("table_last_obs")),
                       tabPanel("Toute la Base de Données", value = "all_data",
                                dataTableOutput("table_all_data")),
                       
                       
                       tabPanel("Structure de la Base", value = "var_sum",
                                verbatimTextOutput("var_summary"))
                       
                       
                     )
                   )
                  
          ),
          
          # Affichage du diagramme des barres
          tabPanel("Graphique", icon = icon("chart-simple"),
                   fluidPage(
                     titlePanel("Nombre d'évènements par année et par pays"),
                     br(),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("pays", "Sélectionnez un pays :", choices = unique(ACLED_sf$pays))
                       ),
                       mainPanel(
                         plotOutput("plot")
                       )
                     )
                   )
                     
                   )
          
          
          )
          
        ),
      
      tabItem(
        
      tabName = "nbre_evenement",
      box( title = "Nombre totale d'évènement par pays de 2015 à 2023 ",
           width = "100%",
           height = "100%",
           leafletOutput("map1"))
    ),
    
    tabItem( 
      
      tabName = "local_evenement",
      box( title = " Localisation des évènements selon le type,le pays et l'année",
           width = "100%",
           height = "100%",
           leafletOutput("map2"))
    ),
    tabItem(
      tabName = "filter_tab",
      fluidRow(
        box(
          title = "SELECTIONNER DES FAITS MARQUANTS",##titrisation de l'onglet
          selectInput("country_filter", "Sélectionner un pays :", choices = unique(ACLED$pays),multiple = TRUE),## une liste deroulante 
          ##permettant àl'utilisateur de selectionnerun pays ou il souhaite visualier ces évenements
          br(),
          ###Après le choix du pays,l'utilisateur descend et l'onglet suivant est une liste deoulant
          ##comportant les types d'evenements à choisir, c'est ce qui decrit le code suivant
          selectInput("event_type_filter", "Choisir un type événement :", choices = unique(ACLED$type),multiple = TRUE),
          br(),
          sliderInput("year_filter", "Choisir une année  :", min = min(ACLED$annee), max = max(ACLED$annee), value = c(min(ACLED$annee), max(ACLED$annee)))
        ),
        box(
          title = "Répresentation spatiale filtrée",### La carte obtenue est isssue du choix du pays selectionner par l'utilisateur
          leafletOutput("filtered_map")
        )
      )
    )
    )))
  


server = function(input, output){
  
  
  data <- reactive({
    # Charger la base de données ici
    read.csv(here("ACLED-Western_Africa.csv"))
  })
  
  # Premières 5 observations
  output$table_first_obs <- renderTable({
    head(data(), 5)
  })
  
  # Dernières 5 observations
  output$table_last_obs <- renderTable({
    tail(data(), 5)
  })
  
  # Toute la base de données
  output$table_all_data <- renderDataTable({
    data()
  })
  
  # Toute la base de données
  output$var_summary <- renderPrint({
    str(data())
  })
  
  output$plot <- renderPlot({
    ggplot(subset(ACLED_sf, pays == input$pays)) +
      aes(x = annee, fill = pays) +
      geom_bar(position = "dodge") +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3) + # Ajouter du texte avec le nombre d'événements
      xlab("Année") +
      ylab("Nombre d'événements") +
      labs(fill = "Pays") +
      ggtitle(paste("Nombre d'événements par année pour", input$pays)) +
    # Supprimer la légende
      theme(legend.position = "none") 
      
  })

  
  
  output$map1 = renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png",
               attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors') %>%
      addPolygons(
        data = Base_new,
        color = "blue",    # Utiliser une couleur transparente pour la bordure
        opacity = 1,
        weight = 0.8,
        fillColor = "transparent", # Utiliser une couleur transparente pour le remplissage
        fillOpacity = 1,
        popup = ~paste("<b>Pays :</b> ", pays , "<br><b>Nombre total d'événements :</b> ", nombre_evenement) # Contenu des popups en HTML
      )
  })
  
  
  palette <- colorFactor(palette = rainbow(length(unique(ACLED_sf$type))), domain = ACLED_sf$type)
  
  
  output$map2 = renderLeaflet({
    
    # Créer une carte Leaflet
    map2 <- leaflet() %>%
      addTiles()
    
    # Ajouter des polygones à partir de Base_polygones
    map2 <- map2 %>%
      addPolygons(data = Pays_ACLED,
                  color = "black",
                  fillColor = "#00238723",
                  fillOpacity = 1,
                  popup = ~NAME_0)
    
    # Ajouter des points à partir de Base_points
    map2 <- map2 %>%
      addCircleMarkers(data = ACLED_sf,
                       color = ~palette(type),
                       fillColor = ~palette(type),
                       fillOpacity = 0.7,
                       radius = 0.1,
                       popup = ~ paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee))%>%
      addTiles() %>% 
      addLegend(position = "topright", 
                pal = palette, 
                values =ACLED_sf$type , title = " type d'évènement")
      
    
    
  })
  
  
  # Filtrage des événements:choix des évéments
  filteredData <- reactive({
    subset(ACLED, pays == input$country_filter & type == input$event_type_filter & annee >= input$year_filter[1] & annee <= input$year_filter[2])
  })
  
  # Carte filtréeaprès lechoix du pays et de l'evenement recherché
  output$filtered_map <- renderLeaflet({
    filtered <- filteredData()
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
  
}

shinyApp(ui,server)


