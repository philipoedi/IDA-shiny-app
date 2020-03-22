
#SHINY APP
#Benötigte Packages werden installiert, wenn nicht vorhanden
if (!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
  require(leaflet.extras)
}

# Benötigte librarys werden anschließend eingebunden.
library(shiny)
library(readr)
library(leaflet)
library(leaflet.extras)
library(dplyr)

#Finalen Datensatz laden
fahrzeuge <- read_csv("Finaler_Datensatz_01.csv")
last_removed_radius <- NULL

################Define UI for application that draws a histogram ################ 
ui <- fluidPage(
  
  # Application title
  titlePanel("Fahrzeuge - Typ11 - K1DI1"),
  
  # Inhalte der Sidebar
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "radius_slider",
        label = "Radius",
        value = 0,
        min = 0, 
        max = 700,
        step = 1),
      
      actionButton(
        inputId = "add_button",
        label = "Add"),
      
      actionButton(
        inputId = "delete_button",
        label = "Delete"),
      
      tableOutput(
        outputId = "radii_list"),
      
      plotOutput("barPlot")
    ),
    
    
    mainPanel(
      # Anzeigen der Weltkarte
      leafletOutput(outputId = "mymap"),
      
      #dropdown mit kreisen
      selectInput("drop_down_radius", "Umkreis in km:",
                  0),
      
      # Anzeigen Tabelle mit Daten  
      dataTableOutput("data_fahrzeuge")
    )
    
    
    
  )
)

################ Define server logic ################ 
server <- function(input, output,session) {
  
  # Erstellung leerer Vektoren
  rv <- reactiveValues()
  rv$radii <- c()
  rv$current_radius <- 0
  rv$acc_fahrz <- c()
  
  # Darstellung der zugrunde liegenden Daten
  update_fahrzeuge_data_table <- eventReactive(rv$current_radius, {
    print("updatedata")
    print(rv$current_radius)
    data <- fahrzeuge %>% filter(abs(Distanz_in_km) < as.numeric(rv$current_radius)) %>% 
      select(Postleitzahl, Gemeinden, "Fahrzeuganzahl" = Anzahl_Fahrzeuge, "Distanz [km]" = Distanz_in_km)
  })
  
  output$data_fahrzeuge <- renderDataTable(update_fahrzeuge_data_table())
  
  # Handler für das hinzufügen eines Radius
  add_radius <- observeEvent(input$add_button, {
    radi <- input$radius_slider
    if (!radi %in% rv$radii) {
      if (radi > 0) {
        rv$radii <- c(rv$radii, radi)
        rv$current_radius <- radi
        leafletProxy("mymap", data = data) %>%
          addCircles(layerId = as.character(radi),
                     lat = 52.520007, lng = 13.404954,
                     weight = 1, radius = radi*1000,
                     color = "red",
                     fillOpacity = 0.2,
                     highlightOptions = highlightOptions(sendToBack = TRUE))
        updateSelectInput(session, "drop_down_radius",
                          label = "Umkreis in km",
                          choices = sort(rv$radii),selected=rv$current_radius)
      }
    }
  })
  #Entfernt den Radius, wenn dieser gelöscht wurde
  delete_radius <- observeEvent(input$delete_button, {
    leafletProxy("mymap", data = data) %>%
      removeShape(as.character(tail(rv$radii, 1)))
    rv$radii <- head(rv$radii, -1)
    updateSelectInput(session, "drop_down_radius",
                      label = "Umkreis in km",
                      choices = sort(rv$radii),selected=rv$current_radius)
  })
  #Nimmt Input in dropdown an, wenn anderer Radius ausgewählt wird
  change_radius_display <- observeEvent(input$drop_down_radius,{
    rv$current_radius <- input$drop_down_radius
  })
  
  #Erstellt Liste von Radien mit dem Namen "rv$radii"
  output$radii_list <- renderTable({
    return(data.frame("Radius_in_km"=rv$radii, Fahrzeuganzahl=as.integer(rv$acc_fahrz)))
  })
  
  
  # Hinzufügen Interaktive Karte 
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      setView(lng = 9, lat = 51, zoom = 5.4) %>% #Deutschland als Standardansicht verwenden
      addTiles() %>% 
      addCircles(data = fahrzeuge,
                 lat = ~ Breitengrad, lng = ~ Laengengrad,
                 weight = 1, radius = ~sqrt(Anzahl_Fahrzeuge)*300, # Kreisgröße ist abhängig von der Fahrzeuganzahl
                 popup = ~paste( Gemeinden, Anzahl_Fahrzeuge),
                 label = ~as.character(paste( Gemeinden,", Fahrzeuganzahl:", Anzahl_Fahrzeuge)),
                 fillOpacity = 0.2)
  })
  
  
  # Plotten Balkendiagramm
  output$barPlot <- renderPlot({
    
    
    
    # Prüfung aller Gemeinden, ob sie sich innerhalb des Radius befinden
    acc_fahrz = c()
    for (rad in rv$radii) {
      ss <- subset(fahrzeuge, fahrzeuge$Distanz_in_km < rad)
      acc_fahrz <- c(acc_fahrz, sum(ss$Anzahl_Fahrzeuge))
    }
    
    # Updaten von Anzahl an Fahrzeugen in den Radien
    rv$acc_fahrz <- acc_fahrz
    
    # Plotten Balkendiagramm mit der Anzahl an Gemeinden die sich innerhalb des Radius befinden
    if (length(acc_fahrz)) {
      barplot(acc_fahrz,
              col = 'lightblue', 
              ylab ="Anzahl Fahrzeuge",
              xlab = "Umkreis von Berlin [km]",
              ylim = range(pretty(c(0, max(rv$acc_fahrz)))),
              names.arg = rv$radii)
    }
    
  })
  
  
}
#################  Run the application ################ 
shinyApp(ui, server)