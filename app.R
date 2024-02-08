# LIST OF THINGS FOR FUTURE REFERENCE
# shinyapps.io setup: https://www.shinyapps.io/admin/#/dashboard
# this application uses R 4.2.2 I'M AFRAID TO UPDATE IT MIGHT BREAK EVERYTHING

# DOES THIS EVEN WORK
# if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")

# i think these are needed so it doesn't completely break on web launch
# i worked on this for so long... are all of these needed? surely some are defunct
# install.packages('htmltools')
# install.packages('shiny')
# install.packages('shiny.router')
# install.packages('leaflet')
# install.packages('dplyr')
# install.packages('DT')
# install.packages('shinydashboard')
# install.packages('ggplot2')


library(shiny)
library(shiny.router)
library(leaflet)
library(dplyr)
library(DT)
library(shinydashboard)
library(ggplot2)
# this library brings me immense pain
library(htmltools)


ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = 'home'),
      menuItem("Raw Data", tabName = 'data'),
      menuItem("Visualization Maps", tabName = 'maps'),
      menuItem("Altitude Dumbbell", tabName = 'dumbbell'),
      menuItem("Pie Charts", tabName = 'pie')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem( # fluidRow() in here if needed
        tabName = 'home',
        titlePanel("Home"),
        p("Welcome to the home page"),
        h1("TODO:"),
        p("mean & median marker on dumbbell"),
        p("(potentially just use a box plot on separate page)")
      ),
      tabItem(
        tabName = 'data',
        # titlePanel("Raw Data"),
        # dataTableOutput("table")
        box(
          title = "Data Table",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          DTOutput('tableDT', width = 'auto', fill = FALSE)
        )
      ),
      tabItem(
        tabName = 'maps',
        box(
          title = "Altitude Map",
          status = "warning",
          solidHeader = TRUE,
          leafletOutput("altmap")
        ),
        box(
          title = "Race Map",
          status = "warning",
          solidHeader = TRUE,
          leafletOutput("racemap")
        ),
        box(
          title = "Population Map",
          status = "warning",
          solidHeader = TRUE,
          leafletOutput("popmap")
        ),
        box(
          title = "Species Map",
          status = "warning",
          solidHeader = TRUE,
          leafletOutput("speciesmap")
        )
      ),
      tabItem(
        tabName = 'dumbbell',
        fluidRow(
          box(
            DTOutput('bellmodded'),
            width = 4
          ),
          box(
            plotOutput('p', height = '1000px'),
            width = 8
          )
        )
      ),
      tabItem(
        tabName = "pie",
        fluidRow(
          box(
            title = "Race Distribution",
            status = "warning",
            solidHeader = TRUE,
            plotOutput('racepie')
          ),
          box(
            title = "Population Distribution",
            status = "warning",
            solidHeader = TRUE,
            plotOutput('populationpie')
          ),
          box(
            title = "Species Distribution",
            status = "warning",
            solidHeader = TRUE,
            plotOutput('speciespie')
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # why is this so simple it doesn't feel right
  main <- read.csv(file = "main.csv")

  output$altmap <- renderLeaflet({
    pal <- colorNumeric(palette = "OrRd",
                        domain = main$Altitude)
    
    leaflet(data = main) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 5,
                       group = "marker",
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillColor = ~pal(Altitude),
                       fillOpacity = 1,
                       popup = paste("<b>Species: </b>", main$Species, "<br/>",
                                     "<b>Race: </b>", main$Race, "<br/>",
                                     "<b>Population: </b>", main$Population)) %>%
      addLegend(position = "bottomright",
                pal = pal,
                values = ~Altitude,
                opacity = 1,
                title = "Sample Altitude (Meters)")
  })

  output$racemap <- renderLeaflet({
    pal <- colorFactor(palette = "Set3",
                       domain = main$Race)
    
    leaflet(data = main) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 5,
                       group = "marker",
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillColor = ~pal(Race),
                       fillOpacity = 1,
                       popup = paste("<b>Species: </b>", main$Species, "<br/>",
                                     "<b>Race: </b>", main$Race, "<br/>",
                                     "<b>Population: </b>", main$Population))
  })
  
  output$popmap <- renderLeaflet({
    pal <- colorFactor(palette = "Set3",
                       domain = main$Population)
    
    leaflet(data = main) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 5,
                       group = "marker",
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillColor = ~pal(Population),
                       fillOpacity = 1,
                       popup = paste("<b>Species: </b>", main$Species, "<br/>",
                                     "<b>Race: </b>", main$Race, "<br/>",
                                     "<b>Population: </b>", main$Population))
  })
  
  output$speciesmap <- renderLeaflet({
    pal <- colorFactor(palette = "Set3",
                       domain = main$Species)
    
    leaflet(data = main) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 5,
                       group = "marker",
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillColor = ~pal(Species),
                       fillOpacity = 1,
                       popup = paste("<b>Species: </b>", main$Species, "<br/>",
                                     "<b>Race: </b>", main$Race, "<br/>",
                                     "<b>Population: </b>", main$Population))
  })
  
  
  # PLEASE use the good ol' comma inside this thing
  # output$table <- renderDataTable(
  #   main,
  #   options = list(pageLength = 20)
  # )
  output$tableDT <- renderDT(
    main,
    options = list(pageLength = 15)
  )
  
  main %>%
    select(Race, Altitude) %>%
    group_by(Race) %>%
    summarize(Max = max(Altitude), Min = min(Altitude)) -> bellmod
  
  output$bellmodded <- renderDT(
    bellmod,
    options = list(pageLength = 15)
  )
  
  # getting rid of blank data
  bellmod <- subset(bellmod, nchar(Race) > 0)
  main2 <- subset(main, nchar(Race) > 0)
  
  output$p <- renderPlot({
    ggplot(bellmod) + 
    geom_segment(aes(x = Min, xend = Max, y = Race, yend = Race),
                 color = '#aeb6bf',
                 size = 3,
                 alpha = 0.5) +
    # geom_point(aes(x = Min, y = Race, color = '#42b6f5'),
    #            size = 4,
    #            show.legend = TRUE) +
    # geom_point(aes(x = Max, y = Race, color = '#e64c35'),
    #            size = 4)
    geom_point(data = main2,
               aes(x = Altitude, y = Race),
               color = '#001858') + 
    theme_classic()
  })
  
  # pie chart stuff
  main %>%
    select(Race) %>%
    group_by(Race) %>%
    mutate(Race = if_else(nchar(Race)==0, "Unspecified", Race)) %>%
    summarise(Count = n()) %>%
    mutate(Race = if_else(Count<4, "Others", Race)) %>%
    group_by(Race) %>%
    summarize(Count = if_else(Race=="Others", sum(Count), Count)) %>%
    summarize(Count = max(Count)) %>%
    group_by(Race) -> racemod
  
  output$racepie <- renderPlot(
    pie(
      x = racemod$Count,
      labels = racemod$Race,
      radius = 1,
      init.angle = 315
    )
  )
  
  main %>%
    select(Population) %>%
    group_by(Population) %>%
    mutate(Population = if_else(nchar(Population)==0, "Unspecified", Population)) %>%
    summarise(Count = n()) -> populationmod

  output$populationpie <- renderPlot(
    pie(
      x = populationmod$Count,
      labels = populationmod$Population,
      radius = 1,
      init.angle = 0
    )
  )
  
  main %>%
    select(Species) %>%
    group_by(Species) %>%
    mutate(Species = if_else(nchar(Species)==0, "Unspecified", Species)) %>%
    summarise(Count = n()) -> speciesmod
  
  output$speciespie <- renderPlot(
    pie(
      x = speciesmod$Count,
      labels = speciesmod$Species,
      radius = 1,
      init.angle = 0
    )
  )

  # router$server(input, output, session)
}


shinyApp(ui, server)