library(shiny)
library(shinydashboard)
library(shinymanager)
library(readr)
library(sysfonts)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sf)

options("scipen"=100, "digits"=4)

target_geom_elem <- readRDS("./geom_elem.rds") %>% filter(!is.na(col_adm_se))


region_check_list <- unique(target_geom_elem$region)
owner_check_list <- unique(target_geom_elem$owner)
jimok_check_list <- unique(target_geom_elem$jimok)

Sys.setlocale("LC_ALL","Korean")

tour_loclat <- read_csv("https://www.dropbox.com/s/i0j7ustoanoh0fx/tour_loclat.csv?dl=1")

header <- 
    dashboardHeader(
        title =  "GIS Visualizer",
        tags$li(a(href = 'https://www.shinyapps.io/',
                  title = "Shiny Apps"),
                class = "dropdown")
        
    )


sidebar <- 
    dashboardSidebar(
        sidebarMenu(
            menuItem("관광시설(대분류) 시각화", tabName = "destination_menu_1"),
            menuItem("관광시설(상세분류) 시각화", tabName = "destination_menu_2")
            )
        )


body <- 
    dashboardBody(
        tabItems(
            tabItem(tabName = "destination_menu_1",
                    fluidRow(
                        box(title = strong("[관광시설(대분류)]  시각화"),
                            solidHeader = FALSE,
                            color = "black",
                            status = "primary",
                            width = 12,
                            height = "1000",
                            leafletOutput("destination_map1", height = 800))
                        )
                    ),
            tabItem(tabName = "destination_menu_2",
                    fluidRow(
                      box(title = strong("[관광시설(상세분류)] 별 시각화"),
                          solidHeader = FALSE,
                          color = "black",
                          status = "primary",
                          width = 12,
                          height = "1000",
                          leafletOutput("destination_map2", height = 800))
                    )
            )

        ) 
    ) # dashboardBody


ui <- 
    dashboardPage(
        title = "KCTI",
        header,
        sidebar,
        body,
        skin = "black")

server <- function(input, output, session) {
    
  destination.map1 <- reactive({
    
    category_1color_list <- c("purple", "red", "blue", "darkgreen", "deeppink", 
                              "darkorange")
    
    address.df <- split(tour_loclat, tour_loclat$category_1)
    pal_gg <- colorFactor(category_1color_list, tour_loclat$category_1)
    
    l <- 
      leaflet() %>% 
      addTiles() %>% 
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(data = target_geom_elem$geom_coord,
                  weight = 0.1,
                  color = "red")
    
    names(address.df) %>%
      purrr::walk( function(df) {
        l <<- l %>%
          addCircleMarkers(data=address.df[[df]],
                           lng=~lon, lat=~lat,
                           label=~paste0(as.character(category_1), " (", as.character(name), "/", as.character(address), ")"),
                           popup=~paste0(as.character(category_1), " (", as.character(name), "/", as.character(address), ")"),
                           radius=2,
                           opacity = 1,
                           fillOpacity = 1,
                           group=df,
                           color=~pal_gg(category_1),
                           labelOptions = labelOptions(noHide = F,
                                                       direction = 'auto'))
      })
    
    
    l %>%
      addLayersControl(
        overlayGroups = names(address.df),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% addLegend(position = "bottomright", 
                      pal = pal_gg, 
                      values = names(address.df), 
                      opacity = 1, 
                      title = "시설분류(대분류)")
    
  })
  
  output$destination_map1 <- leaflet::renderLeaflet({ destination.map1() })
  
  
  # destination map2

  destination.map2 <- reactive({
    
    address.df <- split(tour_loclat, tour_loclat$category_2)
    
    category_2color_list <- c("purple", "brown", "yellow", "coral4", "darkgreen", 
                              "red", "darkblue", "blue", "bisque", "yellow", 
                              "darkred", "gold", "deeppink")

    pal_gg <- colorFactor(category_2color_list, tour_loclat$category_2)

    l <- 
      leaflet() %>% 
      addTiles() %>% 
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(data = target_geom_elem$geom_coord,
                  weight = 0.1,
                  color = "red")

    names(address.df) %>%
        purrr::walk( function(df) {
            l <<- l %>%
                addCircleMarkers(data=address.df[[df]],
                                 lng=~lon, lat=~lat,
                                 label=~paste0(as.character(category_2), " (", as.character(name), "/", as.character(address), ")"),
                                 popup=~paste0(as.character(category_2), " (", as.character(name), "/", as.character(address), ")"),
                                 radius=2,
                                 opacity = 1,
                                 fillOpacity = 1,
                                 group=df,
                                 color=~pal_gg(category_2),
                                 labelOptions = labelOptions(noHide = F,
                                                             direction = 'auto'))
        })


    l %>%
        addLayersControl(
            overlayGroups = names(address.df),
            options = layersControlOptions(collapsed = FALSE)
        ) %>% addLegend(position = "bottomright", 
                        pal = pal_gg, 
                        values = names(address.df), 
                        opacity = 1, 
                        title = "시설분류(세분류)")

  })

  output$destination_map2 <- leaflet::renderLeaflet({ destination.map2() })
 
}

shinyApp(ui, server)
