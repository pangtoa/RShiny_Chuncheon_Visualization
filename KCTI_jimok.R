library(shiny)
library(shinydashboard)
library(shinymanager)
library(rdrop2)
library(stringr)
library(readr)
library(sysfonts)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sf)
library(sp)


target_geom_elem <- 
  readRDS("./geom_elem.rds") %>% 
  filter(!is.na(col_adm_se)) %>% 
  mutate(address_reduced = str_replace_all(address_full, "경기도 가평군 ", "") %>% str_replace_all("강원도 춘천시 ", ""))

jimok_check_list <- unique(target_geom_elem$jimok) %>% sort()

Sys.setlocale("LC_ALL","Korean")

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
            menuItem("[지목구분] 별 시각화", tabName = "jimok_menu")
            )
        )


body <- 
    dashboardBody(
        tabItems(

            tabItem(tabName = "jimok_menu",
                    fluidRow(
                        box(title = strong("[지목구분] 별 시각화"),
                            solidHeader = FALSE,
                            color = "black",
                            status = "primary",
                            width = 12,
                            height = "1000",
                            column(11,leafletOutput("jimok_map", height = 800)),
                            column(1, 
                                   fluidRow(checkboxGroupInput("jimok_checkbox", "지목구분",
                                                         choices = jimok_check_list,
                                                         selected = jimok_check_list),
                                            actionLink("selectall","Select All")),
                                   fluidRow(actionButton("jimok_run", "적용"))
                                   )
                            )
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
    
    
    jimok_color_list <- c("purple", "brown", "brown1", "coral4", "darkgreen", 
                    "darkorange", "darkblue", "coral", "bisque", "red", 
                    "darkred", "gold", "deeppink", "lawngreen", "lightblue", 
                    "maroon1", "orange", "violet", "blue", "wheat", 
                    "yellow", "yellowgreen")
    
    jimok_list <- c('묘','유','임','공','과',
                    '구','답','대','도','원',
                    '잡','장','전','제','종',
                    '주','차','창','천','철',
                    '체','학')
    
    jimok_palette <- colorFactor(jimok_color_list, jimok_list)
    
    observe({
      if(input$selectall == 0) return(NULL) 
      else if (input$selectall%%2 == 0)
      {
        updateCheckboxGroupInput(session,"jimok_checkbox","지목구분", choices = jimok_check_list)
      }
      else
      {
        updateCheckboxGroupInput(session,"jimok_checkbox","지목구분", choices = jimok_check_list, selected = jimok_check_list)
      }
    })
    
    
    
    ## jimok
    

    jimok_map_activated <- 
      eventReactive(input$jimok_run, {
        
        checked_jimok_list <- input$jimok_checkbox
        
        target_mat <- 
          target_geom_elem %>% 
          filter(jimok %in% checked_jimok_list) %>% 
          select(jimok, address_reduced, geom_coord) %>% 
          dplyr::rename(geometry = geom_coord)
        target_df <- split(target_mat, target_mat$jimok)
        
        
        l <- leaflet() %>% addTiles() %>% addProviderTiles('CartoDB.Positron')
        
        names(target_df) %>%
          purrr::walk( function(target_jimok) {
            l <<- l %>%
              addPolygons(data =  st_sf(target_df[[target_jimok]]) %>% filter(!st_is_empty(.)), 
                          fillColor = ~ jimok_palette(jimok),
                          fillOpacity = 0.7,
                          weight = 0.3,
                          group = target_jimok,
                          popup = ~paste0(as.character(jimok), "(", address_reduced, ")"),
                          label = ~paste0(as.character(jimok), "(", address_reduced, ")"),
                          highlightOptions = highlightOptions(color = "red", weight = 2,
                                                              bringToFront = TRUE),
                          labelOptions = labelOptions(noHide = F,
                                                      direction = 'auto'))
            
            
          })
        
        l <- l %>% addLegend(position = "topright", 
                        pal = jimok_palette, 
                        values = jimok_list, 
                        opacity = 1, 
                        title = "지목구분")
        
        # checked_jimok_list <- input$jimok_checkbox
        # jimok_id_list <- split(target_geom_elem %>% select(id, jimok), target_geom_elem$jimok)
        # l <- leaflet() %>% addTiles() %>% addProviderTiles('CartoDB.Positron')
        # 
        # 
        # for (i in 1:22){
        #   if (jimok_list[i] %in% checked_jimok_list){
        #     
        #     target_mat <- target_geom_elem$geom_coord[jimok_id_list[[jimok_list[i]]][["id"]]]
        #     
        #     a <- st_sf(target_mat)
        #     b <- a %>% filter(!st_is_empty(.))
        #     
        #     l <- 
        #       l %>% 
        #       addPolygons(data = b, 
        #                   popup = jimok_list[i],
        #                   color = color_list[i],
        #                   weight = 0.5,
        #                   fillOpacity = 0.5,
        #                   highlightOptions = highlightOptions(color = "red", weight = 2,
        #                                                       bringToFront = TRUE))
        #   }
        #   
        # }
        # 
        
       return(l) 
        
      })
        
    
    output$jimok_map <- leaflet::renderLeaflet({ 
      
      
      jimok_map_result <- jimok_map_activated()
      jimok_map_result
      
    })
    
    
 
}

shinyApp(ui, server)



