options(shiny.fullstacktrace = FALSE)

library(shiny.fluent) #MS Fluent UI wrapper
library(shiny) #base
library(tidyverse) #reshaping, etc.
library(glue) #stringing together commands with glue()
library(leaflet) #create maps
library(leaflet.extras) #setMapWidgetStyle()
library(plotly) #create plots
library(sf) #shapefile processing
library(DT) #for the data tables. Not used for now, keep if I use DT as originally planned
library(readxl) #for reading in the Excel data
#library(forcats) #tidyverse package for handling factors
library(shiny.i18n) #internationalization
library(shiny.router) #page control
library(rgdal)
library(raster) #GADM shapefile pull, among others
library(shinyjs) #for JS functionality including the file uploads. Not used for now, but will be once this is transitioned.
library(htmltools) #for label popups
library(gghighlight) #for selection highlighting in ggplot
library(waffle) #for waffleplots. Requires v. 1.0+. If only 0.7.0 installs, use: install.packages("waffle", repos = "https://cinc.rud.is")

#library(lato)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(png)
library(ggpubr)
library(showtext)
library(gridtext)
library(grid)

# For dot density functionality, remove any not used
#library(rgdal) #already loaded
#library(maptools) #dotsInPolys() used to place the dots in the dot density polygons
#(rgeos) #gCentroid() used to derive a centroid for each polygon
#library(tidyverse) #already loaded
library(broom)
library(png)
library(rmarkdown)
#library(sp) #likely duplicative with sf

source("first_page.R")
source("second_page.R")
source("components.R")

# font_add_google(name = "Lato", family = "Lato", regular.wt = 400, bold.wt = 700)
# showtext_auto()

theme_plot <- function(...) {
  theme(
    
    plot.title.position = "plot",
    text = element_text(family = "Lato"),
    
    # background colors
    plot.background = element_rect(fill = "transparent",
                                   color = NA),
    panel.background = element_rect(fill = "transparent",
                                    color = NA),
    legend.background = element_rect(fill = "transparent",
                                     color = NA),
    
    # titles
    legend.text = element_text(size = 14, 
                               color = "black",
                               family = "Lato"),
    plot.title = element_text(size = 14, 
                              color = "black"),
    plot.subtitle = element_text(size = 10, 
                                 color = "black"),
    ...
  )
}

#LOAD THE DATA##################################################################################################################
imported_data <- read_excel("data/GTMCenso_reprex.xlsx", sheet = "Population")

imported_data_wide_all <- read_excel("data/GTMCenso_indicator_widgets_reprex.xlsx", sheet = "Population")
imported_data <- imported_data %>%
  dplyr::mutate(secondaryText = Type)

imported_data_for_join_2 <- imported_data_wide_all %>% filter(Level == "ADM2")

sf_data_2 <- as(st_read("www/GTM_adm2.shp"), "Spatial")

tempjoin2 <- sp::merge(sf_data_2, imported_data_for_join_2, by.x = "ID_2", by.y = "AlphaJoinC")

shapefile(tempjoin2, filename = "www/join_output_2.shp", overwrite = TRUE)

sf2 <- as(st_read("www/join_output_2.shp"), "Spatial")

ADM1s <- list(
  list(key = "All", text = "Total"),
  list(key = "Alta Verapaz", text = "Alta Verapaz"),
  list(key = "Baja Verapaz", text = "Baja Verapaz"),
  list(key = "Chimaltenango", text = "Chimaltenango"),
  list(key = "Chiquimula", text = "Chiquimula"),
  list(key = "El Progreso", text = "El Progreso"),
  list(key = "Escuintla", text = "Escuintla"),
  list(key = "Guatemala", text = "Guatemala"),
  list(key = "Huehuetenango", text = "Huehuetenango"),
  list(key = "Izabal", text = "Izabal"),
  list(key = "Jalapa", text = "Jalapa"),
  list(key = "Jutiapa", text = "Jutiapa"),
  list(key = "Petén", text = "Petén"),
  list(key = "Quetzaltenango", text = "Quetzaltenango"),
  list(key = "Quiché", text = "Quiché"),
  list(key = "Retalhuleu", text = "Retalhuleu"),
  list(key = "Sacatepéquez", text = "Sacatepéquez"),
  list(key = "San Marcos", text = "San Marcos"),
  list(key = "Santa Rosa", text = "Santa Rosa"),
  list(key = "Sololá", text = "Sololá"),
  list(key = "Suchitepéquez", text = "Suchitepéquez"),
  list(key = "Totonicapán", text = "Totonicapán"),
  list(key = "Zacapa", text = "Zacapa")
)

#INTERNATIONALIZATION##################################

i18n <- Translator$new(translation_json_path='translations/translation.json')

i18n$set_translation_language('English')

lang_options <- list(
  list(key = "English", text = "English"),
  list(key = "Spanish", text = "Spanish")
)

#MAKE HEADER, FOOTER, AND SIDENAV##################################

header <- tagList(
  div(
    tags$div(
      Dropdown.shinyInput("language", options = lang_options, value = "English")
    ))
)

footer <- function(i18n) {
  Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", i18n$t("Created in OSDS"), block=TRUE),
  Text(variant = "medium", nowrap = FALSE, i18n$t("If you'd like to learn more, reach out to us at NSO@DOMAIN.GOV")),
  Text(variant = "medium", nowrap = FALSE, i18n$t("All rights reserved."))
)}

sidenav <- function(i18n) {
  Nav(
  groups = list(
    list(links = list(
      list(name = i18n$t("Public-facing elements"),
           expandAriaLabel = "Expand section",
           collapseAriaLabel = "Collapse section",
           links = list(
             list(name = i18n$t('Demographic and Social'), url = '#!/', key = 'sidebar_first', icon = 'Group'),
             list(name = i18n$t('Disability Status'), url = '#!/2', key = 'sidebar_second', icon = 'Glasses')
           )
      )))
  ),

  initialSelectedKey = 'sidebar_first',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)}

#SIDE DISPLAY########################################################

sidedisplay <- function(i18n) {
  Stack(
  tokens = list(childrenGap = 10),
  makeCard(i18n$t("Controls"),
           Stack(
             Label(i18n$t("Select a location")),
             Dropdown.shinyInput("dropdown", value = "All", options = ADM1s),
             br(),
             conditionalPanel(
               condition = "output.current_page == 2",
               br(),
               Label(i18n$t("Select a subtopic")),
               nav_picker_secondPage
             )
           ),
           size = 11)
)}

#SETUP THE ROUTER FUNCTIONALITY######################################

router <- make_router(
  route("/", firstPage(i18n)),
  route("2", secondPage(i18n))
)

# Add shiny.router dependencies manually: they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

#SETUP LAYOUT#######################################################

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", 
          tagList(
            header
            )),
      div(class = "sidenav", 
          tagList(
            sidenav(i18n)
            )),
      div(class = "main", mainUI),
      div(class = "sidedisplay", 
          tagList(
            sidedisplay(i18n)
            )),
      div(class = "footer", 
          tagList(
            footer(i18n)
            ))
  )
}

#UI##############################################################################w###############################################

ui <- fluentPage(
  useShinyjs(),
  shiny.i18n::usei18n(i18n),
  layout(router$ui),
  tags$head(
    includeCSS("./www/style.css"),
    shiny_router_script_tag
  ),
  tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lato');
                    "))
)

#SERVER#############w############################################################w###############################################

server <- function(input, output, session) {
  
  # observeEvent(input$language, { # translation, server side
  #   update_lang(session, input$language)
  # })
  
  router$server(input, output, session)
  
  output$current_page <- renderText({
    page <- get_page(session)
  })
  
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)
  
  r <- reactiveValues(
    secondPage_focused_dataset = imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Walking" & Level == "ADM1")
  )  
  
  tr <- reactiveVal()
  
  #FILTER DATA TABLES#################################################  
  #Combine as many of these as possible
  
  filtered_data_from_dropdown_ind_widgets <- reactive({
    req(input$dropdown)
    if (input$dropdown == "All") filtered_data <- imported_data_wide_all %>%
        dplyr::filter(Level == "ADM0")
    else filtered_data <- imported_data_wide_all %>%
        dplyr::filter(Level == "ADM1"& ADM1 == input$dropdown)
  })
  
  dropdownFilteredData <- reactive({
    req(input$dropdown)
    if (input$dropdown == "All") filtered_data <- imported_data %>%
        dplyr::filter(Level == "ADM0")
    else filtered_data <- imported_data %>%
        dplyr::filter(ADM1 == input$dropdown)
  })
  
  #RENDER TEXT FOR TOP INDICATOR WIDGETS##############################   
  
  name_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$Name
  })
  
  output$firstPage_statistics_text0 <- renderText({
    name_indicator()
  })
  
  firstPage_stat1 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$Pop
  })
  
  firstPage_stat2 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$PopDensity
  })
  
  output$firstPage_statistics_text1 <- renderText({
    paste(
      firstPage_stat1(),
      i18n$t("people"))
  })
  
  output$firstPage_statistics_text2 <- renderText({
    paste(
      firstPage_stat2(),
      i18n$t("people per square kilometer"))
  })
  
  output$secondPage_statistics_text0 <- renderText({
    name_indicator()
  })
  
  disability_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$Dis
  })
  
  output$secondPage_statistics_text1 <- renderText({
    paste(
      disability_indicator(),
      i18n$t("people"))
  })
  
  #FOCUS BUTTONS######################################################
  
  #second page
  observeEvent(input$walking_button, {
    r$secondPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Walking" & Level == "ADM1")
  })
  
  observeEvent(input$seeing_button, {
    r$secondPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Seeing" & Level == "ADM1")
  })
  
  observeEvent(input$hearing_button, {
    r$secondPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Hearing" & Level == "ADM1")
  })
  
  observeEvent(input$cognition_button, {
    r$secondPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Cognition" & Level == "ADM1")
  })
  
  observeEvent(input$selfcare_button, {
    r$secondPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Selfcare" & Level == "ADM1")
  })
  
  observeEvent(input$communication_button, {
    r$secondPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Communication" & Level == "ADM1")
  })
  
  #LEAFLET############################################################  
  
  first_page_map_bins <- c(0, 20000, 40000, 60000, 80000, 100000, Inf)
  
  output$first_page_map <- renderLeaflet({
    
    first_page_map_pal <- colorBin("YlGn", domain = sf2@data$Pop, bins = first_page_map_bins)
    
    first_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf2@data$NAME_1, sf2@data$Pop, i18n$t("people")) %>% 
      lapply(htmltools::HTML)
    
    first_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf2,
                  fillColor = ~first_page_map_pal(Pop),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = first_page_map_labels) %>%
      addLegend(pal=first_page_map_pal, values = sf2@data$Pop, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  
  # #second page maps
  
  second_page_map_bins <- c(6, 8, 10, 12, 14, Inf)
  
  output$second_page_map <- renderLeaflet({
    
    second_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$SecondShow, bins = second_page_map_bins)
    
    second_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf2@data$NAME_1, sf2@data$SecondShow, i18n$t("percent prevalence, people with at least some difficulty walking")) %>% 
      lapply(htmltools::HTML)
    
    second_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf2,
                  fillColor = ~second_page_map_pal(SecondShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = second_page_map_labels) %>%
      addLegend(pal=second_page_map_pal, values = sf2@data$SecondShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  
  observeEvent(input$walking_button, {
    
    sf2@data$SecondShow <- sf2@data$WalkRate
    
    second_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$SecondShow, bins = second_page_map_bins)
    
    second_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf2@data$NAME_1, sf2@data$SecondShow, i18n$t("percent prevalence, people with at least some difficulty walking")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("second_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~second_page_map_pal(SecondShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = second_page_map_labels) %>%
      addLegend(pal=second_page_map_pal, values = ~sf2@data$SecondShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$seeing_button, {
    
    sf2@data$SecondShow <- sf2@data$SeeRate
    
    second_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$SecondShow, bins = second_page_map_bins)
    
    second_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf2@data$NAME_1, sf2@data$SecondShow, i18n$t("percent prevalence, people with at least some difficulty seeing")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("second_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~second_page_map_pal(SecondShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = second_page_map_labels) %>%
      addLegend(pal=second_page_map_pal, values = ~sf2@data$SecondShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$hearing_button, {
    
    sf2@data$SecondShow <- sf2@data$HearRate
    
    second_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$SecondShow, bins = second_page_map_bins)
    
    second_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf2@data$NAME_1, sf2@data$SecondShow, i18n$t("percent prevalence, people with at least some difficulty hearing")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("second_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~second_page_map_pal(SecondShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = second_page_map_labels) %>%
      addLegend(pal=second_page_map_pal, values = ~sf2@data$SecondShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$cognition_button, {
    
    sf2@data$SecondShow <- sf2@data$CogRate
    
    second_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$SecondShow, bins = second_page_map_bins)
    
    second_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf2@data$NAME_1, sf2@data$SecondShow, i18n$t("percent prevalence, people with at least some cognition difficulty")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("second_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~second_page_map_pal(SecondShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = second_page_map_labels) %>%
      addLegend(pal=second_page_map_pal, values = ~sf2@data$SecondShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$selfcare_button, {
    
    sf2@data$SecondShow <- sf2@data$SelfcaRate
    
    second_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$SecondShow, bins = second_page_map_bins)
    
    second_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf2@data$NAME_1, sf2@data$SecondShow, i18n$t("percent prevalence, people with at least some difficulty with selfcare")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("second_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~second_page_map_pal(SecondShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = second_page_map_labels) %>%
      addLegend(pal=second_page_map_pal, values = ~sf2@data$SecondShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$communication_button, {
    
    sf2@data$SecondShow <- sf2@data$CommRate
    
    second_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$SecondShow, bins = second_page_map_bins)
    
    second_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      sf2@data$NAME_1, sf2@data$SecondShow, i18n$t("percent prevalence, people with at least some difficulty with communication")) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("second_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~second_page_map_pal(SecondShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = second_page_map_labels) %>%
      addLegend(pal=second_page_map_pal, values = ~sf2@data$SecondShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  #PLOTS##############################################################
  
  output$firstPage_Pop_ADM1Plot <- renderPlot({
    
    req(input$dropdown)
    
    internal_df <- imported_data %>%
      filter(Metric == "Population" & Level == "ADM1" & Type == "Total")
    
    a <- internal_df %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#41AB5D", stat = "identity") +
      xlab(i18n$t("Department")) +
      theme(legend.position = "none") +
      ylab(i18n$t("People")) +
      theme_plot() +
      ggtitle(i18n$t("Population by Department")) +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }

  })
  
  output$firstPage_Sex <- renderPlot({
    sexPlot <- dropdownFilteredData() %>%
      
      filter(Metric == "Population" & Type %in% c("Male", "Female")) %>%
      mutate(Sex = fct_relevel(Type, "Female", "Male")) %>%
      ggplot(aes(x = Type, y = Value, label = secondaryText)) +
      geom_bar(stat = "identity") +
      ggtitle(i18n$t("Population by Sex")) +
      ylab(i18n$t("People")) +
      xlab(i18n$t("Sex")) +
      theme_plot() +
      coord_flip()
    
    sexPlot
  })
  
  output$firstPage_Age <- renderPlot({
    agePlot <- dropdownFilteredData() %>%
      filter(Metric == "Population" & Type %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+")) %>%
      mutate(Type = fct_relevel(Type, "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+")) %>%
      ggplot(aes(x = secondaryText, y = Value)) +
      geom_bar(stat = "identity") +
      ggtitle(i18n$t("Population by Age")) +
      ylab(i18n$t("People")) +
      xlab(i18n$t("Age")) +
      theme_plot() +
      coord_flip()

    agePlot
  })
  
  output$firstPage_UrbanRural <- renderPlot({
    urbanRuralPlot <- dropdownFilteredData() %>%
      filter(Metric == "Population" & Type %in% c("Urban", "Rural")) %>%
      mutate(UrbanRural = fct_relevel(UrbanRural, "Urban", "Rural")) %>%
      ggplot(aes(x = Type, y = Value, label = secondaryText)) +
      geom_bar(stat = "identity") +
      ggtitle(i18n$t("Population by Urban / Rural Residence")) +
      ylab(i18n$t("People")) +
      xlab(i18n$t("Urban / Rural Status")) +
      theme_plot() +
      coord_flip()
    
    urbanRuralPlot
  })
  
  output$secondPage_Dis_ADM1Plot <- renderPlot({
    
    internal <- r$secondPage_focused_dataset['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "Walking") {
      plotTitle <- i18n$t("Prevalence Rate of Population with at Least Some \nDifficulty in Walking by Department")
    } else if (internal == "Seeing") {
      plotTitle <- i18n$t("Prevalence Rate of Population with at Least Some \nDifficulty in Seeing by Department")
    } else if (internal == "Hearing") {
      plotTitle <- i18n$t("Prevalence Rate of Population with at Least Some \nDifficulty in Hearing by Department")
    } else if (internal == "Cognition") {
      plotTitle <- i18n$t("Prevalence Rate of Population with at Least Some \nCognition Difficulty by Department")
    } else if (internal == "Selfcare") {
      plotTitle <- i18n$t("Prevalence Rate of Population with at Least Some \nDifficulty With Selfcare by Department")
    } else if (internal == "Communication") {
      plotTitle <- i18n$t("Prevalence Rate of Population with at Least Some \nDifficulty With Communication by Department")
    } else {
      plotTitle <- internal
    }
    
    a <- r$secondPage_focused_dataset %>%
      filter(Level == "ADM1") %>%
      filter(Type == "DisabilityRate") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#F05131", stat = "identity") +
      xlab(i18n$t("Department")) +
      theme(legend.position = "none") +
      ylab(i18n$t("Prevalence Rate (Percent)")) +
      theme_plot() +
      ggtitle(plotTitle) +
      coord_flip()
    
    if(input$dropdown != "All") {
      a <- a  + gghighlight(ADM1 == input$dropdown)
      a
    } else {
      a
    }
    
  })
  
  #TRANSLATION########################################################
  
  observeEvent(input$language, {
    lang <- input$language
    update_lang(session, lang)
    i18n$set_translation_language(lang)
    tr(i18n)
  })
  
  observe({
    req(tr())
    newOptions <- purrr::map(lang_options, ~ list(key = .x$key, text = tr()$t(.x$text)))
    updateDropdown.shinyInput(session = session, inputId = "language", options = newOptions)
    imported_data <- imported_data %>%
      dplyr::mutate(secondaryText = tr()$t(secondaryText))
  })
  

}



shinyApp(ui, server)
