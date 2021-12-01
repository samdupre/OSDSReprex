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
library(maptools) #dotsInPolys() used to place the dots in the dot density polygons
library(rgeos) #gCentroid() used to derive a centroid for each polygon
#library(tidyverse) #already loaded
library(broom)
library(png)
library(rmarkdown)
#library(sp) #likely duplicative with sf

source("first_page.R")
source("second_page.R")

logo <- png::readPNG("./logo-placeholder.png")

font_add_google(name = "Lato", family = "Lato", regular.wt = 400, bold.wt = 700)
showtext_auto()

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
imported_data <- read_excel("data/GTMCenso.xlsx", sheet = "Population")
imported_data_wide_all <- read_excel("data/GTMCenso_indicator_widgets.xlsx", sheet = "Population")
imported_migration_data <- read_excel("data/GTMCenso_Migration.xlsx", sheet = "Place1YearAgo")
imported_data_for_join_0 <- imported_data_wide_all %>% filter(Level == "ADM0")
imported_data_for_join_1 <- imported_data_wide_all %>% filter(Level == "ADM1")
imported_data_for_join_2 <- imported_data_wide_all %>% filter(Level == "ADM2")

# imported_migration_data_net <- imported_migration_data %>% filter(Flow == "Net")
# imported_migration_data_in <- imported_migration_data %>% filter(Flow == "Inbound")
# imported_migration_data_out <- imported_migration_data %>% filter(Flow == "Outbound")

sf_data_0 <- as(st_read("www/GTM_adm0.shp"), "Spatial")
sf_data_1 <- as(st_read("www/GTM_adm1.shp"), "Spatial")
sf_data_2 <- as(st_read("www/GTM_adm2.shp"), "Spatial")

tempjoin0 <- sp::merge(sf_data_0, imported_data_for_join_0, by.x = "NAME_0", by.y = "ADM0")
tempjoin1 <- sp::merge(sf_data_1, imported_data_for_join_1, by.x = "ID_1", by.y = "AlphaJoinC")
tempjoin2 <- sp::merge(sf_data_2, imported_data_for_join_2, by.x = "ID_2", by.y = "AlphaJoinC")

# tempjoin_migration_net <- sp::merge(sf_data_1, imported_migration_data_net, by.x = "ID_1", by.y = "AlphaJoinC")
# tempjoin_migration_in <- sp::merge(sf_data_1, imported_migration_data_in, by.x = "ID_1", by.y = "AlphaJoinC")
# tempjoin_migration_out <- sp::merge(sf_data_1, imported_migration_data_out, by.x = "ID_1", by.y = "AlphaJoinC")

shapefile(tempjoin0, filename = "www/join_output_0.shp", overwrite = TRUE)
shapefile(tempjoin1, filename = "www/join_output_1.shp", overwrite = TRUE)
shapefile(tempjoin2, filename = "www/join_output_2.shp", overwrite = TRUE)

# shapefile(tempjoin_migration_net, filename = "www/join_output_mig_net.shp", overwrite = TRUE)
# shapefile(tempjoin_migration_in, filename = "www/join_output_mig_in.shp", overwrite = TRUE)
# shapefile(tempjoin_migration_out, filename = "www/join_output_mig_out.shp", overwrite = TRUE)

sf0 <- as(st_read("www/join_output_0.shp"), "Spatial")
sf1 <- as(st_read("www/join_output_1.shp"), "Spatial")
sf2 <- as(st_read("www/join_output_2.shp"), "Spatial")

sf_mig <- sf1

# sf() for automated report outputs
sf_ADM_0 <- st_as_sf(st_read("./www/join_output_0.shp"), "Spatial")
sf_ADM_1 <- st_as_sf(st_read("./www/join_output_1.shp"), "Spatial")
sf_ADM_2 <- st_as_sf(st_read("./www/join_output_2.shp"), "Spatial")

# sf_mig_net <- as(st_read("www/join_output_mig_net.shp"), "Spatial")
# sf_mig_in <- as(st_read("www/join_output_mig_in.shp"), "Spatial")
# sf_mig_out <- as(st_read("www/join_output_mig_out.shp"), "Spatial")

#Adjust this to programmatically match keys to avoid having to name all the below individually
# ADM1s <- unique(imported_data_for_join_2$ADM1)

ADM1s <- list(
  list(key = "All", text = "All Areas"),
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

# #DOT DENSITY MAP PREP##################################
# 
# #Create list of ADM1s
# 
# names_ADM1 <- unique(imported_data_for_join_2$ADM1)
# 
# #Create dataset
# 
# selected.regions <- lapply(names_ADM1, function(x) {
#   region <- sf2[sf2$NAME_1 == x,]
# })
# 
# names(selected.regions) <- names_ADM1
# 
# #Create centroids
# 
# centroids <- lapply(1:length(selected.regions), function(i) {
#   data.frame(gCentroid(selected.regions[[i]]))
# })
# names(centroids) <- names_ADM1
# 
# #Create dot set
# 
# pop_dots <- lapply(1:length(selected.regions), function(i) {
# 
#   num.dots <- dplyr::select(selected.regions[[i]]@data, Pop) / 5000
# 
#   sp.dfs <- lapply(names(num.dots), function(x) {
#     dotsInPolys(selected.regions[[i]], as.integer(num.dots[, x]), f="random")
#   })
# 
#   dfs <- lapply(sp.dfs, function(x) {
#     data.frame(coordinates(x)[,1:2])
#   })
# 
#   dots.final <- bind_rows(dfs)
# 
#   return(dots.final)
# })
# names(pop_dots) <- names_ADM1

pop_dots_data <- do.call("rbind", pop_dots)

sf_pop_dots <- st_as_sf(pop_dots_data, coords = c("x", "y"), crs = 4326)

#INTERNATIONALIZATION##################################

i18n <- Translator$new(translation_json_path='translations/translation.json')

i18n$set_translation_language('English')

#MAKE HEADER, FOOTER, AND SIDENAV##################################

header <- tagList(
  div(
    tags$div(
      selectInput(
        inputId='selected_language',
        label=i18n$t('Change language'),
        choices = i18n$get_languages(),
        selected = i18n$get_key_translation()
      )
    ))
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Powered by OSDS", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at NSO@DOMAIN.GOV"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)

sidenav <- Nav(
  groups = list(
    list(links = list(
      list(name = 'UPLOAD', url = '#!/t1', key = 'sidebar_main_first', icon = 'SwitcherStartEnd'),
      list(name = 'ANALYTICS', url = '#!/t2', key = 'sidebar_main_second', icon = 'SeeDo'),
      list(name = 'PUBLISH', url = '#!/t3', key = 'sidebar_main_third', icon = 'Globe'),
      list(name = 'TECHNICAL BRIEFS', url = '#!/t4', key = 'sidebar_main_fourth', icon = 'Ribbon'),
      list(name = "Public-facing elements",
           expandAriaLabel = "Expand section",
           collapseAriaLabel = "Collapse section",
           links = list(
             list(name = 'Home', url = '#!/', key = 'sidebar_first', icon = 'Home'),
             list(name = 'Demographic & Social', url = '#!/1', key = 'sidebar_second', icon = 'Group'),
             list(name = 'Household & Family', url = '#!/2', key = 'sidebar_third', icon = 'Street'),
             list(name = 'Disability Status', url = '#!/3', key = 'sidebar_fourth', icon = 'Glasses'),
             list(name = 'International Migration', url = '#!/4', key = 'sidebar_fifth', icon = 'AnalyticsQuery'),
             list(name = 'Domestic Migration', url = '#!/5', key = 'sidebar_sixth', icon = 'World'),
             list(name = 'Mortality', url = '#!/6', key = 'sidebar_seventh', icon = 'Health'),
             list(name = 'Fertility', url = '#!/7', key = 'sidebar_eighth', icon = 'Family'),
             list(name = 'Education', url = '#!/8', key = 'sidebar_ninth', icon = 'Education'),
             list(name = 'Labour & Economy', url = '#!/9', key = 'sidebar_tenth', icon = 'BacklogBoard'),
             list(name = 'Housing', url = '#!/10', key = 'sidebar_eleventh', icon = 'Home'),
             list(name = 'Water, Sanitation & Hygiene', url = '#!/11', key = 'sidebar_twelfth', icon = 'Precipitation'),
             list(name = 'Power & Technology', url = '#!/12', key = 'sidebar_thirteenth', icon = 'LightningBolt')
           )
      )))
  ),
  # IF this doesn't work, change sidebar_main_first to home
  initialSelectedKey = 'sidebar_main_first',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

#SIDE DISPLAY########################################################

sidedisplay <- Stack(
  tokens = list(childrenGap = 10),
  makeCard(i18n$t("Controls"),
           Stack(
             Label("Select a location"),
             Dropdown.shinyInput("dropdown", value = "All", options = ADM1s),
             br(),
             conditionalPanel(
               condition = "output.current_page == 3",
               br(),
               Label("Select a subtopic"),
               nav_picker_thirdPage
             ),
             conditionalPanel(
               condition = "output.current_page == 4",
               br(),
               Label("Select a subtopic"),
               nav_picker_fourthPage
             ),
             conditionalPanel(
               condition = "output.current_page == 5",
               br(),
               Label("Select a metric"),
               Dropdown.shinyInput("fifthPage_variable", 
                                   value = "Net", 
                                   options = list(
                                     list(key = "Net", text = "Net"),
                                     list(key = "Outbound", text = "Outbound"),
                                     list(key = "Inbound", text = "Inbound")
                                   )),
               br()
             ),
             conditionalPanel(
               condition = "output.current_page == 8",
               br(),
               Label("Select a metric"),
               Dropdown.shinyInput("eighthPage_variable", 
                                   value = "AdultLiteracyRate", 
                                   options = list(
                                     list(key = "AdultLiteracyRate", text = "Adult Literacy"),
                                     list(key = "YouthLiteracyRate", text = "Child Literacy")
                                   ))
             ),
             conditionalPanel(
               condition = "output.current_page == 11",
               br(),
               Label("Select a subtopic"),
               nav_picker_eleventhPage
             ),
             conditionalPanel(
               condition = "output.current_page == 12",
               br(),
               Label("Select a subtopic"),
               nav_picker_twelfthPage
             )
             # ,
             # Label("Map zoom control"),
             # Dropdown.shinyInput("dropdown_dots", value = "Alta Verapaz", options = ADM1s_dots)
           ),
           size = 11)
)

#USE MAKEPAGE() TO MAKE BODY PAGES##################################

#UPLOAD
uploadPage <- makePage(
  "Data Upload",
  "Upload your census data and spatial data to the portal",
  div(
    uploadPage_background(),
    uploadPage_census_data_upload(),
    uploadPage_spatial_data_upload(),
    uploadPage_join()
  )
)

#ANALYTICS
analyticsPage <- makePage(
  "Census Analytics",
  "Explore the data on your portal",
  div(
    analyticsPage_outliers(),
    analyticsPage_contentcheck(),
    analyticsPage_cellflagcheck()
  )
)

#PUBLISH
publishPage <- makePage(
  "Publication and Export",
  "Export data products",
  div(
    publishPage_hosting(),
    publishPage_reports(),
    publishPage_graphics(),
    publishPage_data()
  )
)

#BRIEFS
briefsPage <- makePage(
  "Technical Briefs",
  "Explore guidance on census topics",
  div(
    briefsPage_picker(),
    briefsPage_viewer()
  )
)

#FIRST
firstPage <- makePage(
  i18n$t('Demography & Social Characteristics'),
  "Explore each subtopic",
  div(
    firstPage_first_stack_content(),
    firstPage_second_stack_content()
  )
)

#SECOND
secondPage <- makePage(
  i18n$t('Household & Family Characteristics'),
  "Explore each subtopic",
  div(
    secondPage_first_stack_content(),
    secondPage_second_stack_content()
  )
)

#THIRD
thirdPage <- makePage(
  i18n$t('Disability Status'),
  "Explore each subtopic",
  div(
    thirdPage_first_stack_content(),
    thirdPage_second_stack_content()
  )
)

#FOURTH
fourthPage <- makePage(
  i18n$t('International Migration'),
  "Explore each subtopic",
  div(
    fourthPage_first_stack_content(),
    fourthPage_second_stack_content()
  )
)

#FIFTH
fifthPage <- makePage(
  i18n$t('Domestic Migration'),
  "Explore each subtopic",
  div(
    fifthPage_first_stack_content(),
    fifthPage_second_stack_content()
  )
)

#SIXTH
sixthPage <- makePage(
  i18n$t('Mortality'),
  "Explore each subtopic",
  div(
    sixthPage_first_stack_content(),
    sixthPage_second_stack_content()
  )
)

#SEVENTH
seventhPage <- makePage(
  i18n$t('Fertility'),
  "Explore each subtopic",
  div(
    seventhPage_first_stack_content(),
    seventhPage_second_stack_content()
  )
)

#EIGHTH
eighthPage <- makePage(
  i18n$t('Education'),
  "Explore each subtopic",
  div(
    eighthPage_first_stack_content(),
    eighthPage_second_stack_content()
  )
)

#NINTH
ninthPage <- makePage(
  i18n$t('Labour and Economy'),
  "Explore each subtopic",
  div(
    ninthPage_first_stack_content(),
    ninthPage_second_stack_content()
  )
)

#TENTH
tenthPage <- makePage(
  i18n$t('Housing'),
  "Explore each subtopic",
  div(
    tenthPage_first_stack_content(),
    tenthPage_second_stack_content(),
    tenthPage_third_stack_content(),
    tenthPage_fourth_stack_content()
  )
)

#ELEVENTH
eleventhPage <- makePage(
  i18n$t('Water, Sanitation, and Hygiene'),
  "Explore each subtopic",
  div(
    eleventhPage_first_stack_content(),
    eleventhPage_second_stack_content()
  )
)

#TWELFTH
twelfthPage <- makePage(
  i18n$t('Power & Technology'),
  "Explore each subtopic",
  div(
    twelfthPage_first_stack_content(),
    twelfthPage_second_stack_content()
  )
)

#SETUP THE ROUTER FUNCTIONALITY######################################

router <- make_router(
  route("/", landingPage),
  route("t1", uploadPage),
  route("t2", analyticsPage),
  route("t3", publishPage),
  route("t4", briefsPage),
  route("1", firstPage),
  route("2", secondPage),
  route("3", thirdPage),
  route("4", fourthPage),
  route("5", fifthPage),
  route("6", sixthPage),
  route("7", seventhPage),
  route("8", eighthPage),
  route("9", ninthPage),
  route("10", tenthPage),
  route("11", eleventhPage),
  route("12", twelfthPage)
)

# Add shiny.router dependencies manually: they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

#SETUP LAYOUT#######################################################

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", sidenav),
      div(class = "main", mainUI),
      div(class = "sidedisplay", sidedisplay),
      div(class = "footer", footer)
  )
}

#UI##############################################################################w###############################################

ui <- fluentPage(
  useShinyjs(),
  # theme = "style.css",
  shiny.i18n::usei18n(i18n),
  layout(router$ui),
  tags$head(
    includeCSS("./www/style.css"),
    shiny_router_script_tag
    ##Eventually replace the includeCSS() statement with the below format. Need to diagnose why R is unable to find the CSS file without an absolute pathway
    # tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
  ),
  tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lato');
                    "))
)

#SERVER#############w############################################################w###############################################

server <- function(input, output, session) {
  
  observeEvent(input$selected_language, { # translation, server side
    update_lang(session, input$selected_language)
  })
  
  router$server(input, output, session)
  
  output$current_page <- renderText({
    page <- get_page(session)
  })
  
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)
  
  r <- reactiveValues(
    thirdPage_focused_dataset = imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Walking" & Level == "ADM1")
  )  
  
  
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
  
  firstPage_stat3 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$SexRatio
  })
  
  firstPage_stat4 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$Perc0to14
  })
  
  firstPage_stat5 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$Perc15to64
  })
  
  firstPage_stat6 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$Perc65Plus
  })
  
  firstPage_stat7 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$TotDepRat
  })
  
  firstPage_stat8 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$CDepRate
  })
  
  firstPage_stat9 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$ADepRate
  })
  
  firstPage_stat10 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$MedianAge
  })
  
  firstPage_stat11 <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$LifeExpect
  })
  
  output$firstPage_statistics_text1 <- renderText({
    paste(
      firstPage_stat1(),
      "people")
  })
  
  output$firstPage_statistics_text2 <- renderText({
    paste(
      firstPage_stat2(),
      "people per square kilometer")
  })
  
  output$firstPage_statistics_text3 <- renderText({
    paste(
      firstPage_stat3(),
      "men per woman")
  })
  
  output$firstPage_statistics_text4 <- renderText({
    firstPage_stat4()
  })
  
  output$firstPage_statistics_text5 <- renderText({
    firstPage_stat5()
  })
  
  output$firstPage_statistics_text6 <- renderText({
    firstPage_stat6()
  })
  
  output$firstPage_statistics_text7 <- renderText({
    firstPage_stat7()
  })
  
  output$firstPage_statistics_text8 <- renderText({
    firstPage_stat8()
  })
  
  output$firstPage_statistics_text9 <- renderText({
    firstPage_stat9()
  })
  
  output$firstPage_statistics_text10 <- renderText({
    paste(
      firstPage_stat10(),
      "years")
  })
  
  output$firstPage_statistics_text11 <- renderText({
    paste(
      firstPage_stat11(),
      "years")
  })
  
  output$thirdPage_statistics_text0 <- renderText({
    name_indicator()
  })
  
  disability_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- internal$Dis
  })
  
  output$thirdPage_statistics_text1 <- renderText({
    paste(
      disability_indicator(),
      "people")
  })
  
  walking_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$Walk)
  })
  
  walkingrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$WalkRate)
  })
  
  output$thirdPage_statistics_text2 <- renderText({
    paste(
      walking_dis_indicator(),
      "people (",
      walkingrate_dis_indicator(),
      "%)")
  })
  
  seeing_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$See)
  })
  
  seeingrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$SeeRate)
  })
  
  output$thirdPage_statistics_text3 <- renderText({
    paste(
      seeing_dis_indicator(),
      "people (",
      seeingrate_dis_indicator(),
      "%)")
  })
  
  hearing_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$Hear)
  })
  
  hearingrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$HearRate)
  })
  
  output$thirdPage_statistics_text4 <- renderText({
    paste(
      hearing_dis_indicator(),
      "people (",
      hearingrate_dis_indicator(),
      "%)")
  })
  
  cognition_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$Cog)
  })
  
  cognitionrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$CogRate)
  })
  
  output$thirdPage_statistics_text5 <- renderText({
    paste(
      cognition_dis_indicator(),
      "people (",
      cognitionrate_dis_indicator(),
      "%)")
  })
  
  selfcare_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$Selfca)
  })
  
  selfcarerate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$SelfcaRate)
  })
  
  output$thirdPage_statistics_text6 <- renderText({
    paste(
      selfcare_dis_indicator(),
      "people (",
      selfcarerate_dis_indicator(),
      "%)")
  })
  
  communication_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$Comm)
  })
  
  commrate_dis_indicator <- reactive({
    internal <- filtered_data_from_dropdown_ind_widgets()
    internal <- mean(internal$CommRate)
  })
  
  output$thirdPage_statistics_text7 <- renderText({
    paste(
      communication_dis_indicator(),
      "people (",
      commrate_dis_indicator(),
      "%)")
  }) 
  
  #FOCUS BUTTONS######################################################
  
  #Third page
  observeEvent(input$walking_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Walking" & Level == "ADM1")
  })
  
  observeEvent(input$seeing_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Seeing" & Level == "ADM1")
  })
  
  observeEvent(input$hearing_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Hearing" & Level == "ADM1")
  })
  
  observeEvent(input$cognition_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Cognition" & Level == "ADM1")
  })
  
  observeEvent(input$selfcare_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Selfcare" & Level == "ADM1")
  })
  
  observeEvent(input$communication_button, {
    r$thirdPage_focused_dataset <- imported_data %>%
      filter(Type == "DisabilityRate" & Metric == "Communication" & Level == "ADM1")
  })
  
  #DATA TABLE VIEW FOR THE UPLOADED SHAPEFILES########################
  #PRE-JOIN
  output$printmaptable_0 <- DT::renderDataTable({
    req(sf_data_0)
    
    datatable(
      sf_data_0@data,
      options = list(scrollX = TRUE))
    
  })
  
  #LEAFLET############################################################  
  
  output$first_page_map <- renderLeaflet({
    
    first_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addCircles(data = sf_dots,
                 lng = ~lon,
                 lat = ~lat,
                 weight = 1,
                 radius = 80,
                 fillColor = "white",
                 stroke = FALSE,
                 fillOpacity = 1) %>%
      addLegend(
        colors = "#FFFFFF",
        labels = "one dot = 1,000 people",
        opacity = 1, 
        position = "bottomleft") %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  # observeEvent(input$dropdown_dots, {
  # 
  #   leafletProxy("first_page_map", data = sf1) %>%
  #     setView(lng = centroids[[input$dropdown_dots]]$x,
  #             lat = centroids[[input$dropdown_dots]]$y,
  #             zoom = 8) %>%
  #   addFullscreenControl()
  #   
  # })
  
  observeEvent(input$firstPage_choropleth_map_button, {
    bins <- c(0, 20000, 40000, 80000, 100000, Inf)
    pal <- colorBin("YlGn", domain = sf2@data$Pop, bins = bins)
    
    leafletProxy("first_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      # setView(lng = centroids[[input$dropdown_dots]]$x,
      #         lat = centroids[[input$dropdown_dots]]$y,
      #         zoom = 8) %>%
      addPolygons(fillColor = ~pal(Pop),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      addLegend(pal=pal, values = ~Pop, opacity = 0.7, title = NULL, position = "bottomright")
    
  })
  
  observeEvent(input$firstPage_dot_map_button, {
    
    leafletProxy("first_page_map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(data = sf_dots,
                 lng = ~lon,
                 lat = ~lat,
                 weight = 1,
                 radius = 80,
                 fillColor = "white",
                 stroke = FALSE,
                 fillOpacity = 1) %>%
      addLegend(
        colors = "#FFFFFF",
        labels = "one dot = 1,000 people",
        opacity = 1, 
        position = "bottomleft"
      )
    # setView(lng = centroids[[input$dropdown_dots]]$x,
    #         lat = centroids[[input$dropdown_dots]]$y,
    #         zoom = 8)
    
    
  })
  
  # #Third page maps
  
  third_page_map_bins <- c(6, 8, 10, 12, 14, Inf)
  
  output$third_page_map <- renderLeaflet({
    
    third_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g percent prevalence, people with at least some difficulty walking",
      sf2@data$NAME_1, sf2@data$ThirdShow
    ) %>% lapply(htmltools::HTML)
    
    third_page_map_internal <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", layerId = "basetile",options = providerTileOptions(minZoom = 6)) %>%
      addPolygons(data = sf2,
                  fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      setView(lng = -90,
              lat = 16,
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  
  observeEvent(input$walking_button, {
    
    sf2@data$ThirdShow <- sf2@data$WalkRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g percent prevalence, people with at least some difficulty walking",
      sf2@data$NAME_1, sf2@data$ThirdShow
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      # setView(lng = centroids[[input$dropdown_dots]]$x,
      #         lat = centroids[[input$dropdown_dots]]$y,
      #         zoom = 8) %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addLegend(pal=third_page_map_pal, values = ~sf2@data$ThirdShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$seeing_button, {
    
    sf2@data$ThirdShow <- sf2@data$SeeRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g percent prevalence, people with at least some difficulty seeing",
      sf2@data$NAME_1, sf2@data$ThirdShow
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      # setView(lng = centroids[[input$dropdown_dots]]$x,
      #         lat = centroids[[input$dropdown_dots]]$y,
      #         zoom = 8) %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addLegend(pal=third_page_map_pal, values = ~sf2@data$ThirdShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$hearing_button, {
    
    sf2@data$ThirdShow <- sf2@data$HearRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g percent prevalence, people with at least some difficulty hearing",
      sf2@data$NAME_1, sf2@data$ThirdShow
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      # setView(lng = centroids[[input$dropdown_dots]]$x,
      #         lat = centroids[[input$dropdown_dots]]$y,
      #         zoom = 8) %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addLegend(pal=third_page_map_pal, values = ~sf2@data$ThirdShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$cognition_button, {
    
    sf2@data$ThirdShow <- sf2@data$CogRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g percent prevalence, people with at least some cognition difficulty",
      sf2@data$NAME_1, sf2@data$ThirdShow
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      # setView(lng = centroids[[input$dropdown_dots]]$x,
      #         lat = centroids[[input$dropdown_dots]]$y,
      #         zoom = 8) %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addLegend(pal=third_page_map_pal, values = ~sf2@data$ThirdShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$selfcare_button, {
    
    sf2@data$ThirdShow <- sf2@data$SelfcaRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g percent prevalence, people with at least some difficulty with selfcare",
      sf2@data$NAME_1, sf2@data$ThirdShow
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      # setView(lng = centroids[[input$dropdown_dots]]$x,
      #         lat = centroids[[input$dropdown_dots]]$y,
      #         zoom = 8) %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addLegend(pal=third_page_map_pal, values = ~sf2@data$ThirdShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  observeEvent(input$communication_button, {
    
    sf2@data$ThirdShow <- sf2@data$CommRate
    
    third_page_map_pal <- colorBin("YlOrRd", domain = sf2@data$ThirdShow, bins = third_page_map_bins)
    
    third_page_map_labels <- sprintf(
      "<strong>%s</strong><br/>%g percent prevalence, people with at least some difficulty with communication",
      sf2@data$NAME_1, sf2@data$ThirdShow
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("third_page_map", data = sf2) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      # setView(lng = centroids[[input$dropdown_dots]]$x,
      #         lat = centroids[[input$dropdown_dots]]$y,
      #         zoom = 8) %>%
      addPolygons(fillColor = ~third_page_map_pal(ThirdShow),
                  weight = 1,
                  fillOpacity = 0.8,
                  color = "white",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE),
                  label = third_page_map_labels) %>%
      addLegend(pal=third_page_map_pal, values = ~sf2@data$ThirdShow, opacity = 0.7, title = NULL, position = "bottomright") %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
    
  })
  
  #PLOTS##############################################################
  
  output$firstPage_Pop_ADM1Plot <- renderPlot({
    
    req(input$dropdown)
    
    internal_df <- imported_data %>%
      filter(Metric == "Population" & Level == "ADM1")
    
    a <- internal_df %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#41AB5D", stat = "identity") +
      # xlab(i18n$t("Department")) +
      theme(legend.position = "none") +
      ylab(i18n$t("People")) +
      theme_plot() +
      ggtitle("Population by Department") +
      coord_flip()
    
    if(input$dropdown != "All") a <- a  + gghighlight(ADM1 == input$dropdown)
    
    a
  })
  
  output$firstPage_Sex <- renderPlot({
    sexPlot <- dropdownFilteredData() %>%
      filter(Metric == "Population" & Type %in% c("Male", "Female")) %>%
      mutate(Sex = fct_relevel(Type, "Female", "Male")) %>%
      ggplot(aes(x = Type, y = Value)) +
      geom_bar(stat = "identity") +
      ggtitle("Population by Sex") +
      ylab(i18n$t("People")) +
      xlab("Sex") +
      theme_plot() +
      coord_flip()
    
    sexPlot
  })
  
  output$firstPage_Age <- renderPlot({
    agePlot <- dropdownFilteredData() %>%
      filter(Metric == "Population" & Type %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+")) %>%
      mutate(Type = fct_relevel(Type, "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+")) %>%
      ggplot(aes(x = Type, y = Value)) +
      geom_bar(stat = "identity") +
      ggtitle("Population by Age") +
      ylab(i18n$t("People")) +
      xlab("Age") +
      theme_plot() +
      coord_flip()
    
    agePlot
  })
  
  output$firstPage_UrbanRural <- renderPlot({
    urbanRuralPlot <- dropdownFilteredData() %>%
      filter(Metric == "Population" & Type %in% c("Urban", "Rural")) %>%
      mutate(UrbanRural = fct_relevel(UrbanRural, "Urban", "Rural")) %>%
      ggplot(aes(x = Type, y = Value)) +
      geom_bar(stat = "identity") +
      ggtitle("Population by Urban / Rural Residence") +
      ylab("People") +
      xlab("Urban / Rural Status") +
      theme_plot() +
      coord_flip()
    
    urbanRuralPlot
  })
  
  output$thirdPage_Dis_ADM1Plot <- renderPlot({
    
    internal <- r$thirdPage_focused_dataset['Metric'] %>%
      slice(2) %>%
      unlist(., use.names = FALSE)
    
    if (internal == "Walking") {
      plotTitle <- "Prevalence Rate of Population with at Least Some \nDifficulty in Walking by Department"
    } else if (internal == "Seeing") {
      plotTitle <- "Prevalence Rate of Population with at Least Some \nDifficulty in Seeing by Department"
    } else if (internal == "Hearing") {
      plotTitle <- "Prevalence Rate of Population with at Least Some \nDifficulty in Hearing by Department"
    } else if (internal == "Cognition") {
      plotTitle <- "Prevalence Rate of Population with at Least Some \nCognition Difficulty by Department"
    } else if (internal == "Selfcare") {
      plotTitle <- "Prevalence Rate of Population with at Least Some \nDifficulty With Selfcare by Department"
    } else if (internal == "Communication") {
      plotTitle <- "Prevalence Rate of Population with at Least Some \nDifficulty With Communication by Department"
    } else {
      plotTitle <- internal
    }
    
    a <- r$thirdPage_focused_dataset %>%
      filter(Level == "ADM1") %>%
      filter(Type == "DisabilityRate") %>%
      mutate(ADM1 = fct_reorder(ADM1, Value)) %>%
      ggplot(aes(x = reorder(ADM1, Value), y = Value)) +
      geom_bar(fill = "#F05131", stat = "identity") +
      # xlab(i18n$t("Department")) +
      theme(legend.position = "none") +
      ylab("Prevalence Rate (Percent)") +
      theme_plot() +
      ggtitle(plotTitle) +
      coord_flip()
    
    if(input$dropdown != "All") a <- a  + gghighlight(ADM1 == input$dropdown)
    
    a
    
  })
  
  
  
  #TRANSLATION########################################################
  
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  

}



shinyApp(ui, server)
