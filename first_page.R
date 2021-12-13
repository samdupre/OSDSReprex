#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE DEMOGRAPHIC AND SOCIAL#

first_page_overview_stack <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = TRUE,
    makeCard(" ",
             div(
               leafletOutput("first_page_map")
             ),
             size = 6
    ),
    makeCard(" ",
             div(
               plotOutput("firstPage_Pop_ADM1Plot")),
             size = 3
    ),
    makeCard(" ",
             div(
               strong(i18n$t("Viewing:")),
               textOutput("firstPage_statistics_text0"),
               br(),
               strong(i18n$t("Total Population")),
               textOutput("firstPage_statistics_text1"),
               strong(i18n$t("Population Density")),
               textOutput("firstPage_statistics_text2")
             ),
             size = 3
    )
  )
)

firstPage_first_stack_content <- function() {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             first_page_overview_stack, size = 11)
  )}

first_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("firstPage_Sex")),
               size = 6
      ),
      
      makeCard(" ",
               div(
                 plotOutput("firstPage_Age")),
               size = 5
      )
    ),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 plotOutput("firstPage_UrbanRural")),
               size = 6
      ),
      makeCard(" ",
               Stack(
                 p(i18n$t("NOTE [Definition of dependency ratio, total]"))),
               size = 5
      )
    )
  )
)

firstPage_second_stack_content <- function() {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             first_page_second_stack, size = 11)
  )}


firstPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage(
      i18n$t('Demography and Social Characteristics'),
      i18n$t("Explore each subtopic"),
      div(
        firstPage_first_stack_content(),
        firstPage_second_stack_content()
      )
    ))
}
