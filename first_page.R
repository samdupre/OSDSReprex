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
               strong("Viewing: "),
               textOutput("firstPage_statistics_text0"),
               br(),
               strong("Total Population"),
               textOutput("firstPage_statistics_text1"),
               strong("Population Density"),
               textOutput("firstPage_statistics_text2"),
               strong("Sex Ratio"),
               textOutput("firstPage_statistics_text3"),
               strong("Percent Aged 0 to 14 Years"),
               textOutput("firstPage_statistics_text4"),
               strong("Percent Aged 15 to 64 Years"),
               textOutput("firstPage_statistics_text5"),
               strong("Percent Aged 65+ Years"),
               textOutput("firstPage_statistics_text6"),
               strong("Dependency Ratio, Total"),
               textOutput("firstPage_statistics_text7"),
               strong("Dependency Ratio, Child"),
               textOutput("firstPage_statistics_text8"),
               strong("Dependency Ratio, Aged"),
               textOutput("firstPage_statistics_text9"),
               strong("Median Age"),
               textOutput("firstPage_statistics_text10"),
               strong("Life Expectancy"),
               textOutput("firstPage_statistics_text11")
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
                 p("NOTE [Definition of dependency ratio, total]"),
                 p("NOTE [Definition of dependency ratio, youth]"),
                 p("NOTE [Definition of dependency ratio, child]")),
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

