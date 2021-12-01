#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE DISABILITY STATUS#

nav_picker_thirdPage <- div(
  class = "nav_picker",
  DefaultButton.shinyInput(
    "walking_button",
    text = "Walking",
    # iconProps = list("iconName" = 'Globe'),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "seeing_button",
    text = "Seeing",
    # iconProps = list("iconName" = 'ShareiOS'),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "hearing_button",
    text = "Hearing",
    # iconProps = list("iconName" = 'ShareiOS'),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "cognition_button",
    text = "Cognition",
    # iconProps = list("iconName" = 'Globe'),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "selfcare_button",
    text = "Self-Care",
    # iconProps = list("iconName" = 'ShareiOS'),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "communication_button",
    text = "Communication",
    # iconProps = list("iconName" = 'ShareiOS'),
    className = "nav_item"
  )
)

third_page_first_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 leafletOutput("third_page_map")
               ),
               size = 6
      ),
      makeCard(" ",
               div(
                 plotOutput("thirdPage_Dis_ADM1Plot")
               ),
               size = 3
      ),
      makeCard(" ",
               div(
                 strong("Viewing: "),
                 textOutput("thirdPage_statistics_text0"),
                 br(),
                 strong("At least some difficulty in any domain*:"),
                 textOutput("thirdPage_statistics_text1"),
                 strong("Walking"),
                 textOutput("thirdPage_statistics_text2"),
                 strong("Seeing"),
                 textOutput("thirdPage_statistics_text3"),
                 strong("Hearing"),
                 textOutput("thirdPage_statistics_text4"),
                 strong("Cognition"),
                 textOutput("thirdPage_statistics_text5"),
                 strong("Self-Care"),
                 textOutput("thirdPage_statistics_text6"),
                 strong("Communication"),
                 textOutput("thirdPage_statistics_text7"),
                 br(),
                 p("*included are walking, seeing, hearing, cognition, self-care, and/or communication")
               ),
               size = 3
      )
    )
  )
)

thirdPage_first_stack_content <- function() {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             third_page_first_stack,
             size = 11)
  )}



third_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               p("These figures and calculations include only individuals aged 5 years and older")
             ),
             size = 11
    )
  )
)

thirdPage_second_stack_content <- function() {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard("Notes",
             third_page_second_stack,
             size = 11)
  )}
