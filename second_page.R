#SETUP FUNCTIONS TO GENERATE PAGE SECTIONS, PAGE DISABILITY STATUS#

nav_picker_secondPage <- div(
  class = "nav_picker",
  DefaultButton.shinyInput(
    "walking_button",
    text = i18n$t("Walking"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "seeing_button",
    text = i18n$t("Seeing"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "hearing_button",
    text = i18n$t("Hearing"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "cognition_button",
    text = i18n$t("Cognition"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "selfcare_button",
    text = i18n$t("Self-Care"),
    className = "nav_item"
  ),
  DefaultButton.shinyInput(
    "communication_button",
    text = i18n$t("Communication"),
    className = "nav_item"
  )
)

second_page_first_stack <- Stack(
  tokens = list(childrenGap = 10),
  div(
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(" ",
               div(
                 leafletOutput("second_page_map")
               ),
               size = 6
      ),
      makeCard(" ",
               div(
                 plotOutput("secondPage_Dis_ADM1Plot")
               ),
               size = 3
      ),
      makeCard(" ",
               div(
                 strong(i18n$t("Viewing:")),
                 textOutput("secondPage_statistics_text0"),
                 br(),
                 strong(i18n$t("At least some difficulty in any domain*:")),
                 textOutput("secondPage_statistics_text1"),
                 br(),
                 p(i18n$t("*included are walking, seeing, hearing, cognition, self-care, and/or communication"))
               ),
               size = 3
      )
    )
  )
)

secondPage_first_stack_content <- function() {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(" ",
             second_page_first_stack,
             size = 11)
  )}



second_page_second_stack <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    tokens = list(childrenGap = 10), horizontal = FALSE,
    makeCard(" ",
             div(
               p(i18n$t("These figures and calculations include only individuals aged 5 years and older"))
             ),
             size = 11
    )
  )
)

secondPage_second_stack_content <- function() {
  Stack(
    tokens = list(childrenGap = 10),
    makeCard(i18n$t("Notes"),
             second_page_second_stack,
             size = 11)
  )}

secondPage <- function(i18n) {
  tagList(
    shiny.i18n::usei18n(i18n),
    makePage(
      i18n$t('Disability Status'),
      i18n$t("Explore each subtopic"),
      div(
        secondPage_first_stack_content(),
        secondPage_second_stack_content()
      ))
  )}

