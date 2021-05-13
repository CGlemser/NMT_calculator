library(shiny)
library(shinyWidgets)
library(bslib)
library(data.table)
library(shinyFeedback)
library(ggplot2)

dt <- read.csv2("villagersList.csv")
setDT(dt)

thematic::thematic_shiny()

fluidPage(
  shinyFeedback::useShinyFeedback(),
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  titlePanel(h1("The Nook Mile Tickets Calculator of your dreams!",
                align = "center"),
             windowTitle = "NMT calculator"),
  br(),

  sidebarLayout(
    sidebarPanel(width = 4,
        multiInput(
          inputId = "vill_island",
          label = "What villagers are currently living on your island?",
          choices = NULL,
          choiceNames = dt$name,
          choiceValues = dt$name,
          options = list(
            limit = 10
          )
        ),

        uiOutput("ui_dreamies"),
        
        br(),

        radioButtons("NMTorProba", "What do you want to calculate?",
          choiceNames = c("the number of NMTs needed to achieve a given success probability",
                          "my success probability given the number of NMTs"),
          choiceValues = c("numNMTs", "Proba")),
        
        conditionalPanel(
          condition = "input.NMTorProba == 'numNMTs'",
          sliderInput("proba", "What probability do you want to ensure?",
                      min = 0.05, max = .95, value = .95, step = .05)
        ),
        
        conditionalPanel(
          condition = "input.NMTorProba == 'Proba'",
          numericInput("NMTs", "How many NMTs do you have?",
                       value = 100)
        ),
        
        conditionalPanel(
          condition = "input.vill_dreamies.length > 0",
          actionButton("startCalc", "Do the magic!")
        )
    ),

    mainPanel(width = 7,
              br(),
              uiOutput("result_text"),
              br(),
              plotOutput("result_plot")
    )
  )
)