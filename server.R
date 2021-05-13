library(shiny)
library(shinyWidgets)
library(bslib)
library(data.table)
library(ggplot2)
source("fcts_NMTcalc.R")

function(input, output){
  dt <- read.csv2("villagersList.csv")  
  setDT(dt)
  
  dts <- reactiveValues(dt_dreamies = data.table(), dt_islanders = data.table())
  
  output$ui_dreamies <- renderUI({
    dts$dt_islanders <- dt[!name %in% input$vill_island]

    multiInput(
      inputId = "vill_dreamies",
      label = "And what villagers are you hunting for?",
      choices = NULL,
      choiceNames = dts$dt_islanders$name,
      choiceValues = dts$dt_islanders$name
    )
  })
  
  observe(dts$dt_dreamies <- dt[name %in% input$vill_dreamies])
  
  out <- eventReactive(input$startCalc, {
    
    # validate(length(input$vill_dreamies) > 0)
    
    if(input$NMTorProba == 'Proba'){
      result <- calculateProba_NMTs(dts$dt_dreamies, dts$dt_islanders, NMTs = input$NMTs)
      text <- HTML(paste0("The probability of one of your dreamies appearing on a mystery island is <b>",
             round(result[["proba_perVisit"]]*100, 2), "%</b>. <br><br> With your <b>",
             input$NMTs, "</b> Nook Mile Tickets, you have a <b>",
             round(result[["out"]]*100, 2), "%</b> chance of finding one of them before running out of tickets."))
      plot <- plotNbinomDist(result[["proba_perVisit"]], NMTs = input$NMTs)

    } else {
      result <- calculateProba_NMTs(dts$dt_dreamies, dts$dt_islanders, proba = input$proba)
      text <- HTML(paste0("The probability of one of your dreamies appearing on a mystery island is <b>",
             round(result[["proba_perVisit"]]*100, 2), "%</b>. <br><br>You will need <b>",
             result[["out"]], "</b> Nook Mile Tickets to find one of them with a probability of <b>",
             input$proba*100, "%</b> before running out of tickets."))
      plot <- plotNbinomDist(result[["proba_perVisit"]], proba = input$proba)
    }
    list(result = result, text = text, plot = plot)
  })
  
  output$result_text <- renderUI({
    out()$text
  })
  
  output$result_plot <- renderPlot({
    out()$plot
  })
}