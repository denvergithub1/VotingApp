#install.packages("shiny")
#install.packages("googlesheets4")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("digest")

library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(digest)

# Authenticating
gs4_auth(path = "...")

# UI
ui <- fluidPage(
  titlePanel("Create Your Survey"),
  
  sidebarLayout(
    sidebarPanel(
      tags$p(strong("Expected Sheet Columns:"), "Door, Question, Option, Votes"),
      textInput("sheetURL", "Enter Google Sheet URL:"),
      actionButton("connectSheet", "Connect Sheet"),
      hr(),
      
      h4("Create New Questions"),
      numericInput("numQuestions", "How many questions to add?", value = 1, min = 1, max = 5),
      uiOutput("multiQuestionInputs"),
      actionButton("createMultiple", "Add All Questions"),
      
      hr(),
      h4("Vote on Questions"),
      textInput("doorCode", "Door Code:"),
      actionButton("loadQuestions", "Load Questions"),
      actionButton("refreshQuestions", "Refresh Questions"),   
      uiOutput("votingUI"),
      actionButton("refreshResults", "Refresh Results")
    ),
    
    mainPanel(
      uiOutput("allPlots"),
      tags$div(
        tags$style(HTML(".green-label { color: green; }")),
        HTML("<p class='green-label'>JSur Sponsors</p>")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  sheet_id <- reactiveVal(NULL)
  question_data <- reactiveVal(NULL)
  
  observeEvent(input$connectSheet, {
    req(input$sheetURL)
    id <- sub(".*?/d/([a-zA-Z0-9_-]+).*", "\\1", input$sheetURL)
    sheet_id(id)
    showNotification("Connected to Google Sheet", type = "message")
  })
  
  output$multiQuestionInputs <- renderUI({
    req(input$numQuestions)
    lapply(1:input$numQuestions, function(i) {
      tagList(
        h4(paste("Question", i)),
        textInput(paste0("door", i), label = paste("Door Code to Q", i)),
        textInput(paste0("question", i), label = paste("Question", i)),
        textAreaInput(paste0("options", i), label = "Answer options (one per line):")
      )
    })
  })
  
  observeEvent(input$createMultiple, {
    req(sheet_id())
    
    df_test <- tryCatch(read_sheet(sheet_id()), error = function(e) NULL)
    if (!is.null(df_test) && !all(c("Door", "Question", "Option", "Votes") %in% names(df_test))) {
      showNotification("Sheet is missing required columns.", type = "error")
      return()
    }
    
    entries <- list()
    for (i in 1:input$numQuestions) {
      door <- input[[paste0("door", i)]]
      question <- input[[paste0("question", i)]]
      options_raw <- input[[paste0("options", i)]]
      
      if (!is.null(door) && !is.null(question) && !is.null(options_raw)) {
        opts <- strsplit(options_raw, "\n")[[1]]
        opts <- trimws(opts)
        opts <- opts[opts != ""]
        
        if (length(opts) >= 2) {
          entries[[length(entries) + 1]] <- data.frame(
            Door = door,
            Question = question,
            Option = opts,
            Votes = 0,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(entries) > 0) {
      all_df <- do.call(rbind, entries)
      tryCatch({
        sheet_append(ss = sheet_id(), data = all_df)
        showNotification("Questions added successfully!", type = "message")
      }, error = function(e) {
        showNotification("Failed to add questions.", type = "error")
      })
    }
  })
  
  observeEvent(input$loadQuestions, {
    req(sheet_id(), input$doorCode)
    tryCatch({
      df <- read_sheet(sheet_id())
      if (!all(c("Door", "Question", "Option", "Votes") %in% names(df))) {
        showNotification("Sheet is missing required columns.", type = "error")
        return()
      }
      input_door <- trimws(input$doorCode)
      df_door <- df %>% filter(Door == input_door)
      if (nrow(df_door) == 0) {
        showNotification("No questions found for this Door Code.", type = "warning")
        question_data(NULL)
      } else {
        question_data(df_door)
        showNotification("Questions loaded!", type = "message")
      }
    }, error = function(e) {
      showNotification("Failed to read sheet.", type = "error")
    })
  })
  
  observeEvent(input$refreshQuestions, {
    req(input$doorCode)
    # To manually trigger reload when refreshed
    updateTextInput(session, "doorCode", value = input$doorCode) 
    input$loadQuestions  # Triggers loadQuestions logic again
  })
  
  output$votingUI <- renderUI({
    req(question_data())
    df <- question_data()
    questions <- unique(df$Question)
    
    lapply(questions, function(question) {
      options <- df %>% filter(Question == question)
      tagList(
        h4(question),
        lapply(1:nrow(options), function(i) {
          opt <- options$Option[i]
          door <- options$Door[i]
          btn_id <- paste0("vote_", digest(paste(door, question, opt)))
          
          observeEvent(input[[btn_id]], {
            full_df <- read_sheet(sheet_id())
            full_df <- full_df %>%
              mutate(Votes = ifelse(Door == door & Question == question & Option == opt,
                                    Votes + 1, Votes))
            tryCatch({
              range_write(ss = sheet_id(), data = full_df, sheet = 1, col_names = TRUE)
              showNotification(paste("Voted for:", opt), type = "message")
              question_data(full_df %>% filter(Door == door))
            }, error = function(e) {
              showNotification("Vote failed.", type = "error")
            })
          }, ignoreInit = TRUE)
          
          actionButton(btn_id, label = paste("Vote:", opt))
        }),
        tags$hr()
      )
    })
  })
  
  output$allPlots <- renderUI({
    req(question_data())
    df <- question_data()
    questions <- unique(df$Question)
    
    plot_outputs <- lapply(seq_along(questions), function(i) {
      question <- questions[i]
      plotname <- paste0("plot_", i)
      
      output[[plotname]] <- renderPlot({
        plot_df <- df %>% filter(Question == question)
        ggplot(plot_df, aes(x = Option, y = Votes, fill = Option)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = question, x = "Option", y = "Votes") +
          theme(legend.position = "none")
      })
      
      plotOutput(plotname)
    })
    
    avg_text <- tagList(
      h4("Average Votes per Question:"),
      lapply(questions, function(q) {
        avg <- round(mean(df %>% filter(Question == q) %>% pull(Votes)), 2)
        p(paste(q, ":", avg))
      })
    )
    
    tagList(do.call(tagList, plot_outputs), avg_text)
  })
}

shinyApp(ui, server)






