# app.R
library(shiny)
library(NHANES)
library(dplyr)
library(ggplot2)

data("NHANES")

ui <- fluidPage(
  titlePanel("NHANES: Age, Education, and Household Income"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "xvar",
        label   = "X-axis (Age):",
        choices = c(
          "Age (Decade)"        = "AgeDecade",
          "Age (Continuous)"   = "Age"
        ),
        selected = "AgeDecade"
      ),
      selectInput(
        inputId = "yvar",
        label   = "Y-axis (Education):",
        choices = c(
          "Education Level"   = "Education"
        ),
        selected = "Education"
      ),
      selectInput(
        inputId = "facet",
        label   = "Facet by:",
        choices = c(
          "None"   = "none",
          "Gender" = "Gender",
          "Race" = "Race1",
          "Gender and Race" = "Gender + Race1"
        ),
        selected = "none"
      )
    ),
    
    mainPanel(
      plotOutput("agedec_edu_hhinc_heatmap", hover = "plot_hover"),
      verbatimTextOutput("hover_info")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data filtered based on selected X
  plot_data <- reactive({
    df <- NHANES
    
    if (input$xvar == "AgeDecade") {
      df <- df |> 
        filter(!is.na(AgeDecade),!is.na(HHIncomeMid)) |>
        select(AgeDecade, Education, HHIncomeMid, Gender, Race1)
    } else if (input$xvar == "Age") {
      df <- df |> 
        filter(!is.na(Age),!is.na(HHIncomeMid)) |>
        select(Age, Education, HHIncomeMid, Gender, Race1)
    }
    if (input$yvar == "Education") {
      df <- df |> filter(!is.na(Education))
    }
    df
  })
  
  output$agedec_edu_hhinc_heatmap <- renderPlot({
    df <- plot_data()
    
    if (input$xvar == "AgeDecade") {
        # AgeDecade and Education on Household Income → Heatmap
        p <- ggplot(df, aes(x = AgeDecade, y = Education, fill = HHIncomeMid)) +
          geom_tile() +
          labs(title = "Age Decade and Education Level on Household Income",
               x = "Age Decade", 
               y = "Education Level", 
               fill = "Household Income") +
          coord_fixed() +
          scale_fill_gradient(low = "lightblue", high = "darkblue") +
          theme_minimal()
        
      } else {
        # Age (Continuous) and Education on Household Income → Heatmap
        p <- ggplot(df, aes(x = Age, y = Education, fill = HHIncomeMid)) +
          geom_tile() +
          labs(title = "Age and Education Level on Household Income",
               x = "Age", 
               y = "Education Level", 
               fill = "Household Income") +
          scale_fill_gradient(low = "lightblue", high = "darkblue") +
          theme_minimal()
      }
  
    # Add faceting if selected
    if (input$facet == "Gender") {
      p <- p + facet_wrap(~ Gender)
    }
    else if (input$facet == "Race1") {
      p <- p + facet_wrap(~ Race1)
    }
    else if (input$facet == "Gender + Race1") {
      p <- p + facet_wrap(~ Gender + Race1, nrow = 2)
    }
 
    p
  })
  
  output$hover_info <- renderPrint({
    df <- plot_data()   # USE REACTIVE DATA
    
    if (is.null(input$plot_hover)) return(NULL)
    
    hover_data <- nearPoints(
      df,
      input$plot_hover,
      xvar = input$xvar,
      yvar = input$yvar,
      threshold = 10,
      maxpoints = 1,
      addDist = TRUE
    )
    
    if (nrow(hover_data) > 0) {
      cat("Hovered Data:\n")
      print(hover_data)
    } else {
      cat("No data near cursor.")
    }
  })

}

shinyApp(ui = ui, server = server)
