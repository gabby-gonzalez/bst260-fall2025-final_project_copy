# app.R
library(shiny)
library(NHANES)
library(dplyr)
library(ggplot2)

data("NHANES")

# Choices for PhysActiveDays (0–7)
pa_days_choices <- sort(unique(NHANES$PhysActiveDays[!is.na(NHANES$PhysActiveDays)]))

ui <- fluidPage(
  titlePanel("NHANES: Physical Activity and BMI"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "xvar",
        label   = "X-axis (Physical Activity):",
        choices = c(
          "Physical Activity (Yes/No)"        = "PhysActive",
          "Physical Activity Days per Week"   = "PhysActiveDays"
        ),
        selected = "PhysActive"
      ),
      selectInput(
        inputId = "yvar",
        label   = "Y-axis (BMI):",
        choices = c(
          "BMI (continuous)"   = "BMI",
          "BMI Category (WHO)" = "BMI_WHO"
        ),
        selected = "BMI"
      ),
      selectInput(
        inputId = "facet",
        label   = "Facet by:",
        choices = c(
          "None"   = "none",
          "Gender" = "Gender"
        ),
        selected = "Gender"
      ),
      
      # Toggle between boxplot and jitter when BMI is continuous
      conditionalPanel(
        condition = "input.yvar == 'BMI'",
        radioButtons(
          inputId = "geom_type",
          label   = "Plot type for BMI:",
          choices = c(
            "Boxplot"    = "box",
            "Jitter Plot" = "jitter"
          ),
          selected = "box"
        )
      ),
      
      # Let user pick which physical activity days to include
      conditionalPanel(
        condition = "input.xvar == 'PhysActiveDays'",
        checkboxGroupInput(
          inputId  = "padays_filter",
          label    = "Number of physical activity days per week to include in the graph:",
          choices  = pa_days_choices,
          selected = pa_days_choices
        )
      )
    ),
    
    mainPanel(
      plotOutput("pa_bmi_plot")
    )
  )
)

server <- function(input, output, session) {
  
  plot_data <- reactive({
    df <- NHANES
    
    # X-variable filters
    if (input$xvar == "PhysActive") {
      df <- df |> filter(!is.na(PhysActive))
      
    } else if (input$xvar == "PhysActiveDays") {
      df <- df |> filter(!is.na(PhysActiveDays))
      
      # Apply selected PA days
      if (!is.null(input$padays_filter)) {
        df <- df |> 
          filter(PhysActiveDays %in% as.numeric(input$padays_filter))
      }
    }
    
    # Y-variable filters
    if (input$yvar == "BMI") {
      df <- df |> filter(!is.na(BMI))
      
    } else if (input$yvar == "BMI_WHO") {
      df <- df |> filter(!is.na(BMI_WHO))
    }
    
    # Capitalize gender for facet labels
    df$Gender <- tools::toTitleCase(as.character(df$Gender))
    
    df
  })
  
  output$pa_bmi_plot <- renderPlot({
    df <- plot_data()
    
    # ----- Continuous BMI -----
    if (input$yvar == "BMI") {
      
      if (input$xvar == "PhysActive") {
        # Base plot for PhysActive
        p <- ggplot(df, aes(x = PhysActive, y = BMI, fill = PhysActive))
        
        if (input$geom_type == "box") {
          p <- p + geom_boxplot()
        } else {
          p <- p +
            geom_jitter(
              aes(color = PhysActive),
              width = 0.2,
              alpha = 0.35,
              size = 1.8
            ) +
            guides(color = "none")
        }
        
        p <- p +
          labs(
            title = "BMI by Physical Activity Status",
            x = "Physical Activity (Yes/No)",
            y = "BMI"
          ) +
          guides(fill = "none")
        
      } else {
        # Base plot for PhysActiveDays
        p <- ggplot(df, aes(
          x    = factor(PhysActiveDays),
          y    = BMI,
          fill = factor(PhysActiveDays)
        ))
        
        if (input$geom_type == "box") {
          p <- p + geom_boxplot()
        } else {
          p <- p +
            geom_jitter(
              aes(color = factor(PhysActiveDays)),
              width = 0.2,
              alpha = 0.35,
              size = 1.8
            ) +
            guides(color = "none")
        }
        
        p <- p +
          labs(
            title = "BMI by Physical Activity Days per Week",
            x = "Physical Activity Days (0–7)",
            y = "BMI",
            fill = "PA Days"
          ) +
          guides(fill = "none")
      }
      
    } else {
      # ----- BMI Category (stacked bars) -----
      if (input$xvar == "PhysActive") {
        p <- ggplot(df, aes(x = PhysActive, fill = BMI_WHO)) +
          geom_bar(position = "fill") +
          labs(
            title = "BMI Categories by Physical Activity Status",
            x = "Physical Activity (Yes/No)",
            y = "Proportion",
            fill = "BMI Category"
          )
      } else {
        p <- ggplot(df, aes(x = factor(PhysActiveDays), fill = BMI_WHO)) +
          geom_bar(position = "fill") +
          labs(
            title = "BMI Categories by Physical Activity Days per Week",
            x = "Physical Activity Days (0–7)",
            y = "Proportion",
            fill = "BMI Category"
          )
      }
    }
    
    # Facet if selected
    if (input$facet == "Gender") {
      p <- p + facet_wrap(~ Gender)
    }
    
    # Styling theme
    p +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 14),
        strip.text = element_text(face = "bold", size = 13)
      )
  })
}

shinyApp(ui = ui, server = server)
