library(shiny)
library(tidyverse)
library(NHANES)

str(NHANES)

ui <- fluidPage(
  titlePanel("Clinical Biomarkers"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "yvar",
        "Y Variable",
        choices = c(
          "Systolic Blood Pressure" = "BPSysAve",
          "Diastolic Blood Pressure" = "BPDiaAve",
          "Total Cholesterol" = "TotChol",
          "Testosterone" = "Testosterone"
        ),
        selected = "BPSysAve"
      ),
      sliderInput("ageRange", "Age Range", min = 20, max = 80, value = c(20, 80)),
      checkboxInput("addSmooth", "Show Trend Line", value = TRUE)
    ),
    mainPanel(
      plotOutput("bpPlot")
    )
  )
)

server <- function(input, output, session) {
  
  biomarker_cutoffs <- list(
    BPSysAve = tibble(
      cutoff = c(120, 130, 140,Inf),
      labels = c("Normal", "Elevated", "Stage 1 Hypertension", "Stage 2 Hypertension")
    ),
    BPDiaAve = tibble(
      cutoff = c(80, 90, Inf),
      labels = c("Normal", "Stage 1 Hypertension", "Stage 2 Hypertension")
    ),
    TotChol = tibble(
      cutoff = c(5.2, 6.2, Inf),
      labels = c("Normal", "Borderline High", "High")
    ),
    Testosterone = tibble(
      cutoff = c(270, 1070, Inf),
      labels = c("Low", "Normal", "High")
    )
  )
 #, 6, 86, Inf 
  output$bpPlot <- renderPlot({
    
    # Filter data by selected biomarker and age range
    df <- NHANES %>%
      drop_na(.data[[input$yvar]]) %>%
      mutate(Gender = stringr::str_to_title(Gender)) %>%
      filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
    
    # Get cutoffs for selected biomarker
    co <- biomarker_cutoffs[[input$yvar]]
    
    # Base plot
    p <- df %>%
      ggplot(aes(x = Age, y = .data[[input$yvar]], color = Gender)) +
      geom_point(alpha = 0.5) +
      theme_classic() +
      
      labs(y = input$yvar) +
      theme(legend.position="none",
            axis.title.x = element_text(size = 15, face = "bold"),
            axis.title.y = element_text(size = 15, face = "bold"),
            strip.text = element_text(size = 18, face="bold"),
            axis.text=element_text(size = 15))+
      facet_wrap(~ Gender, scales = "free")
    
    # Add trend line if selected
    if (input$addSmooth) {
      p <- p + geom_smooth(se = FALSE, color = "blue")
    }
    
    # Add horizontal cutoff lines and labels
    if (!is.null(co)) {
      # Cutoff lines
      p <- p + geom_hline(aes(yintercept = cutoff), data = co, linetype = 2, color = "black")
      
      # Labels
      y_ranges <- c(-Inf, co$cutoff, Inf)
      co_labs <- tibble(
        y = sapply(seq_along(co$labels), function(i) mean(c(y_ranges[i], y_ranges[i+1]), na.rm = TRUE)),
        label = co$labels
      )
      
      p <- p + geom_label(
        data = co_labs,
        aes(x = min(df$Age, na.rm = TRUE), y = y, label = label),
        hjust = 0,
        vjust = 0,
        inherit.aes = FALSE,
        size = 4,
        fill = "grey90", 
        fontface="bold",
        color = "black"
      )
    }
    
    p
  })
  
}

shinyApp(ui, server)





