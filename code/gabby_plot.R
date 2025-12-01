
library(tidyverse)
library(ggplot2)
library(shiny)


library(NHANES)

dat<-NHANES
dat$Gender


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
      )
    ),
    mainPanel(
      plotOutput("bpPlot")
    )
  )
)

server <- function(input, output, session) {
  
  biomarker_cutoffs <- list(
    BPSysAve = tibble(
      cutoff = c(120, 130, 140, 180),
      labels = c("Normal", "Elevated", "Stage 1 Hypertension", "Stage 2 Hypertension")
    ),
    BPDiaAve = tibble(
      cutoff = c(80, 90),
      labels = c("Normal", "Stage 1 HTN")
    ),
    TotChol = tibble(
      cutoff = c(200, 240),
      labels = c("Desirable", "Borderline High")
    ),
    Testosterone = tibble(
      cutoff = c(300, 1000),   # sample values—replace with your own
      labels = c("Low", "Normal")
    )
  )
  output$bpPlot <- renderPlot({
    
    df <- dat %>%
      drop_na(.data[[input$yvar]]) %>%
      mutate(Gender = stringr::str_to_title(Gender))
      
    
    # cutoffs 
    co <- biomarker_cutoffs[[input$yvar]]
    
    # Base plot
    p <- df %>%
      ggplot(aes(x = Age, y = .data[[input$yvar]], color = Gender)) +
      geom_point(alpha = 0.3) +
      theme_classic() +
      labs(y = input$yvar) +
      facet_wrap(~ Gender, scales = "free")
    
    # Add horizontal cutoff lines
    if (!is.null(co)) {
      p <- p + 
        geom_hline(aes(yintercept = cutoff), data = co, linetype = 2, color = "red")
      y_ranges <- c(-Inf, co$cutoff, Inf)
      
      co_labs <- tibble(
        y = sapply(seq_along(co$labels), function(i) mean(c(y_ranges[i], y_ranges[i+1]), na.rm = TRUE)),
        label = co$labels
      )
      p <- p +
        geom_text(
          data = co_labs,
          aes(x = min(df$Age, na.rm = TRUE), y = y, label = label),
          hjust = -0.1,
          vjust = 0,
          inherit.aes = FALSE,
          size = 4,
          color = "black"
        )
      
    }
    
    p
  })
}

shinyApp(ui, server)


