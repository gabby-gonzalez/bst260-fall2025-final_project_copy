library(shiny)
library(tidyverse)
library(NHANES)

str(NHANES)

p <- df %>%
  ggplot(aes(x = Age, y = .data[[input$yvar]])) +
  geom_density_2d_filled(alpha = 0.8) +   # filled density
  geom_point(alpha = 0.2, size = 1) +     # optional raw points
  facet_wrap(~ Gender, scales = "free") +
  theme_bw() +
  labs(y = input$yvar, fill = "Density")




# ------------------------
# UI
# ------------------------
ui <- fluidPage(
  titlePanel("Clinical Biomarkers"),
  
  sidebarLayout(
    sidebarPanel(
      
    
      
      ##add to y-slider
      selectInput(
        "yvar",
        "Y Variable",
        choices = c(
          "Systolic Blood Pressure" = "BPSysAve",
          "Diastolic Blood Pressure" = "BPDiaAve",
          "Total Cholesterol" = "TotChol",
          "Testosterone" = "Testosterone",
          "Pulse" = "Pulse",
          "Urine Volume" = "UrineVol1"
        ),
        selected = "BPSysAve"
      ),
      
      sliderInput("ageRange", "Age Range", min = 20, max = 80, value = c(20, 80)),
      
      checkboxInput("addSmooth", "Show Trend Line", value = TRUE),
      checkboxInput("raceColor", "Group by Race", value = FALSE)
      # checkboxInput("showCutoffs", "Show Clinical Cutoffs", value = TRUE)
      
    ),
    
    mainPanel(
      plotOutput("bpPlot")
    )
  )
)

# ------------------------
# Server
# ------------------------
server <- function(input, output, session) {
  
  # Render plot
  output$bpPlot <- renderPlot({
    
    # Filter and clean data
    df <- NHANES %>%
      drop_na(.data[[input$yvar]], Gender, Race1) %>%
      mutate(Gender = str_to_title(Gender)) %>%
      filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
  
    
    # Decide color variable
    color_var <- if (input$raceColor) "Race1" else "Gender"
    
    
    #specify title
    
y_labels <- c(
      BPSysAve = "Systolic Blood Pressure (mmHg)",
      BPDiaAve = "Diastolic Blood Pressure (mmHg)",
      TotChol = "Total Cholesterol (mmol/L)",
      Testosterone = "Testosterone (ng/dL)",
      Pulse = "Pulse (bpm)",
      UrineVol1 = "Urine Volume (mL"
    )
#--------------------------    
    
    # Base scatter plot
    p <- df %>%
      ggplot(aes(x = Age, y = .data[[input$yvar]])) +
    geom_point(
      aes(color = .data[[color_var]]),
      alpha = 0.2,
      size = 1,
      position = position_jitter(width = 0.35, height = 0)
    )+
      facet_wrap(~ Gender, scales = "free") +  
      theme_bw() +
      labs(y = y_labels[[input$yvar]], color = color_var) +
      theme(
        legend.position = "right",
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        strip.text = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 15)
      )
#------------------------
    
 #selection options
    # Add trend line if selected
    if (input$addSmooth) {
      p <- p +
        geom_smooth(
          aes(color = .data[[color_var]]),
          method = "gam",
          formula = y ~ s(x),
          se = FALSE,
          linewidth = 1.2
        )
    }

    p
  })
  
}

# Run the app
shinyApp(ui, server)


str(NHANES)

