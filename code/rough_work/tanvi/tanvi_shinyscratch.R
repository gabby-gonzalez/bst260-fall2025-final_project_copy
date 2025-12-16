###TANVI SHINY SCRATCH

library(shiny)
library(NHANES)
library(dplyr)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(circlize)

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
          "Age (Continuous → binned)" = "Age"
        ),
        selected = "AgeDecade"
      ),
      selectInput(
        inputId = "facet",
        label   = "Facet by:",
        choices = c(
          "None" = "none",
          "Gender" = "Gender",
          "Race" = "Race1",
          "Gender + Race" = "Gender + Race1"
        ),
        selected = "none"
      )
    ),
    mainPanel(
      InteractiveComplexHeatmapOutput("heatmap")
    )
  )
)

server <- function(input, output, session) {
  
  # --------------------
  # Load & clean data
  # --------------------
  heatmap_data <- reactive({
    df <- NHANES %>% filter(!is.na(HHIncomeMid), !is.na(Education))
    
    if (input$xvar == "AgeDecade") {
      df <- df %>% filter(!is.na(AgeDecade))
      df$AgeGroup <- df$AgeDecade
    } else {
      df <- df %>% filter(!is.na(Age))
      df$AgeGroup <- cut(df$Age, breaks = seq(0, 80, 10))
    }
    
    if (input$facet == "none") {
      df$Facet <- "All"
    } else {
      df$Facet <- df[[input$facet]]
    }
    
    df
  })
  
  # --------------------
  # Create heatmap dynamic object
  # --------------------
  dynamic_heatmap <- reactive({
    df <- heatmap_data()
    
    wide <- df %>%
      group_by(Education, AgeGroup, Facet) %>%
      summarise(HHIncomeMid = mean(HHIncomeMid, na.rm = TRUE)) %>%
      ungroup()
    
    facet_levels <- unique(wide$Facet)
    
    hm_list <- lapply(facet_levels, function(facet_level) {
      tmp <- wide %>% filter(Facet == facet_level)
      
      mat <- tidyr::pivot_wider(
        tmp,
        names_from = AgeGroup,
        values_from = HHIncomeMid
      )
      
      rownames(mat) <- mat$Education
      mat <- as.matrix(mat[,-1])
      
      Heatmap(
        mat,
        name = paste("Income", facet_level),
        col = circlize::colorRamp2(
          c(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE)),
          c("lightblue", "darkblue")
        ),
        na_col = "white",
        column_title = paste("Facet:", facet_level)
      )
    })
    
    Reduce(`+`, hm_list)
  })
  
  # --------------------
  # Render dynamic interactive heatmap
  # --------------------
  output$heatmap <- renderUI({
    ht <- dynamic_heatmap()
    InteractiveComplexHeatmapWidget(
      input, output, session,
      ht
    )
  })
  
}


shinyApp(ui = ui, server = server)

# ui <- fluidPage(
#   titlePanel("NHANES: Age, Education, and Household Income"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(
#         inputId = "xvar",
#         label   = "X-axis (Physical Activity):",
#         choices = c(
#           "Physical Activity (Yes/No)"        = "PhysActive",
#           "Physical Activity Days per Week"   = "PhysActiveDays"
#         ),
#         selected = "PhysActive"
#       ),
#       selectInput(
#         inputId = "yvar",
#         label   = "Y-axis (BMI):",
#         choices = c(
#           "BMI (continuous)"   = "BMI",
#           "BMI Category (WHO)" = "BMI_WHO"
#         ),
#         selected = "BMI"
#       ),
#       selectInput(
#         inputId = "facet",
#         label   = "Facet by:",
#         choices = c(
#           "None"   = "none",
#           "Gender" = "Gender"
#         ),
#         selected = "Gender"
#       )
#     ),
#     
#     mainPanel(
#       plotOutput("pa_bmi_plot")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   # Reactive data filtered based on selected X and Y
#   plot_data <- reactive({
#     df <- NHANES
#     
#     if (input$xvar == "PhysActive") {
#       df <- df |> filter(!is.na(PhysActive))
#     } else if (input$xvar == "PhysActiveDays") {
#       df <- df |> filter(!is.na(PhysActiveDays))
#     }
#     
#     if (input$yvar == "BMI") {
#       df <- df |> filter(!is.na(BMI))
#     } else if (input$yvar == "BMI_WHO") {
#       df <- df |> filter(!is.na(BMI_WHO))
#     }
#     
#     df
#   })
#   
#   output$pa_bmi_plot <- renderPlot({
#     df <- plot_data()
#     
#     # ----- Y = continuous BMI -----
#     if (input$yvar == "BMI") {
#       
#       if (input$xvar == "PhysActive") {
#         # PhysActive vs BMI → boxplot
#         p <- ggplot(df, aes(x = PhysActive, y = BMI, fill = PhysActive)) +
#           geom_boxplot() +
#           labs(
#             title = "BMI by Physical Activity Status",
#             x = "Physical Activity (Yes/No)",
#             y = "BMI"
#           ) +
#           guides(fill = "none")
#         
#       } else {
#         # PhysActiveDays vs BMI → your boxplot by gender (with optional facet)
#         p <- ggplot(df, aes(
#           x    = factor(PhysActiveDays),
#           y    = BMI,
#           fill = factor(PhysActiveDays)
#         )) +
#           geom_boxplot() +
#           labs(
#             title = "BMI by Physical Activity Days per Week",
#             x = "Physical Activity Days (0–7)",
#             y = "BMI"
#           ) +
#           theme_minimal() +
#           guides(fill = "none")
#       }
#       
#     } else {
#       # ----- Y = BMI_WHO (categorical) -----
#       if (input$xvar == "PhysActive") {
#         p <- ggplot(df, aes(x = PhysActive, fill = BMI_WHO)) +
#           geom_bar(position = "fill") +
#           labs(
#             title = "BMI Categories by Physical Activity Status",
#             x = "Physical Activity (Yes/No)",
#             y = "Proportion",
#             fill = "BMI Category (WHO)"
#           )
#       } else {
#         p <- ggplot(df, aes(x = factor(PhysActiveDays), fill = BMI_WHO)) +
#           geom_bar(position = "fill") +
#           labs(
#             title = "BMI Categories by Physical Activity Days per Week",
#             x = "Physical Activity Days per Week (0–7)",
#             y = "Proportion",
#             fill = "BMI Category (WHO)"
#           )
#       }
#     }
#     
#     # Add faceting if selected
#     if (input$facet == "Gender") {
#       p <- p + facet_wrap(~ Gender)
#     }
#     
#     p + theme_minimal()
#   })
# }
# 
# shinyApp(ui = ui, server = server)
