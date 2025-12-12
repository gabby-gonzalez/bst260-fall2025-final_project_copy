library(shiny)
library(NHANES)
library(tidyverse)

data("NHANES")


#####################
## Allen
#####################

ui_0 = fluidPage(
  titlePanel("NHANES Data Explorer"),
  p("Tanvi Gaitonde, Gabrielle Gonzalez, Allen Gu, Camila Guetter"),
  hr(),
  p("This Shiny app offers three interactive dashboards, each displaying of a 
    subset of data from the National Health and Nutrition Examination Survey 
    (NHANES). Please select one of the below to get started."),
  hr(),
  
  actionButton("go_1", "Physical Activity and BMI", class = "btn-primary"),
  p("Explore how physical activity associates with body mass index (BMI), with 
    options to alter plot type, variable type (categorical/continuous), and
    Gender."),
  hr(),
  
  actionButton("go_2", "Clinical Parameters", class = "btn-primary"),
  p("Explore the distribution of various clinical parameters by Age, Gender and Race, 
    with implementation as well of a smoothing function."),
  hr(),
  
  actionButton("go_3", "Age, Education, and Household Income", class = "btn-primary"),
  p("Explore a heatmap examining Household Income by Age and Education status, with 
    options to group by Gender and Race. The hover functionality is utilized."),
)


#####################
## Camilla
#####################

# Choices for PhysActiveDays (0–7)
pa_days_choices = sort(unique(NHANES$PhysActiveDays[!is.na(NHANES$PhysActiveDays)]))

ui_1 = fluidPage(
  titlePanel("Physical Activity and BMI"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "xvar_1",
        label   = "X-axis (Physical Activity):",
        choices = c(
          "Physical Activity (Yes/No)"        = "PhysActive",
          "Physical Activity Days per Week"   = "PhysActiveDays"
        ),
        selected = "PhysActive"
      ),
      selectInput(
        inputId = "yvar_1",
        label   = "Y-axis (BMI):",
        choices = c(
          "BMI (continuous)"   = "BMI",
          "BMI Category (WHO)" = "BMI_WHO"
        ),
        selected = "BMI"
      ),
      selectInput(
        inputId = "facet_1",
        label   = "Facet by:",
        choices = c(
          "None"   = "none",
          "Gender" = "Gender"
        ),
        selected = "Gender"
      ),
      
      # Toggle between boxplot and jitter when BMI is continuous
      conditionalPanel(
        condition = "input.yvar_1 == 'BMI'",
        radioButtons(
          inputId = "geom_type_1",
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
        condition = "input.xvar_1 == 'PhysActiveDays'",
        checkboxGroupInput(
          inputId  = "padays_filter_1",
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

server_1 = function(input, output, session) {
  
  
  plot_data = reactive({
    df = NHANES
    
    # X-variable filters
    if (input$xvar_1 == "PhysActive") {
      df = df |> filter(!is.na(PhysActive))
      
    } else if (input$xvar_1 == "PhysActiveDays") {
      df = df |> filter(!is.na(PhysActiveDays))
      
      # Apply selected PA days
      if (!is.null(input$padays_filter_1)) {
        df = df |> 
          filter(PhysActiveDays %in% as.numeric(input$padays_filter_1))
      }
    }
    
    # Y-variable filters
    if (input$yvar_1 == "BMI") {
      df = df |> filter(!is.na(BMI))
      
    } else if (input$yvar_1 == "BMI_WHO") {
      df = df |> filter(!is.na(BMI_WHO))
    }
    
    # Capitalize gender for facet labels
    df$Gender = tools::toTitleCase(as.character(df$Gender))
    
    df
  })
  
  output$pa_bmi_plot = renderPlot({
    df = plot_data()
    
    # ----- Continuous BMI -----
    if (input$yvar_1 == "BMI") {
      
      if (input$xvar_1 == "PhysActive") {
        # Base plot for PhysActive
        p = ggplot(df, aes(x = PhysActive, y = BMI, fill = PhysActive))
        
        if (input$geom_type_1 == "box") {
          p = p + geom_boxplot()
        } else {
          p = p +
            geom_jitter(
              aes(color = PhysActive),
              width = 0.2,
              alpha = 0.35,
              size = 1.8
            ) +
            guides(color = "none")
        }
        
        p = p +
          labs(
            title = "BMI by Physical Activity Status",
            x = "Physical Activity (Yes/No)",
            y = "BMI"
          ) +
          guides(fill = "none")
        
      } else {
        # Base plot for PhysActiveDays
        p = ggplot(df, aes(
          x    = factor(PhysActiveDays),
          y    = BMI,
          fill = factor(PhysActiveDays)
        ))
        
        if (input$geom_type_1 == "box") {
          p = p + geom_boxplot()
        } else {
          p = p +
            geom_jitter(
              aes(color = factor(PhysActiveDays)),
              width = 0.2,
              alpha = 0.35,
              size = 1.8
            ) +
            guides(color = "none")
        }
        
        
        p = p +
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
      if (input$xvar_1 == "PhysActive") {
        p = ggplot(df, aes(x = PhysActive, fill = BMI_WHO)) +
          geom_bar(position = "fill") +
          labs(
            title = "BMI Categories by Physical Activity Status",
            x = "Physical Activity (Yes/No)",
            y = "Proportion",
            fill = "BMI Category"
          )
      } else {
        p = ggplot(df, aes(x = factor(PhysActiveDays), fill = BMI_WHO)) +
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
    if (input$facet_1 == "Gender") {
      p = p + facet_wrap(~ Gender)
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



#########################
## Gabby
#########################
ui_2 = fluidPage(
  titlePanel("Clinical Parameters"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      
      ##add to y-slider
      selectInput(
        "yvar_2",
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

server_2 = function(input, output, session) {
  
  # Render plot
  output$bpPlot = renderPlot({
    
    # Filter and clean data
    df = NHANES %>%
      tidyr::drop_na(.data[[input$yvar_2]], Gender, Race1) %>%
      mutate(Gender = stringr::str_to_title(Gender)) %>%
      filter(Age >= input$ageRange[1], Age <= input$ageRange[2]) %>%
      rename(Race=Race1)
    
    
    # Decide color variable
    color_var = if (input$raceColor) "Race" else "Gender"
    
    
    #specify title
    
    y_labels = c(
      BPSysAve = "Systolic Blood Pressure (mmHg)",
      BPDiaAve = "Diastolic Blood Pressure (mmHg)",
      TotChol = "Total Cholesterol (mmol/L)",
      Testosterone = "Testosterone (ng/dL)",
      Pulse = "Pulse (bpm)",
      UrineVol1 = "Urine Volume (mL)"
    )
    #--------------------------    
    
    # Base scatter plot
    p = df %>%
      ggplot(aes(x = Age, y = .data[[input$yvar_2]])) +
      geom_point(
        aes(color = .data[[color_var]]),
        alpha = 0.2,
        size = 1,
        position = position_jitter(width = 0.35, height = 0)
      )+
      facet_wrap(~ Gender, scales = "free") +  
      theme_bw() +
      labs(
        y = y_labels[input$yvar_2] %||% input$yvar_2,
        color = color_var
      ) +
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
      p = p +
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

#########################
## Tanvi
#########################

ui_3 = fluidPage(
  titlePanel("Household Income"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "xvar_3",
        label   = "X-axis (Age):",
        choices = c(
          "Age (Decade)"        = "AgeDecade",
          "Age (Continuous)"   = "Age"
        ),
        selected = "AgeDecade"
      ),
      selectInput(
        inputId = "yvar_3",
        label   = "Y-axis (Education):",
        choices = c(
          "Education Level"   = "Education"
        ),
        selected = "Education"
      ),
      selectInput(
        inputId = "facet_3",
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

server_3 = function(input, output, session) {
  
  # Reactive data filtered based on selected X
  plot_data = reactive({
    df = NHANES
    
    if (input$xvar_3 == "AgeDecade") {
      df = df |> 
        filter(!is.na(AgeDecade),!is.na(HHIncomeMid)) |>
        select(AgeDecade, Education, HHIncomeMid, Gender, Race1)
    } else if (input$xvar_3 == "Age") {
      df = df |> 
        filter(!is.na(Age),!is.na(HHIncomeMid)) |>
        select(Age, Education, HHIncomeMid, Gender, Race1)
    }
    if (input$yvar_3 == "Education") {
      df = df |> filter(!is.na(Education))
    }
    df
  })
  
  output$agedec_edu_hhinc_heatmap = renderPlot({
    df = plot_data()
    
    if (input$xvar_3 == "AgeDecade") {
      # AgeDecade and Education on Household Income → Heatmap
      p = ggplot(df, aes(x = AgeDecade, y = Education, fill = HHIncomeMid)) +
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
      p = ggplot(df, aes(x = Age, y = Education, fill = HHIncomeMid)) +
        geom_tile() +
        labs(title = "Household Income",
             x = "Age", 
             y = "Education Level", 
             fill = "Household Income") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_minimal()
    }
    
    # Add faceting if selected
    if (input$facet_3 == "Gender") {
      p = p + facet_wrap(~ Gender)
    }
    else if (input$facet_3 == "Race1") {
      p = p + facet_wrap(~ Race1)
    }
    else if (input$facet_3 == "Gender + Race1") {
      p = p + facet_wrap(~ Gender + Race1, nrow = 2)
    }
    
    p
  })
  
  output$hover_info = renderPrint({
    df = plot_data()   # USE REACTIVE DATA
    
    if (is.null(input$plot_hover)) return(NULL)
    
    hover_data = nearPoints(
      df,
      input$plot_hover,
      xvar = input$xvar_3,
      yvar = input$yvar_3,
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



#########################
## Merge - Allen
#########################

ui = fluidPage(
  tabsetPanel(
    id = "main_tabs",

    tabPanel("Introduction", ui_0),
    tabPanel("Physical Activity and BMI", ui_1),
    tabPanel("Clinical Parameters", ui_2),
    tabPanel("Household Income", ui_3)
  )
)

server = function(input, output, session) {
  server_1(input, output, session)
  server_2(input, output, session)
  server_3(input, output, session)
  
  observeEvent(input$go_1, {
    updateTabsetPanel(session, "main_tabs",
                      selected = "Physical Activity and BMI")
  })
  
  observeEvent(input$go_2, {
    updateTabsetPanel(session, "main_tabs",
                      selected = "Clinical Parameters")
  })
  
  observeEvent(input$go_3, {
    updateTabsetPanel(session, "main_tabs",
                      selected = "Household Income")
  })
}

shinyApp(ui, server)