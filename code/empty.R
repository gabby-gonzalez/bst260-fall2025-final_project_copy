
library(tidyverse)
library(ggplot2)
library(shiny)

#load nhanes
library(NHANES)
dat<-NHANES
dat
str(dat)
attach(dat)

##demographic factors by biomarkers
##systolic diastolic BP, testosterone, direct cholesterol, #urine flow
str(dat)
unique(dat$Gender)
dat2<-dat %>%
  filter(Gender=="female" & Testosterone<170)


#two faceted side by side plots - one for diastolic and one for systolic BP
#categorize by clinical groups as outcomes
#tabs for gender, agedecade, race,sleephourspernight,alcohol, smoking, drugs

unique(dat$HardDrugs)

dat %>%
  drop_na(BPSysAve) |>
  mutate(BPSysAve=as.numeric(BPSysAve)) %>%
  mutate(AlcoholDay=as.factor(AlcoholDay)) %>%
  mutate(BP_Sys_Cat = case_when(
    BPSysAve < 120 ~ "Normal",
    BPSysAve >= 120 & BPSysAve < 130 ~ "Elevated",
    BPSysAve >= 130 & BPSysAve <= 139 ~ "Stage 1 Hypertension",
    BPSysAve >= 140 ~ "Stage 2 Hypertension"
  ))%>%
  ggplot(aes(x=BP_Sys_Cat, fill=Race1))+
  geom_bar(color="black") +
  scale_fill_brewer(palette = "Reds") +
  theme_bw()

str(dat)



dat %>%
  drop_na(BPSysAve) |>
  mutate(BPSysAve=as.numeric(BPSysAve)) %>%
  mutate(AlcoholDay=as.factor(AlcoholDay)) %>%
  mutate(BP_Sys_Cat = case_when(
    BPSysAve < 120 ~ "Normal",
    BPSysAve >= 120 & BPSysAve < 130 ~ "Elevated",
    BPSysAve >= 130 & BPSysAve <= 139 ~ "Stage 1 Hypertension",
    BPSysAve >= 140 ~ "Stage 2 Hypertension"
  ))%>%
  ggplot(aes(x=Age, y=BPSysAve, color=Gender))+
  geom_point() +
  geom_smooth()+
  scale_fill_brewer(palette = "Reds") +
  theme_bw()

str(dat)
sum(dat$BPSysAve >= 140, na.rm = TRUE)

max(dat$BP_Sys_Cat)



library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Blood Pressure Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "yvar",
        "Y Variable",
        choices = c(
          "Systolic Blood Pressure" = "BPSysAve",
          "Diastolic Blood Pressure" = "BPDiaAve",
          "Total Cholesterol", "TotChol",
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
  output$bpPlot <- renderPlot({
    
    dat %>%
      drop_na(BPSysAve) %>%
      mutate(
        BPSysAve = as.numeric(BPSysAve),
        AlcoholDay = as.factor(AlcoholDay),
        BP_Sys_Cat = case_when(
          BPSysAve < 120 ~ "Normal",
          BPSysAve >= 120 & BPSysAve < 130 ~ "Elevated",
          BPSysAve >= 130 & BPSysAve <= 139 ~ "Stage 1 Hypertension",
          BPSysAve >= 140 ~ "Stage 2 Hypertension"
        )
      ) %>%
      ggplot(aes(x = Age, y = .data[[input$yvar]], color = Gender)) +
      geom_point() +
      theme_bw()
  })
}

shinyApp(ui, server)


