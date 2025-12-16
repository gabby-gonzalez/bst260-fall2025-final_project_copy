
library(shiny)
library(NHANES)
library(dplyr)
library(ggplot2)

data("NHANES")

df<-NHANES


##basic test plots
##edited titles in actual shiny plot

#systolic blood pressure
plot1 <- df %>%
  drop_na(Age, BPSysAve) %>%
  filter(Age>=20) %>%
  ggplot(aes(x = Age, y = BPSysAve)) +
  geom_point(
    aes(color = Gender),
    alpha = 0.2,
    size = 1,
    position = position_jitter(width = 0.35, height = 0)
  )+
  geom_smooth(
    aes(color = Gender),
    method = "gam",
    formula = y ~ s(x),
    se = FALSE,
    linewidth = 1.2
  )+
  facet_wrap(~ Gender, scales = "free") +  
  theme_bw() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15)
  )
plot1



#diastolic blood pressure
plot2 <- df %>%
  drop_na(Age, BPDiaAve) %>%
  filter(Age>=20) %>%
  ggplot(aes(x = Age, y = BPDiaAve)) +
  geom_point(
    aes(color = Gender),
    alpha = 0.2,
    size = 1,
    position = position_jitter(width = 0.35, height = 0)
  )+
  geom_smooth(
    aes(color = Gender),
    method = "gam",
    formula = y ~ s(x),
    se = FALSE,
    linewidth = 1.2
  )+
  facet_wrap(~ Gender, scales = "free") +  
  theme_bw() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15)
  )
plot2



#total cholesterol
plot3 <- df %>%
  drop_na(Age, TotChol) %>%
  filter(Age>=20) %>%
  ggplot(aes(x = Age, y = TotChol)) +
  geom_point(
    aes(color = Gender),
    alpha = 0.2,
    size = 1,
    position = position_jitter(width = 0.35, height = 0)
  )+
  geom_smooth(
    aes(color = Gender),
    method = "gam",
    formula = y ~ s(x),
    se = FALSE,
    linewidth = 1.2
  )+
  facet_wrap(~ Gender, scales = "free") +  
  theme_bw() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15)
  )
plot3

#testosterone
plot4 <- df %>%
  drop_na(Age, Testosterone) %>%
  filter(Age>=20) %>%
  ggplot(aes(x = Age, y = Testosterone)) +
  geom_point(
    aes(color = Gender),
    alpha = 0.2,
    size = 1,
    position = position_jitter(width = 0.35, height = 0)
  )+
  geom_smooth(
    aes(color = Gender),
    method = "gam",
    formula = y ~ s(x),
    se = FALSE,
    linewidth = 1.2
  )+
  facet_wrap(~ Gender, scales = "free") +  
  theme_bw() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15)
  )
plot4


#pulse
plot5 <- df %>%
  drop_na(Age, Pulse) %>%
  filter(Age>=20) %>%
  ggplot(aes(x = Age, y = Pulse)) +
  geom_point(
    aes(color = Gender),
    alpha = 0.2,
    size = 1,
    position = position_jitter(width = 0.35, height = 0)
  )+
  geom_smooth(
    aes(color = Gender),
    method = "gam",
    formula = y ~ s(x),
    se = FALSE,
    linewidth = 1.2
  )+
  facet_wrap(~ Gender, scales = "free") +  
  theme_bw() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15)
  )
plot5


#pulse
plot6 <- df %>%
  drop_na(Age, UrineVol1) %>%
  filter(Age>=20) %>%
  ggplot(aes(x = Age, y = UrineVol1)) +
  geom_point(
    aes(color = Gender),
    alpha = 0.2,
    size = 1,
    position = position_jitter(width = 0.35, height = 0)
  )+
  geom_smooth(
    aes(color = Gender),
    method = "gam",
    formula = y ~ s(x),
    se = FALSE,
    linewidth = 1.2
  )+
  facet_wrap(~ Gender, scales = "free") +  
  theme_bw() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15)
  )
plot6

