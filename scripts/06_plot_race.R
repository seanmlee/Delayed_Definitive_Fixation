

# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
library(ggsignif)


# preds ------------------------------------------------------------------------
critval <- 1.96

newdata <- expand.grid(
  Age = mean(as.numeric(orif$Age)),
  Gender = "Male",
  Race = c("Black or African American", "White"),
  Open = "Yes",
  Extremity = c("Tibia/Fibula", "Femur", "Ankle"),
  Insurance = "Government",
  Smoke = "No",
  Drugs = "No",
  Alcohol = "No",
  COPD = "No",
  Renal = "No",
  Cirrhosis = "No",
  CHF = "No",
  Diabetes = "No",
  GCS = "15",
  ISS = mean(as.numeric(orif$ISS))
)

preds <- predict(
  mod_stepwise, 
  newdata = newdata, 
  type = "link", 
  se.fit = TRUE
)

fit_link <- preds$fit

fit_response <- mod_stepwise$family$linkinv(fit_link)

upr_link <- preds$fit + (critval * preds$se.fit)

lwr_link <- preds$fit - (critval * preds$se.fit)

upr_response <- mod_stepwise$family$linkinv(upr_link)

lwr_response <- mod_stepwise$family$linkinv(lwr_link)

fit <- as.data.frame(
  cbind(
    newdata,
    fit_response, 
    upr_response,
    lwr_response
  )
)


# plot -------------------------------------------------------------------------
p_Ankle <- data.frame(
  Extremity = "Ankle",
  xmin = 1,
  xmax = 2,
  y_position = 24.25,
  annotation = "p=0.002"
)

p_Femur <- data.frame(
  Extremity = "Femur",
  xmin = 1,
  xmax = 2,
  y_position = 15.5,
  annotation = "p=0.641"
)

p_TibiaFibula <- data.frame(
  Extremity = "Tibia/Fibula",
  xmin = 1,
  xmax = 2,
  y_position = 23.25,
  annotation = "p=0.569"
)

plot_Race <-
fit %>%
  
  ggplot(
    
    aes(
      x = Race, 
      y = fit_response
    )
    
  ) +
  
  geom_beeswarm(
    
    data = orif,
    aes(
      x = Race,
      y = exfix_orif,
      fill = Extremity
    ),
    pch = 21,
    color = "black",
    size = 3.5,
    cex = 4,
    method = "hex"
    
  ) +
  
  geom_errorbar(
    
    aes(
      ymin = lwr_response,
      ymax = upr_response
    ),
    width = 0.65,
    size =  0.75,
    position = position_dodge(width = 0.5)
    
  ) +
  
  geom_point(
    
    pch = 21,
    color = "black",
    fill = "white",
    size = 16,
    alpha = 0.75,
    stroke = 1,
    position = position_dodge(width = 0.5),
    show.legend = FALSE
    
  ) +
  
  scale_y_continuous(
    limits = c(0, 25)
  ) +
  
  ggtitle("b)") +
  
  xlab("") +
  
  ylab("Ex-fix to ORIF (Days)") +
  
  scale_x_discrete(
    labels = c("Black or\nAfrican\nAmerican", "White")
  ) +
  
  theme_bw() +
  
  theme(
    
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 0)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.background = element_blank(),
    strip.text = element_text(size = 20)
    
  ) +
  
  geom_text(
    aes(
      label = sprintf("%.1f", fit_response)
    ),
    position = position_dodge(width = 0.5),
    hjust = 0.5,
    size = 6,
    fontface = "bold"
  ) +
  
  facet_grid(
    ~ Extremity
  ) + 
  
  geom_signif(
    data = p_Ankle,
    aes(
      xmin = xmin,
      xmax = xmax,
      annotations = annotation,
      y_position = y_position
      ),
    manual = TRUE
  ) + 
  
  geom_signif(
    data = p_Femur,
    aes(
      xmin = xmin,
      xmax = xmax,
      annotations = annotation,
      y_position = y_position
    ),
    manual = TRUE
  ) + 
  
  geom_signif(
    data = p_TibiaFibula,
    aes(
      xmin = xmin,
      xmax = xmax,
      annotations = annotation,
      y_position = y_position
    ),
    manual = TRUE
  )


# write ------------------------------------------------------------------------
ggsave(
  "out/plot_Race.png",
  width = 8,
  height = 5
)
