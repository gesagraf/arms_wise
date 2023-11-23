#### setup ####
library(ggplot2)
library(dplyr)

#### data ####
data <- data.frame(
  key = factor(rep(c("1mean", "2minmax", "3nvp", "4glvp"), each = number), ordered = T,
               labels = c("Arithmetisches Mittel", "Alternativer Schätzer", "Normalverteilert Bayesschätzer", "Gleichverteilert Bayesschätzer")),
  value = c(estimators, minmax, bayesWerte, bayesWerteNV)
)




summary_data <- data %>%
  group_by(key) %>%
  summarise(mean_value = mean(value))

#### plot ####

ggplot(data, aes(x = value)) +
  geom_histogram(fill = "lightgrey", color = "black") +

  # every sample as triangle
  geom_point(aes(x = value, y = 0, color = "estimators"), shape = 17) +


  # mu
  geom_point(aes(x = mu, y = 2, colour = "mu"), shape = 17) +
  geom_vline(aes(xintercept = mu, colour = "mu")) +


  # Mittelwerte pro key einzeichnen
  geom_point(data = summary_data, aes(x = mean_value, y = 4, colour = "mean_estBayes"),
             shape = 17) +
  geom_vline(data = summary_data, aes(xintercept = mean_value, colour = "mean_estBayes"),
             linetype = "dashed") +

  facet_wrap(~key, ncol = 2) +


  # Skalen, Theme, Labs etc.
  #  xlim(mu - sd, mu + sd) +  # xAchse festlegen
  # coord_cartesian()
  labs(
    title = "Stichprobenkennwerteverteilung \n je nach Schätzer",
    y = "Häufigkeit",
    x = "x",
    colour = NULL) +
  scale_color_manual(values = c(mu = "red", mean_estBayes = "black", estimators = "darkgrey"),
                     labels = c(mu = paste0("Populationsmittelwert (", expression(mu), ")"), mean_estBayes = "Mittelwert aller Schätzer",
                                estimators = "Schätzer pro Stichprobe")) +
  theme_bw()

