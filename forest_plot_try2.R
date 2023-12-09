## Example data frame
dat <- data.frame(
  Index = c(1, 2, 3, 4), ## This provides an order to the data
  label = c("Arithmetisches Mittel", "Alternativer Schätzer", "Bayesschätzer mit gleichverteilter Priori", "Bayesschätzer mit normalverteilter Priori"),
  OR = c(mean(estimators), mean(minmax), mean(bayesWerte), mean(bayesWerteNV)),
  LL = c(mean(estimators)-1.96*sd(estimators), mean(minmax)-1.96*sd(minmax), mean(bayesWerte)-1.96*sd(bayesWerte), mean(bayesWerteNV)-1.96*sd(bayesWerteNV)),
  UL = c(mean(estimators)+1.96*sd(estimators), mean(minmax)+1.96*sd(minmax), mean(bayesWerte)+1.96*sd(bayesWerte), mean(bayesWerteNV)+1.96*sd(bayesWerteNV)),
  frb = colours[c("est_mean", "est_minmax", "est_bayes_uni", "est_bayes_nv")])
dat

(plot1 <- ggplot(dat, aes(y = Index, x = OR)) +
  geom_point(shape = 18) +
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25, linewidth = 1, colour = dat$frb) +
  geom_vline(xintercept = mu, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:4, labels = dat$label, trans = "reverse") +
  xlab("Wert gemittelt") +
  ylab(" ") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = dat$frb),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black")))
plot1
