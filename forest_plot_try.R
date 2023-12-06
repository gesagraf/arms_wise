# forest plot
dat <- data.frame(
  key = rep(c("mean", "minmax", "uni", "nv"), each = number),
  value =  c(estimators, minmax, bayesWerte, bayesWerteNV),
  mu_hat = mu_hat
)

quantiles <- dat %>%
  group_by(key) %>%
  summarise(Q025 = quantile(value, probs = 0.025),
            Q975 = quantile(value, probs = 0.975))

dat_with_quantiles <- left_join(dat, quantiles, by = "key")

means <- c(mean_est, mean_minmax, mean_estBayes, mean_estBayesNV)

ggplot(dat_with_quantiles, aes(x = value, group = key)) +
  geom_point() +
  geom_errorbar(aes(ymin = Q025, ymax = Q975), width = 0.1) +
  labs(title = "Error bars for Quantiles by Key", x = "Value", y = "Mu_hat")


geom_point(aes(x = means, y = 1:4), shape = 23) +
  coord_cartesian(xlim = coord) +
  theme_bw()

p <- ggplot(df, aes(resp, colour = group))
p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
