if(!require(microbenchmark))install.packages("microbenchmark");library(microbenchmark)
if(!require(Rfast))install.packages("Rfast");library(Rfast)

## benchmark likelihood estimation
set.seed(666)
samp_df <-
  sapply(1:100, FUN = function(i) {
    samp <- rnorm(n = 30, mean = 0, sd = 15)}
)



lengthout<-1000
mu_hat <- seq(-200, 200, length.out = lengthout)

##benchmark for loop and sapply and sapply with sd computation before
like_bench<-microbenchmark(
  "server_return_list_uni1" = {
    norm_likelihood_uni <- matrix(ncol = 100, nrow = lengthout)


    for (i in 1:100) {

      likelihood_function <-
        sapply(mu_hat, FUN = function(i_mu){                                # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
          prod(dnorm(samp_df[ , i], mean = i_mu, sd = sd(samp_df[ , i]))) # prod = prdukt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
        })                                                                  # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()






      # Normierung der Likelihood
      den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
      likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood

      # normierte likelihood speicher, für die plots
      norm_likelihood_uni[ , i] <- likelihood_function_norm
    }
    norm_likelihood_uni

    },
  "one_sapply" = {
    sapply(1:100, function(i){
      likelihood_function<-sapply(mu_hat, FUN = function(i_mu){
        prod(dnorm(samp_df[, i], mean = i_mu, sd = sd(samp_df[, i])))
      })
      den_like<-Bolstad::sintegral(mu_hat, likelihood_function)
      likelihood_function/den_like$value
    })},


    "sd_bevore" = {
      sapply(1:100, function(i){
        sd_i<-apply(samp_df, 2, sd)
        likelihood_function<-sapply(mu_hat, FUN = function(i_mu){
          prod(dnorm(samp_df[, i], mean = i_mu, sd = sd_i[i]))
        })
        # likelihood_function<-apply(likelihood_function, 2, prod)
        den_like<-Bolstad::sintegral(mu_hat, likelihood_function)
        likelihood_function/den_like$value
        })
    }
  ,
  times = 20)
like_bench

library(ggplot2)
autoplot(like_bench)


###############################################################debugging#########################################################
# norm_likelihood_uni <- matrix(ncol = 100, nrow = lengthout)
#
#
# for (i in 1:100) {
#
#   likelihood_function <-
#     sapply(mu_hat, FUN = function(i_mu){                                # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
#       prod(dnorm(samp_df[ , i], mean = i_mu, sd = sd(samp_df[ , i]))) # prod = prdukt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
#     })                                                                  # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()
#
#
#
#
#
#
#   # Normierung der Likelihood
#   den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
#   likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood
#
#   # normierte likelihood speicher, für die plots
#   norm_likelihood_uni[ , i] <- likelihood_function_norm
# }
# temp2<-sapply(1:number(), function(i){
#   likelihood_function <-
#     sapply(mu_hat, FUN = function(i_mu){                                # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
#       prod(dnorm(samp_df()[ , i], mean = i_mu, sd = sd(samp_df()[ , i]))) # prod = prdukt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
#     })                                                                  # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()
#
#
#
#
#
#
#   # Normierung der Likelihood
#   den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
#   likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood
# })
#
# temp2<-sapply(1:100, function(i){
#   sd_i<-apply(samp_df, 2, sd)
#
#   likelihood_function<-sapply(mu_hat, FUN = function(i_mu){
#     prod(dnorm(samp_df[, i], mean = i_mu, sd = sd_i[i]))
#   })
#   den_like<-Bolstad::sintegral(mu_hat, likelihood_function)
#   likelihood_function/den_like$value
# })
#
#
#
# temp3<-array(samp_df)
#
# temp4<-mapply(function(x, mu) {
#   print(x)
#   print(mu)
#   prod(dnorm(x, mean = mu, sd = sd(x)))
# } ,
#        samp_df, mu_hat)
#
# all.equal(norm_likelihood_uni, temp2)
#
# samp_temp<-samp_df[, 1:3]
# mu_temp<-mu_hat[1:3]
#
# mapply(function(x, y){
#   print(x)
#   print(y)
# },
# samp_temp, mu_temp)
#
#
# mapply(function(x, y) seq_len(x) + y,
#        c(a =  1, b = 2, c = 3),  # names from first
#        c(A = 10, B = 0, C = -10))
# dyn.load("C:/Program Files/R/R-4.2.3/library/stats/libs/x64/stats.dll")
#
# sd_without_checks<-function (x)
# {
#   sqrt(.Call(stats:::C_cov, x, NULL, 4, FALSE))
# }
#
# sd_bench<-microbenchmark("sd" = sd(samp_df[,1]),
#                          "sd_without_checks" = sd_without_checks(samp_df[,1]))
#
# autoplot(sd_bench)
#
#
# sd(samp_df[,1])
# sd_without_checks(samp_df[,1])
