# Mean and sd of the log marginal likelihood approximation 

load('./bayes_factor/marginal_likelihood_H1.RData')
load('./bayes_factor/marginal_likelihood_H2.RData')
load('./bayes_factor/marginal_likelihood_H3.RData')

mean(marginal_likelihood_H1); sd(marginal_likelihood_H1)
mean(marginal_likelihood_H2); sd(marginal_likelihood_H2)
mean(marginal_likelihood_H3); sd(marginal_likelihood_H3)
