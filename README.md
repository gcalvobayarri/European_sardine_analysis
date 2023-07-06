European Sardine Analysis Code Documentation
SOFTWARE NEEDED: R and JAGS. All codes are in the R language.

This folder contains the statistical analysis carried out in the paper "Bayesian joint longitudinal models for assessing the exploitation rates of sardine stock in the Mediterranean Sea". The material available here is as follows.

1. Folder "bayes_factor": It includes 7 elements: ar_joint_model.R, arme_joint_model.R, log_marginal_comparison.R, marginal_likelihood_H1.RData, marginal_likelihood_H2.RData, marginal_likelihood_H3.RData, mixed_joint_model.R.
  · ar_joint_model.R, arme_joint_model.R, and mixed_joint_model.R: code to estimate the logarithm of the marginal likelihood through the bridgesampling package for M2, M3, and M1, respectively.
  · log_marginal_comparison.R: code to compare the different approximations.
  · marginal_likelihood_H1.RData, marginal_likelihood_H2.RData, and marginal_likelihood_H3.RData: the approximate log marginal likelihood through the bridgesampling package for M1, M2, and M3, respectively.

2. Folder "cpo_joint_log": It contains a total of 16 elements,
  · computing_missing_data_ar.R, computing_missing_data_arme.R, and computing_missing_data_mixed.R: code for the imputation of missing data based on the posterior distribution of models M2, M3, and M1, respectively. These imputations are necessary for the CPOs computations.
  · cpo_comparison.R: code to compare the different CPOs.
  · industrial_artisanal_matrix_nonas_ar.RData, industrial_artisanal_matrix_nonas_arme.RData, and industrial_artisanal_matrix_nonas_mixed.RData: data after the imputation process based on the posterior distribution of models M2, M3, and M1, respectively.
  · In the "ar_joint" folder:
    * cpo.R: code to compute the CPOs for model M2.
    * draws.R: code to save the realizations of the posterior distribution of M2 in a data frame.
    * log_cpo_ar_joint.RData: CPOs computed for model M2.
  · In the "ar_me_joint" folder:
    * cpo_v2.R: code to compute the CPOs for model M3.
    * draws_v2.R: code to save the realizations of the posterior distribution of M3 in a data frame.
    * log_cpo_arme_joint2.RData: CPOs computed for model M3.
  · In the "mixed_joint" folder:
    * cpo.R: code to compute the CPOs for model M1.
    * draws.R: code to save the realizations of the posterior distribution of M1 in a data frame.
    * log_cpo_mixed_joint.RData: CPOs computed for model M1.

3. Folder "data": It contains 4 elements,
  · annual_tonnes.RData: data frame on the total tonnage of sardines captured by year and country.
  · industrial_artisanal_tonnes.RData: data frame on the total tonnage of sardines captured by year and country, taking into account the type of fishery, artisanal or industrial.
  · matricial_data.R: code to transform the previous data frames into matrices.
  · SAU LME 26 v48-0.csv: data frame that contains information of all the fisheries in the Mediterranean Sea from 1950 to 2018. This data comes from www.seaaroundus.org.

4. Folder "figures": It includes the code for 4 different figures,
  · beta1s_arme_joint_posterior.R: Figure that shows the posterior distribution of the beta1 parameters for model M3.
  · rho0_rho1_arme_joint_posterior.R: Figure that shows the posterior distribution of the parameters rho0 and rho1 for model M3. These two parameters measure the linear association between the random effects of the artisanal response variable and the industrial response variable. This plot is not included in the manuscript.
  · Spaghetti_plots_fishery.R: Figures representing the different fishing dynamics of the countries.
  · plot_arme_joint_random_effects.R: Figure that shows the approximate posterior mean of the random effects parameters from model M3 and their estimated correlations (dashed lines).
   
5. Folder "functions": It contains a function that takes the logarithm of some values as inputs and returns the logarithmic sum of those values (suma_logaritmica_de_elementos.R).

6. Folder "models": It contains the three Bayesian joint longitudinal models used in the study in JAGS format,
  · autoregressive_and_me_joint.txt: model M3.
  · autoregressive_joint_v2.txt: model M2.
  · mixed_joint.txt: model M1.

7. Folder "results": It contains posterior samples from the three different models and their respective DICs,
  · dic_joint_ar.RData and res_autoregressive_jointv2log.RData: M2.
  · dic_joint_arme.RData and res_ar1_me_jointlog.RData: M3.
  · dic_joint_mixed.RData and res_mixed_jointlog.RData: M1.

8. Folder "scripts": It contains 5 scripts:
  · ar_and_me_joint_model.R: code for computing the JAGS model and viewing the posterior results for M3.
  · ar_joint_modelv2.R: code for computing the JAGS model and viewing the posterior results for M2.
  · DICs.R: code for computing the DIC of each model.
  · filtering_data.R: code that filters the data used in this study.
  · mixed_joint_modelv2.R: code for computing the JAGS model and viewing the posterior results for M1.
