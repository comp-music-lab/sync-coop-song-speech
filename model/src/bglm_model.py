from pathlib import Path
import sys

import arviz as az
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pymc as pm
import pytensor.tensor as pt
import seaborn as sns


class Model(object):
    """A class for estimating model parameters given response data.

    Parameters
    ----------
    trials : pd.DataFrame
        Pandas DataFrame, generated by simulate_data.Simulation

    hypothesis : str ('target')
        Toggle between target and null hypotheses ('target', 'null')

    intercept_mode : str ('fixed')
        Modes for calculating intercepts ('fixed', 'site', 'cohort', 'participant')


    ...


    Methods
    -------

    ...



    """
    def __init__(self, trials, **kwargs):

        self.trials = trials        # Trials generated by simulate_data.Simulation

        # Default parameters

        # Model type
        self.hypothesis = 'target'    # ['target', 'null']
        self.intercept_mode = 'fixed' # ['fixed', 'site', 'cohort', 'participant']

        # Model priors
        self.beta_prior_fn = pm.Normal      # Regression coefficient prior distribution
        self.beta_prior_mu = 0.0            # ... mean
        self.beta_prior_sigma = 10.0        # ... standard deviation

        self.alpha_prior_fn = pm.Normal             # Intercept prior distribution
        self.alpha_prior_mu = 0.0                   # ... mean
        self.alpha_prior_sigma = 10.0               # ... standard deviation

        self.alpha_mu_prior_fn = pm.Normal          # Random intercept mean prior distribution
        self.alpha_mu_prior_mu = 0.0                # ... mean
        self.alpha_mu_prior_sigma = 10.0            # ... standard deviation

        self.alpha_sigma_prior_fn = pm.Exponential  # Random intercept variance prior distribution
        self.alpha_sigma_prior_lambda = 1.0         # ... rate parameter

        self.sigma_prior_fn = pm.HalfNormal         # Prior distribution for error variance
        self.sigma_prior_sigma = 1.0                # ... standard deviation

        # MCMC parameters
        self.draws = 2000           # MCMC number of samples per chain for statistics
        self.tune = 1000            # MCMC number of samples per chain for tuning


        # Default parameters can be changed by passing their names
        # as keyword arguments, e.g.,
        #   sim = Simulation(n_site=2, n_cohort=50)

        # Update parameters by reading keyword arguments
        for k, v in kwargs.items():
            setattr(self, k, v)


        # Load basic attributes from trial data
        self.sites = np.unique(self.trials.site)
        self.cohorts = np.unique(self.trials.cohort)
        self.participants = np.unique(self.trials.participant)


    def run_model(self):
        self.model = pm.Model()

        with self.model:
            # Priors for regression parameters
            if self.hypothesis == 'null':
                beta = self.beta_prior_fn("beta", mu=self.beta_prior_mu, sigma=self.beta_prior_sigma, shape=2)

            elif self.hypothesis == 'target':
                beta = self.beta_prior_fn("beta", mu=self.beta_prior_mu, sigma=self.beta_prior_sigma, shape=3)

            # Priors for random effects (intercepts)
            if self.intercept_mode != 'fixed':
                alpha_prior_mu = self.alpha_mu_prior_fn("alpha_mu", mu=self.alpha_mu_prior_mu, sigma=self.alpha_mu_prior_sigma)
                alpha_prior_sigma = self.alpha_sigma_prior_fn("alpha_sigma", self.alpha_sigma_prior_lambda)

            # Intercepts
            if self.intercept_mode == 'fixed':
                # In this case there is only one intercept, use the class
                # attributes for prior distribution parameters
                alpha = self.alpha_prior_fn("alpha", mu=self.alpha_prior_mu, sigma=self.alpha_prior_sigma)

            elif self.intercept_mode == 'site':
                a_per_site = self.alpha_prior_fn("alpha", mu=alpha_prior_mu, sigma=alpha_prior_sigma, shape=self.sites.size)
                alpha = a_per_site[self.trials.site]

            elif self.intercept_mode == 'cohort':
                a_per_cohort = self.alpha_prior_fn("alpha", mu=alpha_prior_mu, sigma=alpha_prior_sigma, shape=self.cohorts.size)
                alpha = a_per_cohort[self.trials.cohort]

            elif self.intercept_mode == 'participant':
                a_per_participant = self.alpha_prior_fn("alpha", mu=alpha_prior_mu, sigma=alpha_prior_sigma, shape=self.participants.size)
                alpha = a_per_participant[self.trials.participant]

            # Priors for noise (error term)
            sigma = self.sigma_prior_fn("sigma", sigma=self.sigma_prior_sigma)

            # expected value of outcome
            if self.hypothesis == 'null':
                mu = beta[0] * self.trials.synchronous + beta[1] * self.trials.verbal + alpha
            else:
                mu = beta[0] * self.trials.singing + beta[1] * self.trials.synchronous + beta[2] * self.trials.verbal + alpha

            # Likelihood (sampling distribution) of observations
            Y_obs = pm.Normal("Y_obs", mu=mu, sigma=sigma, observed=self.trials.response)

            # draw 1000 posterior samples
            self.trace = pm.sample(draws=self.draws, tune=self.tune)


#############################################################################
### Model Comparison


def compare_models(model1, model2):
    for M in [model1, model2]:
        with M.model:
            pm.compute_log_likelihood(M.trace)
    df_comp_loo = az.compare({'hypothesis':model1.trace, 'null':model2.trace})
    return df_comp_loo




