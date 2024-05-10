from itertools import product
from pathlib import Path
import sys

import pandas as pd
import numpy as np


class Simulation(object):
    """A class for simulating participant response data.

    Given an underlying model of the factors affecting social bonding,
    and a sample sizes, generates participant responses.
    Models of different degrees of complexity can be simulated.


    Parameters
    ----------
    n_site : int (10)
        Number of sites

    n_cohort : int (10)
        Number of cohorts per site

    n_part : int (10)
        Number of participants per cohort

    cond_per_part : int (1)
        Number of conditions per participant (max 5)

    drop_frac : float (between 0 and 1; default 0)
        Fraction of trials to drop

    beta : list ([1.4, 0.6, 1.0])
        Regression coefficients for trial conditions [singing, synchronous, and verbal]

    intercept_mode : str ('fixed')
        Modes for calculating intercepts ('fixed', 'site', 'cohort', 'participant')

    intercept_mu : float (1.0)
        Mean value of the normal distribution from which intercepts are generated

    intercept_sigma : float (0.5)
        Standard deviation of the normal distribution from which intercepts are generated

    noise_sigma : float (1.0)
        Standard deviation of the normal distribution from which noise is


    Methods
    -------
    generate_trials()
        Generates X, independent variables; Data is stored in self.trials

    generate_responses()
        Generates Yobs, response variables; Data is stored in self.trials



    """
    def __init__(self, **kwargs):

        # Default parameters
        self.n_site = 10            # Number of sites
        self.n_cohort = 10          # Number of cohorts per site
        self.n_part = 10            # Number of participants per cohort
        self.cond_per_part = 1      # Number of conditions per participant (max 5)
        self.drop_frac = 0.0        # Fraction of unusable data / missing data
                                    # (e.g. participants not turning up)

        self.beta = [1.4, 0.6, 1.0] # Coefficients for [singing, synchronous, verbal]
        self.intercept_mode = 'fixed' # ['fixed', 'site', 'cohort', 'participant']
        self.intercept_mu = 1.0     # Mean intercept
        self.intercept_sigma = 0.5  # Intercept standard deviation
        self.noise_sigma = 1.0      # Standard deviation of noise

        # Default parameters can be changed by passing their names
        # as keyword arguments, e.g.,
        #   sim = Simulation(n_site=2, n_cohort=50)

        # Update parameters by reading keyword arguments
        for k, v in kwargs.items():
            setattr(self, k, v)


        self.conditions = self._get_conditions()
        self.n_condition = len(self.conditions)


    def _get_conditions(self):
        conditions = np.array([[1, 1, 1], # [singing, synchronous, verbal]
                               [0, 0, 1],
                               [1, 0, 1],
                               [0, 1, 1],
                               [0, 0, 0]
                              ])
        return conditions


    def generate_trials(self):
        # Initialize list to hold trial data
        data = []

        # Intialize counters for cohorts, participants and conditions
        count_cohort = 0
        count_part = 0
        count_cond = 0

        # Iterate over sites
        for site in range(self.n_site):
            # Iterate over cohorts per site 
            for _ in range(self.n_cohort):
                # Iterate over participants per cohort
                for _ in range(self.n_part):
                    # Iterate over conditions
                    for _ in range(self.cond_per_part):
                        condition = list(self.conditions[count_cond % self.n_condition])
                        data.append([site, count_cohort, count_part] + condition)
                        count_cond += 1
                    count_part += 1
                count_cohort += 1

        # Save unique values for site, cohort and participant
        self.sites = np.arange(self.n_site)
        self.cohorts = np.arange(count_cohort)
        self.participants = np.arange(count_part)

        columns = ['site', 'cohort', 'participant', 'singing', 'synchronous', 'verbal']
        self.trials = pd.DataFrame(data=data, columns=columns)

        # Randomly drop a fraction of the trials;
        # default behavior is to skip this.
        if self.drop_frac > 0:
            n_drop = int(len(self.trials) * self.drop_frac)
            drop_idx = np.random.choice(self.trails.index, replace=False, size=n_drop)
            self.trials = self.trials.drop(index=drop_idx)


    def generate_intercepts(self):
        """Generate intercepts per trial.

        Generate a fixed intercept, or a set of intercepts
        depending on the intercept mode.
        Intercepts are generated from a normal distribution

        Modes:
            fixed          ::  Randomly selects a single value
            site           ::  Randomy selects a value per site
            cohort         ::  Randomy selects a value per cohort
            participant    ::  Randomy selects a value per participant
        """

        if self.intercept_mode == 'fixed':
            self.intercept = np.random.normal(self.intercept_mu, self.intercept_sigma)
            self.trials['intercept'] = self.intercept

        elif self.intercept_mode == 'site':
            self.intercept = np.random.normal(self.intercept_mu, self.intercept_sigma, size=self.sites.size)
            self.trials['intercept'] = self.intercept[self.trials.site]

        elif self.intercept_mode == 'cohort':
            self.intercept = np.random.normal(self.intercept_mu, self.intercept_sigma, size=self.cohorts.size)
            self.trials['intercept'] = self.intercept[self.trials.cohort]

        elif self.intercept_mode == 'participant':
            self.intercept = np.random.normal(self.intercept_mu, self.intercept_sigma, size=self.participants.size)
            self.trials['intercept'] = self.intercept[self.trials.participant]


    def generate_responses(self):
        """Generate responses for each trial.

        Yobs = beta_0 * singing + beta_1 * synchronous + beta_2 * verbal + intercept + noise
        """

        # Simulate deterministic part of outcome variable
        Yobs = self.beta[0] * self.trials.singing + self.beta[1] * self.trials.synchronous + self.beta[2] * self.trials.verbal

        # Generate and add intercepts
        self.generate_intercepts()
        Yobs = Yobs + self.trials.intercept

        # Add noise
        Yobs = Yobs + np.random.normal(0, self.noise_sigma, size=len(self.trials))

        self.trials['response'] = Yobs

        

                
