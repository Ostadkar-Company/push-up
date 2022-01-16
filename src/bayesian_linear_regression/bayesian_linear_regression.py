#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 16 16:06:29 2022

@author: abbas
"""



import arviz as az
import bambi as bmb
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pymc3 as pm

from pymc3 import HalfCauchy, Model, Normal, glm, plot_posterior_predictive_glm, sample, math



print(f"Running on PyMC3 v{pm.__version__}")



RANDOM_SEED = 1367
rng = np.random.default_rng(RANDOM_SEED)

#%config InlineBackend.figure_format = 'retina'
az.style.use("arviz-darkgrid")


mydata = pd.read_csv('/home/abbas/myProjects/211128_ostadkar_fullfillment/push-up/data/Cleaning_data_extracted-r1400-02_cleaned.csv')

mydata.dtypes
########
#filling missing values
mydata.loc[mydata.isnull().any(axis=1)].shape[0]
cols_with_missing_vals = mydata.columns[mydata.isnull().any()].tolist()


for col in cols_with_missing_vals:
    mydata.fillna(mydata[col].median(), inplace = True)


mydata.loc[mydata['train_test'].isnull()]
#################
mydata_train_pd = mydata.loc[mydata['train_test'] == 'train'].\
    drop(['train_test'], axis=1, inplace=False)




#cur_predictors = list(mydata_train_pd)#.remove('label_wn_quote')

cur_data_train_pd = mydata_train_pd
cur_predictors = [x for x in cur_data_train_pd if x != 'label_wn_quote']


#https://docs.pymc.io/en/v3/pymc-examples/examples/generalized_linear_models/GLM-linear.html


model_str = 'label_wn_quote ~ ' + ' + '.join(cur_predictors)
model = bmb.Model(model_str, cur_data_train_pd)

trace = model.fit(draws=3000)



"""
with Model() as model:
    
    X = cur_data_train_pd.drop(['label_wn_quote'], axis=1, inplace=False).to_numpy()
    y = cur_data_train_pd[['label_wn_quote']].to_numpy()
    
    
    intercept = Normal('alpha', mu=0, sd=10) 
    #sigma = pm.HalfNormal('sigma', sd=1) # error term
    sigma = HalfCauchy("sigma", beta=10, testval=1.0)#https://docs.pymc.io/en/v3/api/distributions/continuous.html#pymc3.distributions.continuous.HalfCauchy
    beta = Normal('beta', mu=0, sd=10, shape= X.shape[1])
    
    likelihood = Normal("y", mu=intercept + math.dot(X, beta), observed=y)
    trace = sample(3000, return_inferencedata=True)

############################################

pd.set_option('display.max_colwidth', None)
pd.set_option('display.max_columns', None)

cur_data_train_pd.loc[cur_data_train_pd.isnull().any(axis=1)].shape[0]

cur_data_train_pd.shape[0]
    
    
    
    # Define priors in a list
    #distr_ls = []
    #distr_names_ls = []
    
    
    sigma = HalfCauchy("sigma", beta=10, testval=1.0)#https://docs.pymc.io/en/v3/api/distributions/continuous.html#pymc3.distributions.continuous.HalfCauchy
    intercept = Normal("Intercept", 0, sigma=20)
    x_coeff = Normal("x", 0, sigma=20)
    
    # Define likelihood
    likelihood = Normal("y", mu=intercept + x_coeff * x, sigma=sigma, observed=y)

    # Inference!
    # draw 3000 posterior samples using NUTS sampling
    trace = sample(3000, return_inferencedata=True)
    
"""