import seaborn as sb 
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats

# use sklearn package to run models
from sklearn.preprocessing import  MinMaxScaler 
from sklearn.model_selection import  StratifiedShuffleSplit
from sklearn.metrics import mean_absolute_error, r2_score
from sklearn.utils import resample
 
def getTrainTestSplit(df):

    y = df['Age at Visit'] # outcome var

    # Drop the column with the independent variable (age), subject ID and dxgroup
    X  = df.drop(['Age at Visit', 'BD#', 'Dx'], axis = 1).astype('float64')

    scaler = MinMaxScaler()
    # fit scaler on data
    scaler.fit(X)
    # apply transform
    MinMaxNormalized = scaler.transform(X)
    age_bins = [30, 40, 50, 60, 70, 80]

    # Assign each data point to its corresponding age bin
    age_bin_indices = np.digitize(y, age_bins) 

    # Perform stratified shuffle split
    splitter = StratifiedShuffleSplit(n_splits=1, test_size=0.5, random_state=42)
    train_indices, test_indices = next(splitter.split(MinMaxNormalized, age_bin_indices))
    
    # Get the train and test sets
    X_train, X_test = MinMaxNormalized[train_indices],  MinMaxNormalized[test_indices] 
    y_train, y_test = y[train_indices], y[test_indices]  

    return(X_train, X_test, y_train, y_test, train_indices, test_indices)

# function using loop to create multiple bootstrap samples, fitting the Ridge Regression model on each sample, 
# and then collecting the coefficient estimates from each iteration
 
def performBootstrap(X, y, n_samples, model, random_state =42):
    coefs = []  # Store coefficient estimates from each bootstrap sample
    
    for i in range(n_samples):
        # resample from the 60 individuals 
        X_boot, y_boot = resample(X, y, random_state=random_state+i, replace=True)
 
        model.fit(X_boot, y_boot)
        coefs.append(model.coef_)
    return np.array(coefs)

# Model evaluation using bootstrapped samples
def evaluateModel(X_train, X_test, y_train, y_test, n_bootstrap_samples, model):
    bootstrap_mae = []
    bootstrap_r = []
    bootstrap_r2 = []
    for i in range(n_bootstrap_samples):
        X_boot, y_boot = resample(X_train, y_train, random_state=42+i, replace=True)
        
        model.fit(X_boot, y_boot)
        y_pred = model.predict(X_test)  # Assuming you have a separate test set
        
        mae = mean_absolute_error(y_test, y_pred)
        bootstrap_mae.append(mae)
        
        r = np.corrcoef(y_test, y_pred)
        bootstrap_r.append(r)
        
        r2= r2_score(y_test, y_pred)
        bootstrap_r2.append(r2)
    return bootstrap_mae, bootstrap_r, bootstrap_r2
