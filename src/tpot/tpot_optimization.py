from tpot import TPOTClassifier
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd

mydata = pd.read_csv('/home/abbas/myProjects/211128_ostadkar_fullfillment/push-up/data/Cleaning_data_extracted-r1400-02_cleaned.csv')
#print(mydata[['is_delivery_at_holiday',  'is_created_at_holiday']])

#print(mydata)
#print(mydata.dtypes)
#print(mydata.district.unique())
X_train, X_test, y_train, y_test = train_test_split(mydata.drop('label_wn_quote', axis=1, inplace=False), 
    mydata[['label_wn_quote']], train_size=0.75, test_size=0.25, random_state=42)

#help(TPOTClassifier)

tpot = TPOTClassifier(generations=5, population_size=50, verbosity=2, random_state=42,\
    n_jobs= -1, use_dask = True, \
    periodic_checkpoint_folder = '/home/abbas/myProjects/211128_ostadkar_fullfillment')
tpot.fit(X_train.to_numpy(), y_train.to_numpy().ravel())

tpot.export('tpot_iris_pipeline.py')
print(tpot.score(X_test.to_numpy(), y_test.to_numpy().ravel()))
