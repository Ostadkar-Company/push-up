import pandas as pd

import sklearn.datasets
import sklearn.metrics

import autosklearn.classification


"""
X, y = sklearn.datasets.load_breast_cancer(return_X_y=True)
print (type(X))
print('--------')
print (type(y))
print('--------')
print(X)
print('--------')
print(y)
"""


mydata = pd.read_csv('/home/abbas/myProjects/211128_ostadkar_fullfillment/push-up/data/Cleaning_data_extracted-r1400-02_cleaned.csv')
#print(mydata[['is_delivery_at_holiday',  'is_created_at_holiday']])

#print(mydata)
#print(mydata.dtypes)
#print(mydata.district.unique())
X_train, X_test, y_train, y_test = sklearn.model_selection.train_test_split(mydata.drop('label_wn_quote', axis=1, inplace=False), 
    mydata[['label_wn_quote']], train_size=0.75, test_size=0.25, random_state=42)

X_train = X_train.to_numpy()
y_train = y_train.to_numpy().ravel()

X_test = X_test.to_numpy()
y_test = y_test.to_numpy().ravel()


help(autosklearn.classification.AutoSklearnClassifier)
"""
tpot = TPOTClassifier(generations=5, population_size=50, verbosity=2, random_state=42,\
    n_jobs= -1, use_dask = True, \
    periodic_checkpoint_folder = '/home/abbas/myProjects/211128_ostadkar_fullfillment')
tpot.fit(X_train.to_numpy(), y_train.to_numpy().ravel())

tpot.export('tpot_iris_pipeline.py')
print(tpot.score(X_test.to_numpy(), y_test.to_numpy().ravel()))
"""
