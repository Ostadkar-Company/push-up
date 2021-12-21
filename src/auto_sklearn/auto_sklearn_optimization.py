import pandas as pd
import os
import shutil
import sklearn.datasets
import sklearn.metrics
import PipelineProfiler
import autosklearn.classification
import matplotlib.pyplot as plt
import pickle


if __name__ == "__main__":


	mydata = pd.read_csv('/home/abbas/myProjects/211128_ostadkar_fullfillment/push-up/data/Cleaning_data_extracted-r1400-02_cleaned.csv')


	X_train, X_test, y_train, y_test = sklearn.model_selection.train_test_split(mydata.drop('label_wn_quote', axis=1, inplace=False),
		mydata[['label_wn_quote']], train_size=0.75, test_size=0.25, random_state=42)

	X_train = X_train.to_numpy()
	y_train = y_train.to_numpy().ravel()

	X_test = X_test.to_numpy()
	y_test = y_test.to_numpy().ravel()


	feat_type = ['Categorical', 'Categorical', 'Numerical', 'Categorical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Categorical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical', 'Numerical']
	dataset_name = 'ostadkar_cleaning_fulfillment'
	time_left_for_this_task= int(60*60*0.5)
	per_run_time_limit=60*15
	initial_configurations_via_metalearning = None
	max_models_on_disc = 80
	seed = 1367
	memory_limit = 13*1000
	resampling_strategy = 'cv'
	resampling_strategy_arguments = {'folds': 5}
	tmp_folder='/home/abbas/myProjects/211128_ostadkar_fullfillment/push-up/src/auto_sklearn/out3/tmp2'
	output_directory = '/home/abbas/myProjects/211128_ostadkar_fullfillment/push-up/src/auto_sklearn/out3'
	delete_output_folder_after_terminate = False
	delete_tmp_folder_after_terminate = False
	n_jobs = -1

	#help(autosklearn.classification.AutoSklearnClassifier)

	automl = autosklearn.classification.AutoSklearnClassifier(
		n_jobs = n_jobs,
		time_left_for_this_task = time_left_for_this_task,
		per_run_time_limit = per_run_time_limit,
		#initial_configurations_via_metalearning = initial_configurations_via_metalearning,
		max_models_on_disc = max_models_on_disc,
		seed = seed,
		memory_limit = memory_limit,
		resampling_strategy = resampling_strategy,
		resampling_strategy_arguments = resampling_strategy_arguments,
        #output_folder=output_directory,
		tmp_folder = tmp_folder,
		#,
		delete_tmp_folder_after_terminate = delete_tmp_folder_after_terminate,
		delete_output_folder_after_terminate = delete_output_folder_after_terminate

	)
	automl.fit(X_train, y_train,
		feat_type = feat_type,
		dataset_name = dataset_name)
	#profiler_data = PipelineProfiler.import_autosklearn(automl)
	#PipelineProfiler.plot_pipeline_matrix(profiler_data)
    #with open('/home/abbas/disppage.htm','wb') as f:   # Use some reasonable temp name
    #    f.write(PipelineProfiler.plot_pipeline_matrix(profiler_data).html.encode("UTF-8"))
	
	

	with open('/home/abbas/myProjects/211128_ostadkar_fullfillment/push-up/src/auto_sklearn/automl-classifier3.pkl', 'wb') as f:
		pickle.dump(automl, f)    
    # load model
    #with open('iris-classifier.pkl', 'rb') as f:
    #    loaded_classifier = pickle.load(f)


predictions = automl.predict(X_test)
print("Accuracy score", sklearn.metrics.accuracy_score(y_test, predictions))


poT = automl.performance_over_time_
poT.plot(
    x='Timestamp',
    kind='line',
    legend=True,
    title='Auto-sklearn accuracy over time',
    grid=True,
)
plt.show()

from sklearn.inspection import plot_partial_dependence, permutation_importance

r = permutation_importance(automl, X_test, y_test, n_repeats=10, random_state=0)
sort_idx = r.importances_mean.argsort()[::-1]

plt.boxplot(r.importances[sort_idx].T,
            labels=[list(set(list(mydata))-set(['label_wn_quote']))[i] for i in sort_idx])

plt.xticks(rotation=90)
plt.tight_layout()
plt.show()

plt.savefig('books_read.png')


#######################
from autosklearn.experimental.askl2 import AutoSklearn2Classifier

automl = AutoSklearn2Classifier(
		n_jobs = n_jobs,
		time_left_for_this_task = time_left_for_this_task,
		per_run_time_limit = per_run_time_limit,
		#initial_configurations_via_metalearning = initial_configurations_via_metalearning,
		max_models_on_disc = max_models_on_disc,
		seed = seed,
		memory_limit = memory_limit,
		#resampling_strategy = resampling_strategy,
		#resampling_strategy_arguments = resampling_strategy_arguments,
		tmp_folder = tmp_folder,
		#,
		delete_tmp_folder_after_terminate = delete_tmp_folder_after_terminate,
		delete_output_folder_after_terminate = delete_output_folder_after_terminate

	)

tmp_folder='/home/abbas/myProjects/211128_ostadkar_fullfillment/push-up/src/auto_sklearn/out4/tmp2'

automl.fit(X_train, y_train,
           feat_type = feat_type,
           dataset_name = dataset_name)