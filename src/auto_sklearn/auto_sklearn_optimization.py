import pandas as pd
import os
import shutil
import sklearn.datasets
import sklearn.metrics

import autosklearn.classification


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
	time_left_for_this_task=60*60*24
	per_run_time_limit=60*20
	initial_configurations_via_metalearning = None
	max_models_on_disc = None
	seed = 1367
	memory_limit = 14*1000
	resampling_strategy = 'cv'
	resampling_strategy_arguments = {'folds': 5}
	tmp_folder='autosklearn'
	output_folder = 'autosklearn_out'
	delete_output_folder_after_terminate = True
	delete_tmp_folder_after_terminate = True
	n_jobs = -1

	if os.path.exists(os.getcwd() + '/' + output_folder):
		#os.remove(os.getcwd() + '/' + output_folder)
		shutil.rmtree(os.getcwd() + '/' + output_folder)
	if os.path.exists(os.getcwd() + '/' + tmp_folder):
		shutil.rmtree(os.getcwd() + '/' + tmp_folder)


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
		tmp_folder = tmp_folder#,
		#output_folder = output_folder,
		#e_tmp_folder_after_terminate = delete_tmp_folder_after_terminate,
		#delete_output_folder_after_terminate = delete_output_folder_after_terminate

	)
	automl.fit(X_train, y_train,
		feat_type = feat_type,
		dataset_name = dataset_name)
