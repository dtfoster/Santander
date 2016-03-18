import pandas as pd

import numpy as np
import csv
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier, RandomForestClassifier, GradientBoostingClassifier
from xgboost import XGBClassifier
from sklearn.cross_validation import StratifiedKFold
from sklearn import ensemble

print('Load data...')
train = pd.read_csv("../raw/train.csv")
target = np.array(train['TARGET'].values)
id_train = train['ID'].values
train = train.drop(['ID', 'TARGET'], axis=1)

test = pd.read_csv("../raw/test.csv")
id_test = test['ID'].values
test = test.drop(['ID'], axis=1)

cross_val = StratifiedKFold(target, n_folds=4, shuffle=False, random_state=2016)

X_train = np.array(train)
X_test = np.array(test)

print('Training...')

extc = ExtraTreesClassifier(n_estimators=850,max_features= 60,criterion= 'entropy',min_samples_split= 4,
                            max_depth= 40, min_samples_leaf= 2, n_jobs = 2)

rfc = RandomForestClassifier(n_estimators=850,max_features= 60,criterion= 'entropy',min_samples_split= 4,
                            max_depth= 40, min_samples_leaf= 2, n_jobs = 2)

gbc = GradientBoostingClassifier(n_estimators=850,max_features= 60, min_samples_split= 4,
                            max_depth= 40, min_samples_leaf= 2)

xgb = XGBClassifier(learning_rate=0.01, max_depth=40, n_estimators=500)

models = {'extc': extc, 'rfc': rfc, 'gbc': gbc, 'xgb': xgb}

for model_name, model in models.iteritems():
	#validation
	val_preds = []
	for train_index, test_index in cross_val:
		val_X_train, val_X_test = X_train[train_index], X_train[test_index]
		val_y_train, val_y_test = target[train_index], target[test_index]
		model.fit(val_X_train, val_y_train)
		val_pred = model.predict_proba(val_X_test.tolist())
		if len(val_preds):
			val_preds = np.concatenate((val_preds, val_pred), axis=0)
		else:
			val_preds = val_pred
	pd.DataFrame({"ID": id_train, "PredictedProb": val_preds[:,1]}).to_csv('output/validation/%s_val_pred.csv'%(model_name), index=False)

	#prediction
	model.fit(X_train,target)
	pred = model.predict_proba(X_test)
	pd.DataFrame({"ID": id_test, "PredictedProb": pred[:,1]}).to_csv('output/submissions/%s_pred.csv'%(model_name), index=False)