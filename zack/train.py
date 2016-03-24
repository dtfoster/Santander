import pandas as pd

import numpy as np
import csv
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier, RandomForestClassifier, GradientBoostingClassifier
import xgboost as xgb
from sklearn.cross_validation import StratifiedKFold
from sklearn import ensemble
from sklearn import preprocessing

print('Load data...')
train = pd.read_csv("../raw/train.csv")
target = np.array(train['TARGET'].values)
id_train = train['ID'].values
train = train.drop(['ID', 'TARGET'], axis=1)

test = pd.read_csv("../raw/test.csv")
id_test = test['ID'].values
test = test.drop(['ID'], axis=1)

for f in train.columns:
    if train[f].dtype=='object':
        lbl = preprocessing.LabelEncoder()
        lbl.fit(list(train[f].values))
        train[f] = lbl.transform(list(train[f].values))

for f in test.columns:
    if test[f].dtype=='object':
        lbl = preprocessing.LabelEncoder()
        lbl.fit(list(test[f].values))
        test[f] = lbl.transform(list(test[f].values))

train.fillna((-999), inplace=True)
test.fillna((-999), inplace=True)

train=np.array(train)
test=np.array(test)
train = train.astype(float)
test = test.astype(float)

print('Training...')

watchlist = [(test, 'eval'), (train, 'train')]

params = { "objective": "binary:logistic",
                "booster": "gbtree",
                "eval_metric": "auc",
                "eta": 0.02,
                "max_depth": 6,
                "subsample": 0.9,
                "colsample_bytree": 0.85}

param = {'max_depth':2, 'eta':1, 'silent':1, 'objective':'binary:logistic' }

train = xgb.DMatrix(train)

xgb = xgb.train(param, train, 500, watchlist)


exit

cross_val = StratifiedKFold(target, n_folds=4, shuffle=False, random_state=2016)

models = {'xgb': xgb}

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