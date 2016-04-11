import pandas as pd
from xgbUtilis import xgb_make_model_with_stopping
import numpy as np
from zmodel.xgb import make_model_with_stopping
import csv
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier, RandomForestClassifier, GradientBoostingClassifier
import xgboost as xgb
from sklearn.cross_validation import StratifiedKFold, train_test_split
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

params = { "objective": "binary:logistic",
                "booster": "gbtree",
                "eval_metric": "auc",
                "eta": 0.0202,
                "max_depth": 5,
                "subsample": 0.6815,
                "colsample_bytree": 0.701}

model = make_model_with_stopping(train, target, params)

test = xgb.DMatrix(test)

preds = model.predict(test)
print preds
pd.DataFrame({"ID": id_test, "TARGET": preds}).to_csv('output/submissions/%s_pred.csv'%("xgb"), index=False)
