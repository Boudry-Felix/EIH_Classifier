import numpy as np
import pandas as pd
import sklearn.metrics
from sklearn import tree
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
import lightgbm as lgb
import pyreadr
import shap
import matplotlib
import dtreeviz

viz_data = r.viz_data
train_x = pd.DataFrame(r.light_gbm_train_data)
train_y = np.array(r.light_gbm_train_data_label)
test_x = pd.DataFrame(r.light_gbm_test_data)
test_y = np.array(r.light_gbm_test_data_label)


dtrain = lgb.Dataset(train_x, label = train_y)

param = r.lgbm_params

gbm = lgb.train(param, dtrain, num_boost_round = 800)

preds = gbm.predict(test_x)
pred_labels = np.rint(preds)
accuracy = sklearn.metrics.accuracy_score(test_y, pred_labels)

lgb.plot_tree(gbm)
matplotlib.pyplot.show()
