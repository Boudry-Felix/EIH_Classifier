import lightgbm as lgb
import xgboost as xgb
import numpy as np
import pandas as pd
import keras
import autokeras as ak
from sklearn.model_selection import train_test_split
from pytorch_tabular import TabularModel
from tensorflow.random import set_seed

set_seed(int(r.project_seed))

# Data import
train_x = pd.DataFrame(r.ml_train_data["values"])
train_y = np.array(r.ml_train_data["label"]).ravel()
test_x = pd.DataFrame(r.ml_test_data["values"])
test_y = np.array(r.ml_test_data["label"]).ravel()
dtrain = lgb.Dataset(train_x, label=train_y)
train, val = train_test_split(pd.DataFrame(r.dl_train), 
                              random_state = int(r.project_seed))

# LightGBM
model_path = r.model_path + "/LGBM.txt"
lgbm_model = lgb.Booster(model_file=model_path)
# lgbm_params = lgbm_model.get_params()
lgbm_pred_y = lgbm_model.predict(test_x)
lgbm_pred_y = np.round(lgbm_pred_y)

# XGBoost
model_path = r.model_path + "/XGBoost.json"
xgboost_model = xgb.XGBClassifier()
xgboost_model.load_model(fname=model_path)
# xgboost_params = xgboost_model.get_params()
xgboost_pred_y = xgboost_model.predict(test_x)
feature_names = xgboost_model.get_booster().feature_names

# NODE
model_path = r.model_path + "/NODE"
node_model = TabularModel.load_model(model_path)

node_pred = node_model.predict(test_x)
node_pred_y = node_pred['prediction']

# GANDALF
model_path = r.model_path + "/GANDALF"
gandalf_model = TabularModel.load_model(model_path)

gandalf_pred = gandalf_model.predict(test_x)
gandalf_pred_y = gandalf_pred['prediction']

# DANET
model_path = r.model_path + "/DANET"
danet_model = TabularModel.load_model(model_path)

danet_pred = danet_model.predict(test_x)
danet_pred_y = danet_pred['prediction']
