import optuna
import numpy as np
import pandas as pd
import sklearn.metrics
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import confusion_matrix, f1_score, cohen_kappa_score, accuracy_score
import lightgbm as lgb
from flaml import AutoML
import matplotlib.pyplot as plt
import shap

sampler = optuna.samplers.TPESampler(seed=10)

choice = r.params

train_x = pd.DataFrame(r.lgbm_train_data_intensity["values"])
train_y = np.array(r.lgbm_train_data_intensity["label"])
train_y = train_y.ravel()
test_x = pd.DataFrame(r.lgbm_test_data_intensity["values"])
test_y = np.array(r.lgbm_test_data_intensity["label"])
test_y = test_y.ravel()
dtrain = lgb.Dataset(train_x, label=train_y)

if choice["new_LGBM_params"]:
  def objective(trial):
      train_x = pd.DataFrame(r.lgbm_train_data_intensity["values"])
      train_y = np.array(r.lgbm_train_data_intensity["label"])
      train_y = train_y.ravel()
      test_x = pd.DataFrame(r.lgbm_test_data_intensity["values"])
      test_y = np.array(r.lgbm_test_data_intensity["label"])
      test_y = test_y.ravel()
      dtrain = lgb.Dataset(train_x, label=train_y)

      param = {
        'boosting_type': 'gbdt',
        'objective': 'multiclass',
        'metric': 'multi_logloss',
        'num_class': 4,
        'learning_rate': trial.suggest_float('learning_rate',0.05, 3),
        'lambda_l1': trial.suggest_float('lambda_l1', 1e-3, 5.0),
        'lambda_l2': trial.suggest_float('lambda_l2', 1e-3, 5.0),
        'num_leaves': trial.suggest_int('num_leaves', 3, 700),
        'max_depth': trial.suggest_int('max_depth', 5, 120),
        'feature_fraction': trial.suggest_float('feature_fraction', 0.6, 1.0),
        'bagging_fraction': trial.suggest_float('bagging_fraction', 0.5, 1.0),
        'bagging_freq': trial.suggest_int('bagging_freq', 2, 6),
        'drop_rate': trial.suggest_float('drop_rate', 0.001, 0.7),
        'reg_alpha': trial.suggest_float('reg_alpha', 0.1, 10),
        'reg_lambda': trial.suggest_float('reg_lambda', 0.1, 10),
        'is_unbalance':True
      }

      # gbm = lgb.train(param, dtrain, num_boost_round=int(r.optuna_rounds))
      gbm = lgb.LGBMClassifier(**param)
      gbm.fit(train_x, train_y)
      preds = gbm.predict(test_x)
      # pred_labels = np.rint(preds)
      # accuracy = sklearn.metrics.accuracy_score(test_y, pred_labels)
      accuracy = sklearn.metrics.accuracy_score(test_y, preds)
      return accuracy

  study = optuna.create_study(direction='maximize')
  optuna.logging.set_verbosity(optuna.logging.CRITICAL)
  study.optimize(objective, n_trials=int(150), show_progress_bar=True)
  print('Number of finished trials:', len(study.trials))
  print('Best trial:', study.best_trial.params)

  best_params_intensity = study.best_params
  best_accuracy_intensity = study.best_value
  lgbm_model = lgb.LGBMClassifier(**best_params_intensity, n_estimators=5000)
  lgbm_model.fit(train_x, train_y)

  predictions_intensity = lgbm_model.predict(test_x)
  accuracy_intensity = accuracy_score(test_y, predictions_intensity)
  kappa_intensity = cohen_kappa_score(test_y, predictions_intensity)
  # f1 = f1_score(test_y, predictions, pos_label=0)
  conf_matrix_intensity = confusion_matrix(test_y, predictions_intensity)
  print("Accuracy optuna :", accuracy_intensity)
  print("Kappa Score:", kappa_intensity)
  # print("F1 Score:", f1)
  print("Confusion Matrix:\n", conf_matrix_intensity)

  # lgbm_dt = lgb.plot_tree(lgbm_model, figsize=(200, 200), show_info=['split_gain'])

  lgbm_model.booster_.save_model("lgbm_model2.txt")


# if choice["FLAML_params"]:
#   settings = {
#     "time_budget" : r.flaml_budget,
#     "estimator_list": ['lgbm'],
#     "task": 'classification',
#     "log_file_name": 'flaml.log',
#     "seed": 123,
#     "verbose": 0
#   }
#
#   automl = AutoML()
#   automl.fit(X_train = pd.DataFrame(r.lgbm_train_data["values"]), y_train =np.array(r.lgbm_train_data["label"]), **settings)
#   study = automl.best_config
