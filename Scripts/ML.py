import optuna
import lightgbm as lgb
import xgboost as xgb
import numpy as np
import pandas as pd
import sklearn.metrics as sm

# General Optuna settings
optuna.logging.set_verbosity(optuna.logging.CRITICAL)
sampler = optuna.samplers.TPESampler(seed=int(r.project_seed))

# Data import
train_x = pd.DataFrame(r.ml_train_data["values"])
train_y = np.array(r.ml_train_data["label"]).ravel()
test_x = pd.DataFrame(r.ml_test_data["values"])
test_y = np.array(r.ml_test_data["label"]).ravel()
dtrain = lgb.Dataset(train_x, label=train_y)

# LGBM model
## Optuna search
def lgbm_tune(trial):
    params = {
      'boosting_type': 'dart',
      'objective': 'binary',
      'metric': 'binary_logloss',
      'verbosity': -1,
      'learning_rate': trial.suggest_float('learning_rate',0.03, 1),
      'lambda_l1': trial.suggest_float('lambda_l1', 2, 5.0),
      'lambda_l2': trial.suggest_float('lambda_l2', 0.05, 3.0),
      'num_leaves': trial.suggest_int('num_leaves', 8, 80),
      'max_depth': trial.suggest_int('max_depth', 2, 200),
      'feature_fraction': trial.suggest_float('feature_fraction', 0.06, 1.0),
      'bagging_fraction': trial.suggest_float('bagging_fraction', 0.05, 1.0),
      'bagging_freq': trial.suggest_int('bagging_freq', 2, 6),
      'drop_rate': trial.suggest_float('drop_rate', 0.001, 0.8),
      'reg_alpha': trial.suggest_float('reg_alpha', 4, 100),
      'reg_lambda': trial.suggest_float('reg_lambda', 4, 100),
      'is_unbalance':True
    }

    optuna_model = lgb.LGBMClassifier(**params, n_estimators=int(r.lgbm_rounds))
    optuna_model.fit(train_x, train_y)
    optuna_preds = optuna_model.predict(test_x)
    optuna_pred_y = np.rint(optuna_preds)
    optuna_accuracy = sm.accuracy_score(test_y, optuna_pred_y)
    return optuna_accuracy

study_lgbm = optuna.create_study(direction='maximize', sampler = sampler)
study_lgbm.optimize(lgbm_tune, n_trials=int(r.optuna_trials), show_progress_bar=True, n_jobs=1)
  
## Final LGBM model
my_params = {
  'boosting_type': 'dart',
  'objective': 'binary',
  'metric': 'binary_logloss',
  'is_unbalance':True
}
optuna_lgbm_best_accuracy = study_lgbm.best_value
lgbm_best_params = study_lgbm.best_params
for items in my_params:
  lgbm_best_params[items] = my_params[items]
print(lgbm_best_params)
lgbm_model = lgb.LGBMClassifier(**lgbm_best_params, n_estimators=int(r.lgbm_rounds), keep_training_booster = True)
lgbm_model.fit(train_x, train_y)
lgbm_params = lgbm_model.get_params()
lgbm_pred_y = lgbm_model.predict(test_x)

model_path = "Models/" + r.env_name + "/LGBM.txt"
lgbm_model.booster_.save_model(model_path)

# XGBoost model
## Optuna search
def xgboost_tune(trial):
    params = {
      'objective': 'binary:logistic',
      'booster': 'dart',
      'eval_metric': 'mlogloss',
      'max_depth': trial.suggest_int('max_depth', 1, 20),
      'learning_rate': trial.suggest_float('learning_rate', 0.01, 1.0),
      'min_child_weight': trial.suggest_int('min_child_weight', 1, 10),
      'gamma': trial.suggest_float('gamma', 1e-8, 1.0),
      'subsample': trial.suggest_float('subsample', 0.01, 1.0),
      'colsample_bytree': trial.suggest_float('colsample_bytree', 0.01, 1.0),
      'reg_alpha': trial.suggest_float('reg_alpha', 1e-8, 1.0),
      'reg_lambda': trial.suggest_float('reg_lambda', 1e-8, 1.0),
      'eta': trial.suggest_float("eta", 1e-8, 1.0),
      'grow_policy': trial.suggest_categorical('grow_policy', ["depthwise", "lossguide"]),
      'sample_type': trial.suggest_categorical('sample_type', ["uniform", "weighted"]),
      'normalize_type': trial.suggest_categorical('normalize_type', ["tree", "forest"]),
      'rate_drop': trial.suggest_float('rate_drop', 1e-8, 1.0),
      'skip_drop': trial.suggest_float('skip_drop', 1e-8, 1.0)
    }

    optuna_model = xgb.XGBClassifier(**params, n_estimators=int(r.xgboost_rounds))
    optuna_model.fit(train_x, train_y)
    optuna_pred_y = optuna_model.predict(test_x)
    optuna_accuracy = sm.accuracy_score(test_y, optuna_pred_y)
    return optuna_accuracy

study_xgboost = optuna.create_study(direction='maximize', sampler = sampler)
study_xgboost.optimize(xgboost_tune, n_trials=int(r.optuna_trials), show_progress_bar=True)

## Final XGBoost model
my_params = {
  'objective': 'binary:logistic',
  'eval_metric': 'mlogloss',
  'booster': 'dart'
}
optuna_xgb_best_accuracy = study_xgboost.best_value
xgboost_best_params = study_xgboost.best_params
for items in my_params:
  xgboost_best_params[items] = my_params[items]
print(xgboost_best_params)
xgboost_model = xgb.XGBClassifier(**xgboost_best_params, n_estimators=int(r.xgboost_rounds))
xgboost_model.fit(train_x, train_y)
xgboost_params = xgboost_model.get_params()
feature_names = xgboost_model.get_booster().feature_names
xgboost_pred_y = xgboost_model.predict(test_x)

model_path = "Models/" + r.env_name + "/XGBoost.json"
xgboost_model.save_model(model_path)
