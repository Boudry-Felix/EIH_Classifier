import optuna
import lightgbm as lgb
import xgboost as xgb
import numpy as np
import pandas as pd
import sklearn.metrics as sm

sampler = optuna.samplers.TPESampler(seed=int(r.project_seed))

train_x = pd.DataFrame(r.ml_train_data["values"])
train_y = np.array(r.ml_train_data["label"])
train_y = train_y.ravel()
test_x = pd.DataFrame(r.ml_test_data["values"])
test_y = np.array(r.ml_test_data["label"])
test_y = test_y.ravel()
dtrain = lgb.Dataset(train_x, label=train_y)

# LGBM model
def lgbm_tune(trial):
    params = {
      'boosting_type': 'dart',
      'objective': 'binary',
      'metric': 'binary_logloss',
      'verbosity': -1,
      'learning_rate': trial.suggest_float('learning_rate',0.05, 1),
      'lambda_l1': trial.suggest_float('lambda_l1', 1e-3, 5.0),
      'lambda_l2': trial.suggest_float('lambda_l2', 1e-3, 5.0),
      'num_leaves': trial.suggest_int('num_leaves', 3, 700),
      'max_depth': trial.suggest_int('max_depth', 5, 80),
      'feature_fraction': trial.suggest_float('feature_fraction', 0.6, 1.0),
      'bagging_fraction': trial.suggest_float('bagging_fraction', 0.5, 1.0),
      'bagging_freq': trial.suggest_int('bagging_freq', 2, 6),
      'drop_rate': trial.suggest_float('drop_rate', 0.01, 0.7),
      'reg_alpha': trial.suggest_float('reg_alpha', 0.1, 10),
      'reg_lambda': trial.suggest_float('reg_lambda', 0.1, 10),
      'is_unbalance':True
    }

    lgbm = lgb.train(params, dtrain, num_boost_round=int(r.lgbm_rounds))
    preds = lgbm.predict(test_x)
    pred_labels = np.rint(preds)
    accuracy = sm.accuracy_score(test_y, pred_labels)
    return accuracy

study_lgbm = optuna.create_study(direction='maximize', sampler = sampler)
optuna.logging.set_verbosity(optuna.logging.CRITICAL)
study_lgbm.optimize(lgbm_tune, n_trials=int(r.optuna_trials), show_progress_bar=True)

lgbm_best_params = study_lgbm.best_params
lgbm_best_accuracy = study_lgbm.best_value
lgbm_model = lgb.LGBMClassifier(**lgbm_best_params, n_estimators=int(r.lgbm_rounds))
lgbm_model.fit(train_x, train_y)

lgbm_pred_y = lgbm_model.predict(test_x)
lgbm_accuracy = sm.accuracy_score(test_y, lgbm_pred_y)
lgbm_kappa = sm.cohen_kappa_score(test_y, lgbm_pred_y)
lgbm_f1 = sm.f1_score(test_y, lgbm_pred_y, pos_label=1)
lgbm_confusion = sm.confusion_matrix(test_y, lgbm_pred_y)
lgbm_auc_roc = sm.roc_auc_score(test_y, lgbm_pred_y)
lgbm_precision = sm.precision_score(test_y, lgbm_pred_y)
lgbm_recall = sm.recall_score(test_y, lgbm_pred_y)

# lgbm_precision_recall = sm.precision_recall_curve(test_y, lgbm_pred_y)
# lgbm_roc = sm.roc_curve(test_y, lgbm_pred_y)

lgbm_model.booster_.save_model("lgbm_model.txt")

# XGBoost model
def xgboost_tune(trial):
    params = {
      'max_depth': trial.suggest_int('max_depth', 1, 9),
      'learning_rate': trial.suggest_float('learning_rate', 0.01, 1.0),
      'n_estimators': trial.suggest_int('n_estimators', 50, 500),
      'min_child_weight': trial.suggest_int('min_child_weight', 1, 10),
      'gamma': trial.suggest_float('gamma', 1e-8, 1.0),
      'subsample': trial.suggest_float('subsample', 0.01, 1.0),
      'colsample_bytree': trial.suggest_float('colsample_bytree', 0.01, 1.0),
      'reg_alpha': trial.suggest_float('reg_alpha', 1e-8, 1.0),
      'reg_lambda': trial.suggest_float('reg_lambda', 1e-8, 1.0),
      'eval_metric': 'mlogloss'
    }
    
    optuna_model = xgb.XGBClassifier(**params)
    optuna_model.fit(train_x, train_y)
    pred_y = optuna_model.predict(test_x)
    accuracy = sm.accuracy_score(test_y, pred_y)
    return accuracy

study_xgboost = optuna.create_study(direction='maximize', sampler = sampler)
optuna.logging.set_verbosity(optuna.logging.CRITICAL)
study_xgboost.optimize(xgboost_tune, n_trials=int(r.optuna_trials), show_progress_bar=True)

xgboost_best_params = study_xgboost.best_params
xgboost_best_accuracy = study_xgboost.best_value
xgboost_model = xgb.XGBClassifier(eval_metric="mlogloss", n_estimator=int(r.xgboost_rounds))
xgboost_model.fit(train_x, train_y)

feature_names = xgboost_model.get_booster().feature_names

xgboost_pred_y = xgboost_model.predict(test_x)
xgboost_accuracy = sm.accuracy_score(test_y, xgboost_pred_y)
xgboost_kappa = sm.cohen_kappa_score(test_y, xgboost_pred_y)
xgboost_f1 = sm.f1_score(test_y, xgboost_pred_y, pos_label=1)
xgboost_confusion = sm.confusion_matrix(test_y, xgboost_pred_y)
xgboost_auc_roc = sm.roc_auc_score(test_y, xgboost_pred_y)
xgboost_precision = sm.precision_score(test_y, xgboost_pred_y)
xgboost_recall = sm.recall_score(test_y, xgboost_pred_y)

xgboost_model.save_model("xgboost_model.txt")
