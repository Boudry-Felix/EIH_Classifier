import os
import pandas as pd
import numpy as np
import random

import tensorflow as tf
import keras_tuner as kt
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.utils import to_categorical
from keras.callbacks import EarlyStopping
from tensorflow import keras

from sklearn.datasets import make_classification
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, f1_score
from pytorch_tabular import TabularModel
from pytorch_tabular.models import CategoryEmbeddingModelConfig, NodeConfig
from pytorch_tabular.config import DataConfig, OptimizerConfig, TrainerConfig, ExperimentConfig
from pytorch_tabular.models.common.heads import LinearHeadConfig

# Data import
tf.random.set_seed(int(r.project_seed))

train_df = pd.DataFrame(r.analysis_data)
train_x = train_df.drop(columns = ["eih"])
train_y = to_categorical(train_df.eih - 1)
train, val = train_test_split(pd.DataFrame(r.ml_train), random_state = int(r.project_seed))
test = pd.DataFrame(r.ml_test)

# Keras model
# HParams tuning
n_cols = train_x.shape[1]
def model_builder(hp):
  model = keras.Sequential()
  # Tune the number of units in the first Dense layer
  model.add(keras.layers.Dense(n_cols, activation='relu', input_shape=(n_cols,)))
  hp_units = hp.Int('units', min_value=2, max_value=n_cols, step=1)
  model.add(keras.layers.Dense(units=hp_units, activation='relu'))
  model.add(keras.layers.Dense(2))

  # Tune the learning rate for the optimizer
  hp_learning_rate = hp.Choice('learning_rate', values=[1e-2, 1e-3, 1e-4])

  model.compile(optimizer=keras.optimizers.Adam(learning_rate=hp_learning_rate),
                loss='categorical_crossentropy',
                metrics=['accuracy'])

  return model

tuner = kt.Hyperband(model_builder,
                     objective='val_accuracy',
                     max_epochs=100,
                     factor=2,
                     project_name='keras_models')

stop_early = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)

tuner.search(train_x, train_y, epochs=50, validation_split=0.2, callbacks=[stop_early])

# Get the optimal hyperparameters
best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]

# PyTorch tabular model (NODE)
cat_col_names = ['sex', 'activity', 'sport']

data_config = DataConfig(
  target=['eih'],
  categorical_cols=cat_col_names,
  num_workers=24
)

trainer_config = TrainerConfig(
  auto_lr_find=True,
  batch_size=1024,
  max_epochs=100,
  accelerator="auto", # can be 'cpu','gpu', 'tpu', or 'ipu'
  early_stopping= 'valid_loss',
  early_stopping_patience=5,
  checkpoints_path='node_models'
)

optimizer_config = OptimizerConfig()

model_config = NodeConfig(
  num_layers=2,
  num_trees = n_cols,
  depth = 5,
  task='classification',
  learning_rate = 1e-3,
  seed=int(r.project_seed)
)

tabular_model = TabularModel(
  data_config=data_config,
  model_config=model_config,
  optimizer_config=optimizer_config,
  trainer_config=trainer_config,
)

tabular_model.fit(train=train, validation=val)
eval_result = tabular_model.evaluate(val)

test_y = r.ml_test['eih']
test_y = test_y.to_numpy()
test_y = test_y.tolist()

node_pred = tabular_model.predict(test)
node_pred_y = node_pred['prediction']

node_confusion = sm.confusion_matrix(test_y, node_pred_y)

node_accuracy = accuracy_score(node_pred['eih'], node_pred['prediction'])
node_kappa = sm.cohen_kappa_score(test_y, node_pred_y)
node_f1 = sm.f1_score(test_y, node_pred_y, pos_label=1)
node_auc_roc = sm.roc_auc_score(test_y, node_pred_y)
node_precision = sm.precision_score(test_y, node_pred_y)
node_recall = sm.recall_score(test_y, node_pred_y)
