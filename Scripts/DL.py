import os
import pandas as pd
import numpy as np

import tensorflow as tf
import keras_tuner as kt
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.utils import to_categorical
from keras.callbacks import EarlyStopping

from sklearn.datasets import make_classification
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, f1_score
from pytorch_tabular import TabularModel
from pytorch_tabular.models import (CategoryEmbeddingModelConfig,
                                    NodeConfig,
                                    GANDALFConfig)
from pytorch_tabular.config import (DataConfig, 
                                    OptimizerConfig,
                                    TrainerConfig,
                                    ExperimentConfig)
from pytorch_tabular.models.common.heads import LinearHeadConfig

tf.random.set_seed(int(r.project_seed))

# Data import
train_df = pd.DataFrame(r.analysis_data)
train_x = train_df.drop(columns = ["eih"])
train_y = to_categorical(train_df.eih - 1)
train, val = train_test_split(pd.DataFrame(r.dl_train), 
                              random_state = int(r.project_seed))
test = pd.DataFrame(r.dl_test)
test.drop('eih', axis = 1)

# Keras model
## HParams tuning
n_cols = train_x.shape[1]
def model_builder(hp):
  model = keras.Sequential()
  # Tune the number of units in the first Dense layer
  model.add(keras.layers.Dense(n_cols,
                               activation = 'relu',
                               input_shape = (n_cols,)))
  hp_units = hp.Int('units', min_value = 2, max_value = n_cols, step = 1)
  model.add(keras.layers.Dense(units = hp_units, activation = 'relu'))
  model.add(keras.layers.Dense(2))

  # Tune the learning rate for the optimizer
  hp_learning_rate = hp.Choice('learning_rate', values = [1e-2, 1e-3, 1e-4])

  model.compile(
    optimizer = keras.optimizers.Adam(learning_rate = hp_learning_rate),
    loss = 'categorical_crossentropy',
    metrics = ['accuracy']
  )

  return model

tuner = kt.Hyperband(model_builder,
                     objective = 'val_accuracy',
                     max_epochs = 10,
                     factor = 2,
                     project_name = 'keras_models')

stop_early = tf.keras.callbacks.EarlyStopping(monitor = 'val_loss', 
                                              patience = 5)

tuner.search(train_x,
             train_y,
             epochs = 50,
             validation_split = 0.2,
             callbacks = [stop_early])

# Get the optimal hyperparameters
best_hps = tuner.get_best_hyperparameters(num_trials = 1)[0]
best_model = tuner.get_best_models()[0]

dense_pred = best_model.predict(test_x)

# PyTorch tabular model
## General model configuration
cat_col_names = ['sex', 'activity', 'sport']

data_config = DataConfig(
  target = ['eih'],
  categorical_cols = cat_col_names,
  num_workers = 24
)

trainer_config = TrainerConfig(
  auto_lr_find = True,
  batch_size = 128,
  max_epochs = 10,
  accelerator = "auto",
  early_stopping = None,
  checkpoints_path = 'pytabular_models'
)

optimizer_config = OptimizerConfig()

experiment_config = ExperimentConfig(exp_watch = 'all',
                                     log_target = "tensorboard",
                                     log_logits = True,
                                     project_name = "test_node")

## NODE model
node_config = NodeConfig(
  num_layers = 2,
  num_trees = 2,
  depth = 8,
  task = 'classification',
  learning_rate = 1e-5,
  seed = int(r.project_seed)
)

node_model = TabularModel(
  data_config = data_config,
  model_config = node_config,
  optimizer_config = optimizer_config,
  trainer_config = trainer_config,
)

node_model.fit(train = train, validation = val)
eval_result = node_model.evaluate(val)

node_pred = node_model.predict(test_x)
node_pred_y = node_pred['prediction']

## GANDALF model
gandalf_config = GANDALFConfig(
  task = 'classification',
  learning_rate = 1e-5,
  seed = int(r.project_seed)
)

gandalf_model = TabularModel(
  data_config = data_config,
  model_config = gandalf_config,
  optimizer_config = optimizer_config,
  trainer_config = trainer_config,
)

gandalf_model.fit(train = train, validation = val)
eval_result = gandalf_model.evaluate(val)

gandalf_pred = gandalf_model.predict(test_x)
gandalf_pred_y = gandalf_pred['prediction']
