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
train = pd.DataFrame(r.ml_train)
val = pd.DataFrame(r.ml_test)

# Keras model
# HParams tuning
n_cols = train_x.shape[1]
def model_builder(hp):
  model = keras.Sequential()
  # Tune the number of units in the first Dense layer
  model.add(keras.layers.Dense(n_cols, activation='relu', input_shape=(n_cols,)))
  hp_units = hp.Int('units', min_value=2, max_value=52, step=1)
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
                     project_name='khyperband')

stop_early = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)

tuner.search(train_x, train_y, epochs=50, validation_split=0.2, callbacks=[stop_early])

# Get the optimal hyperparameters
best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]

print(f"""
The hyperparameter search is complete. The optimal number of units in the first densely-connected
layer is {best_hps.get('units')} and the optimal learning rate for the optimizer
is {best_hps.get('learning_rate')}.
""")

# PyTorch tabular model (NODE)
cat_col_names = ['sex', 'eih']

data_config = DataConfig(
    target=['eih'], #target should always be a list. Multi-targets are only supported for regression. Multi-Task Classification is not implemented
    categorical_cols=cat_col_names,
)
trainer_config = TrainerConfig(
    auto_lr_find=True, # Runs the LRFinder to automatically derive a learning rate
    batch_size=128,
    max_epochs=1000,
    accelerator="cpu", # can be 'cpu','gpu', 'tpu', or 'ipu' 
)
optimizer_config = OptimizerConfig()

head_config = LinearHeadConfig(
    layers="", # No additional layer in head, just a mapping layer to output_dim
    dropout=0.1,
    initialization="kaiming"
).__dict__ # Convert to dict to pass to the model config (OmegaConf doesn't accept objects)

model_config = NodeConfig(
    num_trees = 150,
    depth = 8,
    task='classification',
    learning_rate = 1e-3,
)


tabular_model = TabularModel(
    data_config=data_config,
    model_config=model_config,
    optimizer_config=optimizer_config,
    trainer_config=trainer_config,
)

tabular_model.fit(train=train, validation=val)

# result = tabular_model.evaluate(val)
