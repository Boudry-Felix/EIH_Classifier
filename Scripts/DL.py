import os
import setuptools
import pandas as pd
import numpy as np
from tensorflow.random import set_seed
from sklearn.model_selection import train_test_split
from pytorch_tabular import TabularModel
from pytorch_tabular.models import (NodeConfig,
                                    GANDALFConfig,
                                    DANetConfig)
from pytorch_tabular.config import (DataConfig, 
                                    OptimizerConfig,
                                    TrainerConfig,
                                    ExperimentConfig)
from pytorch_tabular import utils

set_seed(int(r.project_seed))
utils.suppress_lightning_logs(log_level=None)

# Data import
train_x = pd.DataFrame(r.ml_train_data["values"])
train_y = np.array(r.ml_train_data["label"]).ravel()
test_x = pd.DataFrame(r.ml_test_data["values"])
test_y = np.array(r.ml_test_data["label"]).ravel()
train, val = train_test_split(pd.DataFrame(r.dl_train), 
                              random_state = int(r.project_seed))

# PyTorch tabular model
## General model configuration
cat_col_names = ['sex', 'sport']

data_config = DataConfig(
  target = ['eih'],
  categorical_cols = cat_col_names,
  num_workers = 24
)

trainer_config = TrainerConfig(
  auto_lr_find = True,
  batch_size = 128,
  max_epochs = 10,
  accelerator = "cpu",
  early_stopping = None,
  checkpoints_path = '.pytabular_models'
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

model_path = "Models/" + "/NODE"
node_model.save_model(model_path)

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

model_path = "Models/" + "/GANDALF"
gandalf_model.save_model(model_path)

## DANET model
danet_config = DANetConfig(
  task = 'classification',
  learning_rate = 1e-5,
  seed = int(r.project_seed)
)

danet_model = TabularModel(
  data_config = data_config,
  model_config = danet_config,
  optimizer_config = optimizer_config,
  trainer_config = trainer_config,
)

danet_model.fit(train = train, validation = val)
eval_result = danet_model.evaluate(val)

danet_pred = danet_model.predict(test_x)
danet_pred_y = danet_pred['prediction']

model_path = "Models/" + "/DANET"
danet_model.save_model(model_path)
