import pandas as pd
import tensorflow as tf
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.utils import to_categorical
from keras.callbacks import EarlyStopping

import tensorflow as tf
from tensorflow import keras
import keras_tuner as kt

tf.random.set_seed(123)

train_df = pd.DataFrame(r.analysis_data)
train_x = train_df.drop(columns = ["eih"])
train_y = to_categorical(train_df.eih - 1)

# Standalone model
model = Sequential()

n_cols = train_x.shape[1]
early_stopping_monitor = EarlyStopping(patience=5)

#add layers to model
model.add(Dense(52, activation='relu', input_shape=(n_cols,)))
model.add(Dense(150, activation='relu'))
model.add(Dense(150, activation='relu'))
model.add(Dense(2, activation='softmax'))

#compile model using accuracy to measure model performance
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

#train model
model.fit(train_x,train_y, epochs=30, validation_split=0.2, callbacks=[early_stopping_monitor])

# HParams tuning
def model_builder(hp):
  model = keras.Sequential()
  # Tune the number of units in the first Dense layer
  model.add(keras.layers.Dense(52, activation='relu', input_shape=(n_cols,)))
  hp_units = hp.Int('units', min_value=32, max_value=512, step=32)
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
