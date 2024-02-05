# EIH prediction

## Purpose

This project aims to create a model that would be able to detect if an athlete
has Exercise Induced Hypoxaemia (EIH) without using it's direct indicator which
is the saturation.

## Usage

Summary or time-series files that are used for this project have to be in the "Data" folder at the root of the project.

If you don't already have a summary file containing features about each one of
your subjects, you can create one using the 'summary_gen()' function that will
put this file in the "Data" folder.

This function only works if your files and projects are structured in the
following way:

```
raw_data_folder
|
|- Informations_about_studies.csv
|- Study_1_folder
|  |- Informations_about_subjects.csv
|  |- Informations_about_tests.csv
|  |- Gas_exchange_data_folder
|      |- file_1.xlsx
|      |- file_2.xlsx
|      |- file_X.xslx
|- Study_2_folder
|  |- Informations_about_subjects.csv
|  |- Informations_about_tests.csv
|  |- Gas_exchange_data_folder_1
|  |   |- file_1.xlsx
|  |   |- file_2.xlsx
|  |   |- file_X.xslx
|  |- Gas_exchange_data_folder_1
|      |- file_1.xlsx
|      |- file_2.xlsx
|      |- file_X.xslx
etc.
```

Once the summary file(s) are created you can knit the document with the desired
parameters. If you knit with options you can specify the summary files to use.

## Analysis

The project aimed to compare various models and methods to achieve the 
classification, including clustering methods (k-means and hierearchical), 
gradient boosted methods (LightGBM, XGBoost) and Neural Networks (pytabular, 
keras). Each of those  approaches has it's own script and some of the methods 
need parameters to be indicated (hardcoded for this project) in order to have 
pertinent analysis.

## Parameters

- Cluster numbers
- Cluster methods
- LGBM rounds
- Optuna trial
- Optuna rounds
- FLAML time budget

(Some may have been forgotten, those are the most important ones.)

## Data

The data used on our side and for the research paper were given by:

-   Durand Fabienne
-   Raberin Antoine
-   Gaston Anne-Fleur
-   Mucci Patrick

Thank you!

## Contact

For further discussions or informations you can contact me at
[felix.boudry\@univ-perp.fr](mailto:felix.boudry@univ-perp.fr).
