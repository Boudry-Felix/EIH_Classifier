# EIH prediction

## Purpose

This project aims to create a model that would be able to detect if an athlete
has Exercise Induced Hypoxaemia (EIH) without using it's direct indicator which
is the saturation.

## Usage

Summary or time-series files that are used for this project have to be in the "Data" folder at the root of the project.

## Analysis

The project aimed to compare various models and methods to achieve the 
classification, including clustering methods (k-means and hierearchical), 
gradient boosted methods (LightGBM, XGBoost) and Neural Networks (pytabular, 
keras). Each of those  approaches has it's own script and some of the methods 
need parameters to be indicated (hardcoded for this project) in order to have 
pertinent analysis.

## Parameters

- Cluster numbers
- LGBM rounds
- Optuna trial
- Optuna rounds

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
