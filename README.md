# EIH Modeling

## Purpose and explanations

This project aims to create a mathematical model explaining the "Exercise Induced Hypoxia" (EIH) phenomenon. To do so we use data containing stress tests data and the blood oxygen saturation of each subject during this test. After importing those data we compute a summary for each subject's data. Once this is done we use two approaches:

1.  Firstly a descriptive one

2.  Then a predictive one

### Descriptive analysis

This first approach is based on several clustering methods. The purpose is to determine if it is possible to regroup subjects using only gas exchange values in order to categorize them as EIH athletes or NEIH (not EIH) athletes. For more details about those methods, visit [scikit-learn](https://scikit-learn.org/stable/modules/clustering.html#clustering).

### Predictive analysis

This second approach is base on machine learning algorithms (like GBM). The objective is to be able to discriminate EIH and NEIH athletes using only the gas exchange data. For more details about this methods, visit [scikit-learn](https://scikit-learn.org/stable/supervised_learning.html).

## Data

The data used were given by:

-   Durand Fabienne
-   Raberin Antoine
-   Gaston Anne-Fleur
-   Mucci Patrick

## How to use the code

### Project structure

First a few folder are mandatory, if they don't exist they will be created and are used to store results and parameters:

-   Data : Contains all data used in this project, you can put your own data inside following the provided structure.
-   Environment : Stores .RData files with the environments of the previous scripts
-   Images : Stores the computed images
-   Output : Stores .rds files for specific r objects that are used in other scripts and shall be saved.
-   Params : A folder in which you can put computed parameters for the GBM analysis.
-   Scripts : Contains the different scripts of this project.

### Data structure

You can put your own data in the "Data"" folder but you have to follow a structure. In  the root folder (Data) you can put a CSV file with the informations about the different studies the data come from. You can also add folders for each study. In those folder you can put two CSV files, the first on with informations about the subjects and the second one with informations about the tests. In the study folder you can add a folder with your test data (XLSX files not CSV!) with the structure you want.

### Scripts

Each script has a specific purpose described in it's "Informations" section and should run smoothly if you respected the data structure.
You only have to configure the lightGBM parameter by putting the results of the optuna script (that can be found in "Output") into the "Params" folder. If this folder is empty or the parameters are set to NULL in the script and the optuna script will be executed to compute new parameters.

## Contact

For further discussions or informations you can contact me at [felix.boudry\@univ-perp.fr](mailto:felix.boudry@univ-perp.fr).
