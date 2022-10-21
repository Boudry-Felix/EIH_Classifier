# EIH Modeling

## Purpose and explanations

This project aims to create a mathematical model explaining the "Exercise Induced Hypoxia" (EIH) phenomenon. To do so we use data containing stress tests data and the oxygen saturation of each subject during this test. After importing those data we compute a summary for each subject's data. Once this is done we use two approaches:

1.  Firstly a descriptive one

2.  Then a predictive one

### Descriptive analysis

This first approach is based on several clustering methods. The purpose is to determine if it is possible to regroup subjects using only gas exchange values in order to categorize them as EIH athletes or NEIH athletes. For more details about this methods, visit [scikit-learn](https://scikit-learn.org/stable/modules/clustering.html#clustering).

### Predictive analysis

This second approach is base on machine learning algorithms (like GBM). The objective is to be able to discriminate EIH and NEIH athletes using only the gas exchange data. For more details about this methods, visit [scikit-learn](https://scikit-learn.org/stable/supervised_learning.html).

## Data

The data used were given by:

-   Durand Fabienne
-   Raberin Antoine
-   Gaston Anne-Fleur
-   Mucci Patrick

## How to use the code

## Contact

For further discussions you can contact me at [felix.boudry\@univ-perp.fr](mailto:felix.boudry@etudiant.univ-perp.fr){.email}.
