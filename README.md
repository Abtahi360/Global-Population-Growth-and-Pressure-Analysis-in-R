## Project Overview

This project analyzes global population data collected from Worldometer and uses R programming to build a complete data science workflow. It has two main scripts:

* [`Scraping.R`](./Scraping.R) for data collection and CSV creation
* [`ds_project.R`](./ds_project.R) for data cleaning, analysis, visualization, and machine learning

The dataset contains **234 countries/territories** with variables such as population, yearly change, density, fertility rate, median age, urban population percentage, and world share.

## Workflow Summary

### 1. Data Scraping

[`Scraping.R`](./Scraping.R) extracts the country-level population table from Worldometer and saves it as `worldometers_population.csv`.

### 2. Data Cleaning and Preprocessing

[`ds_project.R`](./ds_project.R) performs:

* missing value handling
* duplicate removal
* numeric conversion
* outlier handling using winsorization
* feature engineering
* scaling and preparation for modeling

### 3. Exploratory Data Analysis

The script creates visualizations for:

* feature distributions
* boxplots for outlier detection
* correlation heatmap
* scatter plots
* top 20 fastest-growing countries

### 4. Feature Engineering

New analytical features are created, including:

* `Dependency_Pressure_Index`
* `Urban_Stress_Index`
* `Pop_Density_Log`
* `Population_Log`

### 5. Population Pressure Index

A custom **CPPS** score is calculated using a weighted combination of demographic indicators. Based on this score, countries are labeled into:

* **Low**
* **Moderate**
* **High**

### 6. Machine Learning Models

The project builds and compares models for:

#### Regression

To predict `Yearly_Change`:

* Linear Regression
* Decision Tree Regressor
* Random Forest Regressor
* XGBoost Regressor, if available

#### Classification

To predict `Population_Pressure`:

* Logistic Regression
* Decision Tree Classifier
* Random Forest Classifier
* SVM with RBF kernel
* XGBoost Classifier, if available

### 7. Model Evaluation

The models are evaluated using:

* **Regression:** MAE, MSE, RMSE, R², CV-R²
* **Classification:** Accuracy, Precision, Recall, F1 Score, CV Accuracy

## Outputs

All generated figures and final cleaned data are saved inside the [`project_outputs_R/`](./project_outputs_R/) folder.

The project produces:

* **12 visualizations**
* **cleaned_dataset_with_labels.csv**
* model comparison tables in the console output

## Main Objectives

This project has two main objectives:

1. **Predict annual population growth** using demographic and spatial features.
2. **Classify population pressure** into Low, Moderate, and High categories.

## Tools and Libraries Used

The project uses the following R packages:

* `tidyverse`
* `caret`
* `randomForest`
* `rpart`
* `e1071`
* `reshape2`
* `gridExtra`
* `scales`
* `nnet`
* `xgboost` (optional)

## How to Run

1. Open `Scraping.R` and run it to generate `worldometers_population.csv`.
2. Open `ds_project.R` and run it for cleaning, visualization, and modeling.
3. Check the `project_outputs_R/` folder for plots and final output files.

## Notes

* The project is fully reproducible in R.
* Missing values and outliers are handled carefully.
* XGBoost is included with a safe fallback, so the project can still run even if the package is unavailable.
