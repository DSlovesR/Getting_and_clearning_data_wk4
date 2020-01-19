# Getting and Cleaning data
Week 4 assignemnt

## Introduction

The objective of this exercise is to load data from internet link provided and prepare a dataset. Dataset is prepared from the zip file downloaded from UCI Machine meanring repository containing Smartphone - recognitio of human activities and postural transitions data. 
Final dataset contians the mean and std of the traing and test dataset features.

## Data

Downloaded .zip file has following data files/Folders:
1- test - Folder contains three files and one sub folder
	- Subject_test.txt - Each test row identifies the subject who performed the activity. Its range is from 1 to 30. 
	- X_test.txt - Test set containing 561 variables.
	- y_test.txt - Test label
	- Inertial Signals (folder) not in scope of the assignment
2- train - Folder contains three files and one sub folder
	- Subject_train.txt - Each Training row identifies the subject who performed the activity. Its range is from 1 to 30. 
	- X_train.txt - Training data containing 561 variables.
	- y_train.txt - training labels.
	- Inertial Signals (folder) not in scope of the assignment

3- Activity_labels.txt - Contains 6 actvities with the description

4- features.txt - List of all features/varaibles

5- features_info.txt - detail of variables

6- ReadME.txt

## Source file(s)

run_analysis.R - R script contians the set of instruction to download the zip file and load trainig, test and other txt files to prepare tidy dataset.

Codebook.md  - Codebook file contians detail instruction on how data prepared and transformed.
