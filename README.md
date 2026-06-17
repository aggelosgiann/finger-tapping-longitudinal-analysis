# Finger Tapping Parameters Predict Longitudinal Cognitive Trajectories

## Overview

This repository contains the R code used for statistical data analyses and figure generation in the study:

**"Finger Tapping Parameters Predict Longitudinal Cognitive Trajectories"**

The study investigates whether kinematic characteristics predicts future cognitive outcomes in older adults over a two-year follow-up period.

The repository is provided to promote transparency, reproducibility, and open science practices.

---

## Manuscript

Preprint / publication:

[INSERT MANUSCRIPT LINK HERE]

---

## Data Availability

The participant-level dataset is **not included** in this repository.

The data contain sensitive human-subject information and cannot be publicly shared due to ethical and privacy restrictions.

Researchers interested in accessing the data should contact the corresponding author and obtain the necessary approvals.

---

## Analysis Workflow

The analysis pipeline includes:

1. Data import and restructuring
2. Data cleaning and variable derivation
3. Gender-specific centering and scaling of kinematic variables
4. Ordinary Least Squares (OLS) regression screening
5. A Linear Mixed Model (LMM) for longitudinal cognitive trajectories
6. Generation of figures
7. Model diagnostics and visualization

---

## Statistical Methods

### OLS Regression

Baseline kinematic variables were evaluated as predictors of Year 3 MoCA performance while adjusting for:

* Baseline MoCA score
* Gender

The mean ISI of the index finger was selected for longitudinal modeling, as it was statistically significant and offered greater interpretability than the more complex standard deviation (SD) measures of ISI derived from the middle and index fingers.

### Linear Mixed Model

Longitudinal changes in cognitive performance were examined using a mixed-effects model with:

* Random intercepts for participants
* Fixed effects for:

  * Time
  * Age
  * Education
  * Gender
  * Mean ISI of the index finger
  * Time × Mean ISI of the index finger

---

## Software

Analyses were conducted in R.

### Main Packages

* dplyr
* tidyr
* broom
* lme4
* lmerTest
* sjPlot
* ggplot2
* patchwork
* ggeffects

---

## Repository Structure

#### R
Contains all statistical analysis scripts.

#### figures
Contains figures generated from the analysis pipeline.

#### manuscript
Contains citation information and manuscript-related documentation.

---

## Reproducibility Notes

The code assumes access to the original study dataset and therefore may require adaptation if applied to external datasets.

Variable names reflect the naming conventions used in the original project.

No raw participant data are distributed through this repository.

---

## Citation

If you use or adapt code from this repository, please cite:

[INSERT FULL CITATION AFTER ACCEPTANCE]

---

## Ethical Statement

All procedures involving human participants were approved by the relevant ethics committees and conducted in accordance with the Declaration of Helsinki.

To protect participant privacy and comply with ethical requirements, no identifiable or participant-level data are publicly released.
