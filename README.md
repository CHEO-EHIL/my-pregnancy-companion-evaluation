# My Pregnancy Companion — Mixed Method Clinical Acceptability and Consistency Evaluation (Pre-Deployment)

This repository contains the R scripts used for statistical and computational analyses in the pre-deployment mixed-methods clinician evaluation of **My Pregnancy Companion (MPC)**, a conversational AI system designed to provide personalized and proactive pregnancy education.

The evaluation examined clinical acceptability of single-turn responses, agreement between clinician raters, and response consistency under repeated identical inputs.

## Repository Contents

| Script | Description |
|---|---|
| `clinical_acceptability_sample_size.R` | Sample-size and power calculations for the clinical acceptability evaluation. |
| `clinical_acceptability_analysis.R` | Analysis of clinician ratings, including interrater agreement, clinical acceptability, confidence intervals, hypothesis testing, and sensitivity analyses. |
| `semantic_repeatability_sample_size.R` | Simulation-based sample-size and power analysis for semantic repeatability. |
| `semantic_repeatability_analysis.R` | Analysis of semantic repeatability and tool-call repeatability across repeated responses. |

## Requirements

Analyses were implemented in R using the following packages:

- `tidyverse`
- `pwr`
- `binom`
- `irr`
- `readxl`
- `ggplot2`
- `MKpower`

## Data Availability

Illustrative examples from the evaluation dataset are provided in the associated manuscript and supplementary materials. The complete evaluation dataset is not publicly available because it continues to be used for ongoing system testing and evaluation, and public release could compromise the independence of future assessments.

Consequently, scripts requiring evaluation data cannot be executed directly without access to the corresponding input files. The sample-size scripts can be run independently.

## Scope

This repository contains evaluation and analysis code only. It does not include the source code of the My Pregnancy Companion conversational AI system, which is proprietary.

## Publication

Under review.
