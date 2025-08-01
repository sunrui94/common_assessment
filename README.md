# 📊 Common Assessment (CA) Exploratory Data Analysis

This project explores anonymized Common Assessment (CA) data collected from a public services context to evaluate how effectively the CA tool captures client intake information.

## 🔍 Project Overview

The analysis focuses on:
- Understanding patterns in the use of “Prefer not to answer” (PNTA) responses
- Identifying trends across socio-demographic groups
- Extracting themes from free-text responses in “Other (specify)” fields using text mining techniques

The dataset includes client intake records over an 18-month period.

## 📈 Key Insights

- **PNTA Usage**: While 29% of clients opted out of at least one question, most only did so for a single item. Sensitive questions (e.g. wage, race, LGBTQ+) showed the highest PNTA rates.
- **Demographic Trends**: Clients from equity-deserving groups were more often associated with higher-risk segments, based on socio-demographic and education indicators.
- **Text Mining**: Unstructured text in “Other” responses revealed common client themes such as unstable job conditions, financial barriers, and non-traditional education or residency status.

## 🛠️ Tools and Technologies

- **Language**: R
- **Packages**: `tidyverse`, `lubridate`, `tidytext`, `ggplot2`, `ggraph`, `janitor`, `DT`, `kableExtra`, `textdata`, `tm`, `igraph`
- **Techniques**:  
  - Exploratory data analysis (EDA)  
  - Text preprocessing and sentiment-based phrase replacement  
  - Unigram, bigram, and trigram frequency analysis  
  - Network graph visualization of common phrases  
  - Demographic breakdown by segmentation streams

