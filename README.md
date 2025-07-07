# California PUMS Econometric Study

This repository contains R code and resources for an econometric analysis of the California Public Use Microdata Sample (PUMS) dataset. The primary focus is to study the determinants of wage and salary income using demographic, educational, and occupational variables.

## Contents
- `data_analysis.R`: Main R script for data cleaning, feature engineering, and regression modeling.
- `California State Data Set.csv`: (Not included) The raw data file required for analysis. Please obtain this from the appropriate source.

## Requirements
- R (version 4.0 or higher recommended)

### Required R Packages
- tidyverse
- ggplot2
- languageserver (recommended for code completion and editor support)

Install all required packages in R with:
```r
install.packages(c("tidyverse", "ggplot2", "languageserver"))
```

## Usage
1. Place the `California State Data Set.csv` file in the project directory.
2. Open `data_analysis.R` in RStudio or your preferred R environment.
3. Run the script. It will:
   - Load and inspect the data
   - Summarize missing values
   - Engineer features for analysis (e.g., citizenship, occupation, education)
   - Filter and clean the data
   - Fit multiple linear regression models to study wage determinants
   - Output model summaries and correlation matrices

## Analysis Overview
The script performs the following steps:
- **Data Cleaning:** Handles missing values and filters relevant observations.
- **Feature Engineering:** Creates binary/categorical variables for citizenship, occupation, English proficiency, education, etc.
- **Modeling:** Fits several linear regression models to estimate the impact of demographic and socioeconomic factors on wage income, including interaction effects.
- **Interpretation:** Provides summary statistics and model outputs for further interpretation.

## Notes
- The dataset (`California State Data Set.csv`) is not included due to size and licensing. Please download it from the official PUMS/ACS source.
- The script is well-commented for clarity and reproducibility.
- For questions or contributions, please open an issue or submit a pull request.

## License
This project is provided for academic and research purposes. Please cite appropriately if used in publications. 