# election_results

> ## Purpose 
The primary objective of this project was to determine whether socioeconomic or demographic variables could be used effectively to predict state-level presidential election outcomes. The goal was to build a data science model capable of predicting US Presidential election results based on factors such as educational attainment, demographic variables, voting participation rates, age, unemployment, and median income. Analyzing factors that impact elections is important as it helps track trends in socioeconomic conditions and education levels in the US, which can influence the country's development. Furthermore, studying US presidential elections provides an opportunity to identify trends in voting behavior and electoral preferences.

## Project Overview

- **Objective:**: The project aimed to create a model to predict US Presidential election results at the national level and spanning the seven presidential elections that occurred between 2000 and 2024, specifically to predict state-level outcomes using socioeconomic and demographic variables.
- **Domain:** Politics
- **Key Techniques:** Random Forest Classificatiom, Regrerssion, Exploratory Data Analysis.

---

## Project Structure

```
├── data/                 # Raw and processed data Notebook 
├── code/                 # Jupyter notebooks and Python scripts: https://github.com/jamilditter/election_results/blob/main/code/Data%20Cleaning.ipynb, https://github.com/jamilditter/election_results/blob/main/code/Data%20Cleaning.ipynb
├── reports/              # https://github.com/jamilditter/election_results/blob/main/reports/Project%20Progress%20Report.docx
├── requirements.txt      # Dependencies
└── README.md             # Project documentation
```

---

## Data

- **Source:**
Voting registration data: Bureau, US Census. “Voting and Registration in the Election of November 2024.” Census.Gov, US Census, 24 Apr. 2025, www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-587.html. 

Educational Attainment Data: Bureau, US Census. “CPS Historical Time Series Tables.” Census.Gov, US Census, 25 Aug. 2025, www.census.gov/data/tables/time-series/demo/educational-attainment/cps-historical-time-series.html. 

Median income by state data: Economic Data, Federal Reserve. “Federal Reserve Economic Data.” MEDIAN HH INCOME BY STATE 1984 -2023 (Updated Periodically) - FRED\ALFRED - St. Louis Fed, FRED, 2025, fredaccount.stlouisfed.org/public/datalist/8534/. 

Download center, StatsAmerica. StatsAmerica Download Center, 2025, www.statsamerica.org/downloads/default.aspx. 

Economic Data, Federal Reserve. “Federal Reserve Economic Data.” MEDIAN HH INCOME BY STATE 1984 -2023 (Updated Periodically) - FRED\ALFRED - St. Louis Fed, FRED, 2025, fredaccount.stlouisfed.org/public/datalist/8534/. 

MIT Election Data and Science Lab, 2018, "County Presidential Election Returns 2000-2024", https://doi.org/10.7910/DVN/VOQCHQ, Harvard Dataverse, V16, UNF:6:NKTy7eW9uEWX4imXpPxf5g== [fileUNF]

Statista, Statista. “Educational Attainment Distribution in the United States from 1960 to 2022 .” Statista, 2025, www.statista.com/statistics/184260/educational-attainment-in-the-us/. 

- **Description:** These data sources were used to put together our merged + clean deataset that we used for our analysis.
- **License:** MIT License

Copyright (c) 2025 CameronMangione

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

---

## Analysis
The methodology involved gathering nine demographic and economic factors into datasets sorted by year and state, which were compiled from ACS, BLS, LAUS, FRED, and iPUMS, then merged with voting data from the MIT Election Data and Science Lab, covering elections from 2000 to 2024. Exploratory analysis included computing correlation matrices and generating pairwise scatterplots to assess linear relationships among predictors and outcomes. For the random forest classification model, the data was split using an 80-20 training and testing split, and the model used a dummy variable to predict a Democratic victory. For the linear regression analysis, single-predictor models were created first. This was followed by the creation of two multiple linear regression models using all factors: one predicting Democrat vote percentage and one predicting Republican vote percentage. Reduced linear models were developed by removing predictors that were not statistically significant (p-values less than 0.05), and coefficients in these reduced models were standardized to determine the relative impact of predictors. Swing-state specific models were also developed for Wisconsin, Pennsylvania, Ohio, Michigan, and Florida.



---

## Results
The national random forest classification model achieved an overall accuracy score of 83.3%. The model identified the three most important predictors as voter participation, unemployment rate, and median income. In terms of precision, the model was more accurate when predicting Democratic wins (87.5%) compared to Republican wins (80%). However, random forest models developed for individual swing states or the combined five swing states only achieved an accuracy of 50%. For the multiple linear regression models, the Democrat model had an R-squared value of 0.445, and the Republican model had an R-squared value of 0.403. Across all models, the findings indicated that median income and unemployment rate were the strongest predictors of how a state would vote. In the swing-state regression models, the full Republican model had an R-squared value of 0.691, while the full Democrat model had a value of 0.656. The national multiple linear regression model produced an error rate generally between approximately ±5-7% from the true voting percentage total for each group on average.



---

## Authors

- Cameron Mangione - [@CameronMangione](https://github.com/CameronMangione)
- Jamil Ditter - [@jamilditter](https://github.com/JamilDitter)
- Alex Song - [@AlexSong-Lab](https://github.com/alexsong-lab)

---

## License

MIT License

Copyright (c) 2025 CameronMangione

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


---

## Acknowledgements

- numpy, scikit, pandas (especially pandas), plotly, matplotlib, seaborn, wget, statsmodels
- Tutorials or papers referenced
- Inspiration or collaborators
