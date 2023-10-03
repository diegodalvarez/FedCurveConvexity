# An Analysis of Federal Reserve Policy via Treasury Yield Curve Convexity
```bash
    FedFundsConvexity
      └───src
          │   collect_data.R
          │   butterfly_model.R
          │   regression.R
          │   params.R
      └───data
          │   data.parquet
          │   flys.parquet
          │   params.parquet
```

src files:
* ```collect_data.R```: Collects data from Fred for yield curve treasury rates and Fed Funds rates
* ```butterfly_model.R```: Creates all yield curve butterfly rates for every possibly teneors
* ```regression.R```: Does a regression on one specific butterfly regression
* ```params.R```: Analysis of the OLS parameters of the regression

data files:
* ```data.parquet```: FRED Treasury & Fed Funds date
* ```fly.parquet```: All Yield Curve FLys data
* ```params.parquet```: Fly regression Paramater Analysis

## Background
Curve Convexity via Butterflys for Fed Funds Analysis. The idea behind this model is to measure the confidence that is placed in the federal reserve's montery policy. Since the treasury curve will be the most responsive set of securities that can be parsed via their term we can use an analysis of the curve. Ideally we can regress specific treasury rates to their respective butterflies. Since the butterflies are a measure of convexity and simply put curve volatility we can analyze relationship between yields and the curve. Essentially as we regress the treasury yields to their respective yield curve we are measuing how strong the relationship is between respective tenors. This is all done by measure the R squared of the regression. As the R squared decreases the tenors trade less in line with respect to the curve. 

We can first start by picking a Treasury Butterfly in this case we'll use the 2-3-5 butterfly. We can see that there is a strong relationship between by the yield and the butterfly yield. We can also see although there is some homogeneity within the data discrimating by year seems to show clear differences in the regression.
![image](https://github.com/diegodalvarez/FedCurveConvexity/assets/48641554/9a560644-e69f-4819-ac95-22dd39d23d57)

In this case we can break down by the year and examine the various R squares for the regression. We are left with this bar chart of the R squared over time. 
![image](https://github.com/diegodalvarez/FedCurveConvexity/assets/48641554/71fd1266-5156-408e-b751-1c3321a55d36)

Looking at the distribution of the parameters in this case using a 1y rolling linear regression
![image](https://github.com/diegodalvarez/FedCurveConvexity/assets/48641554/536de6ab-6299-44e8-9ddc-3e88770177c2)
