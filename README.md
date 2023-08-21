# An Analysis of Federal Reserve Policy via Treasury Yield Curve Convexity

## Background
Curve Convexity via Butterflys for Fed Funds Analysis. The idea behind this model is to measure the confidence that is placed in the federal reserve's montery policy. Since the treasury curve will be the most responsive set of securities that can be parsed via their term we can use an analysis of the curve. Ideally we can regress specific treasury rates to their respective butterflies. Since the butterflies are a measure of convexity and simply put curve volatility we can analyze relationship between yields and the curve. Essentially as we regress the treasury yields to their respective yield curve we are measuing how strong the relationship is between respective tenors. This is all done by measure the R squared of the regression. As the R squared decreases the tenors trade less in line with respect to the curve. 

We can first start by picking a Treasury Butterfly in this case we'll use the 2-3-5 butterfly. We can see that there is a strong relationship between by the yield and the butterfly yield. We can also see although there is some homogeneity within the data discrimating by year seems to show clear differences in the regression.
![image](https://github.com/diegodalvarez/FedCurveConvexity/assets/48641554/60a0af48-ae43-4d62-859e-f95a1793c199)

In this case we can break down by the year and examine the various R squares for the regression. We are left with this bar chart of the R squared over time. 
![image](https://github.com/diegodalvarez/FedCurveConvexity/assets/48641554/dc84448b-a505-4d73-b9d8-f3eb351a0acf)

Looking at the distribution of the parameters in this case using a 1y rolling linear regression
![image](https://github.com/diegodalvarez/FedCurveConvexity/assets/48641554/2be37020-8688-4149-86b2-e402af489ebe)
