# Shipping Volume Forecast

This repository contains the codes for a real world project I did at Purdue from Jan to May'2022.

## Problem
Our client was a furniture retail company located in the US who sourced most of their products from the east. 
Every year they enter in to contracts with carrier companies who ship their goods across the oceans. 
With unpredicatble demand and undesirable intentions of the carriers, forecasting next years volume was need of the hour.

## What did we do?

**Step 1 - Understanding and manipulating data**

We had shipping data from 2014 to 2022. Combined the origin, pol, pod, and bl city to define a lane and aggregated the total volume per month for each lane.
 
 **Step 2 - Modelling**
 
Modelled and plotted each lane with the best combination of mean imputation of missing values, if needed, and ARIMA model, ETS models. Evaluated the models using
MAPE so we can compare the accuracy across lanes.

**Results**

With our forecasted volumes, we saved the client more than $2.5 million USD. 
