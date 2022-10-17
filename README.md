# Body Fat Study

This is a project that we are working on to predict the body fat% based on different features.

There are three folders named as "Code", "Data" and "Image". 

The Code folder contains three R files. 
"Final Code.R" serves for data analysis and modeling, "model diagnose and visualization.R" serves for model evaluation, and "shiny.R" is our  Shiny App. 

Inside the Data Work folder:
The raw data is found under the name of "BodyFat". It has 252 rows and 17 features.
The cleaned data is found under the name of "BodyFat_Clean". It has 249 rows and 17 features.

The Image folder contains all Images in the Code.

The final model that we came up with is : Body Fat% = 0.78 + 0.60 * Abdomen(cm) - 0.21 * Height(cm)
