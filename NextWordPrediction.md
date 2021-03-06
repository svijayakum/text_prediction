NextWordPrediction
========================================================
author: Swathi Vijayakumar
date: Thu Mar 29 12:18:54 2018
autosize: true
transition: fade

Introduction
========================================================
The final project for the Data Products Course consists of two parts:
- A shiny app which predicts the next word based on one, two or three words that the user enters
  - App is available at the following website:       
           https://svijayakum.shinyapps.io/PredictiveNextWord/
           
- A slide deck presentation that explains the algorithm and app
  


How to use the prediction app
========================================================

- This app was built using three english language texts and so can only be used for prediction in the english language. 

- Enter in a phrase containing, one, two or three words in the input box and click predict. 

- The algorithm will take these as inputs and give you the three most likely predictions 



The Algorithm
========================================================

Random samples of about 5000 lines from text file (twitter, news, and blog) was used for building the prediction model. Data pre-processing was done by removing all numbers, punctuations and whitespace. All characters were transformed to lowercase as well. 

One, two and three gram frequencies were calculated using the NGramTokenizer function. Each n-gram frequency was stored as a CSV file that was read into the text prediction algorithm.  

The text prediction algorithm counts the number of words in the input string to determine which n-gram to extract the most likely next word from. The algorithm works as a look-up table that matches the inputs and outputs the next work with the highest frequenct. 

Example
========================================================

![my image](PredictTextExample.png)

