# Comparison of clustering methods on an RFM model

This repository contains the files for the project "Comparison of clustering methods on an RFM model" for the course Data Analysis II at the Humboldt-University of Berlin in the Winter Term 2018/19.

## Description
Understanding its customers is a prime objective for companies. This information aids in both business and marketing decisions. In particular, when it comes to marketing, it is important to be able to distinguish among different classes of customers, in order to better target them. This is where the *Recency, Frequency, Monetary (RFM)* model and cluster analysis come to help ([[1]](#1)). The RFM model is a frequently used technique to select customers for direct mailings ([[2]](#2)), which describes customers’ behavior by three variables relating to their purchase behavior in a specific time frame. The RFM attributes can already be used to segment customers, but a better result can be accomplished using more advanced clustering techniques.

This study was realized with a similar dataset to the one employed by [[3]](#3), differing of one month in the time frame. Therefore, the purpose of this analysis was to challenge the results from [[3]](#3) by using two other clustering methods, *Agglomerative Hierarchical Clustering* and *DBSCAN*, each of which has its own strengths and weaknesses. The three techniques delivered different results, which have been compared looking at their size, Silhouette Index and at how they reflect the RFM Segments. 

The seminar paper can be read [here](https://github.com/silvia-ventoruzzo/Datenanalyse-II-WISE-2018/blob/master/SeminarPaper/SeminarPaper.pdf).

## Content
The project is divided into the following parts:
1.	Data preparation: 
    - Calculation of RFM variables based on the sales data
2.	Exploratory Data Analysis: 
    - Descriptive statistics and distribution plots
3.	Clustering: 
    - k-Means
    - Hierarchical Clustering
    - DBSCAN
4.	Clustering validation and comparison
    - Silhouette Index

## Data
The data for the project was downloaded from [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Online+Retail).

## References
<a id="1">[1]</a> Birant, D. (2011): Data mining using RFM analysis. in Knowledge-oriented applications in data mining, InTech.

<a id="2">[2]</a> Bult, J. R. and T. Wansbeek (1995): Optimal selection for direct mail. Marketing Science, 14, 378–394.

<a id="3">[3]</a> Chen, D., S. L. Sain, and K. Guo (2012): Data mining for the online retail industry: A case study of RFM model-based customer segmentation using data mining. Journal of Database Marketing & Customer Strategy Management, 19, 197–208.


