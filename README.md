# Project title
gep survey data analysing 

## Description
The Gender Equality Plan (GEP) monitoring survey analysis script contains R code for analysing INSPIRE's survey. INSPIRE survey targeted 27 EU member plus Bosnia-Herzegovina, Israel, Norway, Serbia, Switzerland, and the UK. Survey is launched in 2024. Data analysis was conducted using various software tools; however, to promote transparency and reproducibility, we aim to publish the analysis code in R. Due to data protection regulations, the survey data cannot be publicly shared. The data analysis script is based on the <strong>D3.3 Report on GEP Monitoring Survey and Webcrawl Results</strong>. The report can be accessed via the following link: <a href="https://zenodo.org/records/14264597" target="_blank">https://zenodo.org/records/14264597</a>.

INSPIRE is an EU funded project. INSPIRE is Europe's Centre of Excellence on inclusive gender equality in research and innovation. It brings together cutting-edge knowledge, ambitious policy approaches, and innovative practices to provide a gateway for scholars, equality experts, practitioners and trainers to connect and share resources, as well as co-create new ones. For more info please visit: <a href="https://www.inspirequality.eu/" target="_blank">INSPIRE's official website</a>


## List of scripts

<ol>
<li>Response rate by country</li>
<li>Response rate by country cluster</li>
<li>Response rate by language</li>
<li>Response rate by type of organisation</li>
<li>People and functions fullfilling the survey</li>
<li>Prevalence survey organisation type</li>
<li>Reasons for not having a GEP</li>
<li>Kind of GEP or equivalent</li>
<li>Yea of the first GEP in the organsiation</li>
<li>Year of the first GEP in the organsiation - only 2020 and 2024</li>
<li>Influence of eligibility criterion to set up a GEP</li>
<li>Publication of the GEP</li>
<li>Language of the GEP</li>
<li>Mean of ratings changes with and without GEP</li>
<li>Mean of ratings stabilisation with and without GEP</li>
<li>Mean of ratings stabilisation before 2021 and after 2021</li>
<li>Mean of ratings stabilisation with and without financial resources</li>
<li>Mean of ratings changes country cluster</li>
<li>Mean of ratings stabilisation country cluster</li>
<li>Relevance of the GEP on the achieved positive changes</li>
<li>Correlation between the relevance of the GEP for achieved positive changes and achieved stabilisation</li>


</ol>

## Installation
Make sure you have a recent version of R or RStudio. R version 4.4.3 is recommended.

Install the following required libraries to run script :

<code>install.packages("haven")</code><br>
<code>install.packages("dplyr")</code><br>
<code>install.packages("tidyr")</code><br>
<code>install.packages("ggplot2")</code><br>
<code>install.packages("readxl")</code><br>
<code>install.packages("gt")</code><br>
<code>install.packages("webshot")</code><br>
<code>install.packages("fmsb")</code><br>
<code>install.packages("scales")</code><br>
<code>install.packages("Hmisc")</code><br>
<code>install.packages("grid")</code><br>
<code>install.packages("gridExtra")</code><br>


## License
Distributed under the Attribution-NonCommercial-ShareAlike 4.0 International license. Please, see LICENSE.md for more detail.