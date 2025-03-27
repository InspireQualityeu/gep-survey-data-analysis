# Project title
gep survey data analysing 

## Description
The Gender Equality Plan (GEP) monitoring survey analysis script contains R code for analysing INSPIRE's survey. INSPIRE survey targeted 27 EU member plus Bosnia-Herzegovina, Israel, Norway, Serbia, Switzerland, and the UK. Survey is launched in 2024. Data analysis was conducted using various software tools; however, to promote transparency and reproducibility, we aim to publish the analysis code in R. Due to data protection regulations, the survey data cannot be publicly shared. The data analysis script is based on the <strong>D3.3 Report on GEP Monitoring Survey and Webcrawl Results</strong>. The report can be accessed via the following link: <a href="https://zenodo.org/records/14264597" target="_blank">https://zenodo.org/records/14264597</a>.

INSPIRE is an EU funded project. INSPIRE is Europe's Centre of Excellence on inclusive gender equality in research and innovation. It brings together cutting-edge knowledge, ambitious policy approaches, and innovative practices to provide a gateway for scholars, equality experts, practitioners and trainers to connect and share resources, as well as co-create new ones. For more info please visit: <a href="https://www.inspirequality.eu/" target="_blank">INSPIRE's official website</a>


## List of scripts

<ol>
<li>01_Response_rate_by_country</li>
<li>02_Response_rate_by_country_cluster</li>
<li>03_Response_rate_by_language</li>
<li>04_Response_rate_by_type_of_organisation</li>
<li>05_People_and_functions_fullfilling_the_survey</li>
<li>06_Prevalence_survey_organisation_type</li>
<li>07_Reasons_for_not_having_a_GEP</li>
<li>08_Kind of GEP or equivalent</li>
<li>09_Year_of_the_first_GEP_in_the_organsiation</li>
<li>10_Year_of_the_first_GEP_in_the_organsiation - only 2020 and 2024/li>
<li>11_Influence_of_eligibility_criterion to set up a GEP</li>
<li>12_Publication_of_the_GEP</li>
<li>13_Language_of_the_GEP</li>
<li>14_Mean_of_ratings_changes_with_and_without_GEP</li>
<li>15_Mean_of_ratings_stabilisation_with_and_without_GEP</li>
<li>16_Mean_of_ratings_stabilisation_before_2021_and_after_2021</li>
<li>18_Mean_of_ratings_stabilisation_with_and_without_financial_resources</li>
<li>19_Mean_of_ratings_changes_country_cluster</li>
<li>20_Mean_of_ratings_stabilisation_country_cluster</li>
<li>21_Relevance_of_the_GEP_on_the_achieved_positive_changes</li>
<li>22_Correlation between the relevance of the GEP for achieved positive changes and</li>


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