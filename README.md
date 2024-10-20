# second-sample-analysis
Second sample repository with excerpts to highlight some of my quantitative/ coding skills

Background: The client was a consortium of Germany’s biggest NGOs working with refugee/ IDP communities in Asia, Africa, and Latin America. The consortium initiated a project to strengthen child protection amongst refugees/ IDPs as well as their host communities. To guide programming, I was tasked to design a pre/ post evaluation design. As part of the evaluation design, a baseline was commissioned as well. I designed the baseline framework, which included of a multi-stage sampling design. Data was collected in country by local consultants. The data was collected using KOBO toolbox. The data was then pulled into R using an API. I used the data collected in country to perform the global baseline analysis. Within the current excerpt from the global baseline analysis, I determine the typical socio-demographic profile of primary caregivers that were interviewed across the different levels. To do so, I used sampling weights.

The entire project consists of the following folders:
01 raw data, which contains raw data pulled into the project. Note, some relevant data may be pulled from other sources using API's.
02 processed data, which contains data manipulated during project work. 
03 scripts, which contains all R scripts to load, clean, and analyse data. The sequence in which to run the files are clearly indicated within the script names.
04 results, which contains all analysis output files such as tables and graphs produced during the analysis. 
05 documentation, which contains the reports that present the analysis results and findings.

The entire project is managed in RStudio. To review and edit the project, open the Rproj-file 'second-sample-analysis', which is located at the top-level of the repository. It opens up the two R-scripts, which are placed in the folder '03 scripts'.

Note: the main bulk of the data is pulled from Kobo toolbox through a live API. Th credentials have been removed from the script '01_data loading & cleaning_20241015_V01'. Thus, running the script will NOT result in pulling in the data.  