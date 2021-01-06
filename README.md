# Covid19andCancerConsortium-VBISP-2020

Automating the Processes of Structuring Clinical Data and Extracting Meaningful Medication Information from Free Text

Brendan J. Lee
Johns Hopkins University
VBISP REU: Vanderbilt Summer Research Internship Program in Biomedical Informatics

Jeremy L. Warner, MD, MS
Department of Biomedical Informatics; Division of Hematology and Oncology

Manual curation efforts often create massive amounts of unstructured data.
The COVID-19 and Cancer Consortium (CCC19) registry, which has manually collected data from >3000 COVID-19 patients with cancer, 
contains >400k cells that potentially have unstructured text, which collectively contain nearly 4 million characters; 
semi-structured fields such as laboratory values possess variability in representation. 
Computational methods must be utilized to properly convert unstructured data into organized information for analysis. 
Hence, several regular-expression-based algorithms were developed. 
The algorithms that involved numeric variables organized each variable so that values were in the proper units (BMI_V2.R and Lab_Values.R). 
These algorithms successfully structured ~99% of the data.  
Another algorithm extracted lists of drug names from unstructured medication lists (medications.R, etc.). This algorithm had precision=0.895, recall=0.903, and F1=0.899, 
based on a randomly selected sample of 40 of 858 records with data present. Analysis was conducted with base R (v.3.6.2) and packages qdap and httr. 
From these algorithms, a framework for transformation of unstructured data in the CCC19 registry was developed. 
This framework enables the continued flexibility of free text data entry to capture complex concepts, while maximizing the utility of the captured data. 
Several CCC19 projects will be utilizing this output.
