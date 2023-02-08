**Associations between late-life psychosis and incident dementia – a longitudinal study**

The set of scripts aims to analyze data from the National Alzheimer's Coordinating Center (NACC) to explore the longitudinal associations of late-life emergent and persistent psychosis with incident dementia in a sample of dementia-free older adults at baseline.

Late-life emergent and persistent psychosis is termed as MBI-psychosis and is operationalized from scores in NPI-Q delusion and hallucination domains, in line with core criteria of the mild behavioral impairment (MBI) construct to accurately characterize neuropsychiatric symptoms (NPS) in early dementia.

**NOTE: The source data used for this analysis cannot be provided. It is available directly from NACC upon submitting a data access request at https://naccdata.org/requesting-data/data-request-process**

**Step 1: Cleaning_NPS_groups.R** After performing basic cleaning on the NACC dataset, Cleaning_NPS_groups.R will create two NPS groups: MBI-psychosis vs No NPS prior to dementia diagnosis and writes two .csv files (one for each NPS group).

**Step 2: Reformat_data_for_Cox.R** The output files from Cleaning_NPS_groups.R are fed into Reformat_data_for_Cox.R to reformat the data to prepare it for use in Cox modeling. This script will write a single .csv file with both NPS groups merged into one dataframe.

**Step 3: Survival_analysis.R** The output file from Reformat_data_for_Cox.R is then fed into Survival_analysis.R to analyze the data. The main outputs of this analysis are:

(1) Kaplan Meier (KM) curves for the chance of survival free of dementia over ten years stratified by NPS groups: MBI-psychosis versus no NPS prior to dementia diagnosis.

(2) Forest plot of adjusted hazard ratios for dementia across NPS groups: MBI-psychosis versus no NPS prior to dementia diagnosis.

(3) Potential interaction effects between NPS groups and other model covariates
