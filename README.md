Ecosleep study: Effect of light on Melatonin onset and Cortisol Awakening Response as Biomarkers of Circadian Rhythm and Morning Alertness.
Folder Structure:
-Main
 |
 |---- input_data
 |---- output_data
 |---- python_scripts
 R scripts
 

Most of the files in melatonin and cortisol analysis use lightLogR data in .rds format. This is to avoid some repetitive task so a cleaned df was saved once. To generate these df, please run missingData.R once. Then save present_data as rds to output_data/LightLogR using saveRDS function.