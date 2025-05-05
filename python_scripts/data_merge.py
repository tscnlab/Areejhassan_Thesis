import pandas as pd
df_cortisol = pd.read_excel('../input_data/cortisolMelatonin/TUM_EcoSleep_Results_MEL&CORT_ saliva samples_excel.xlsx')
#df_saliva = pd.read_csv('2025.03.18.EcoSleep_Saliva_Results.v.0.0.2.csv', sep=";", encoding='latin1')
df_saliva = pd.read_excel('../input_data/cortisolMelatonin/2025.03.25.EcoSleep_Saliva_Results.v.0.0.3.xlsx')
'''
for index_cor, row_cor in df_cortisol.iterrows():
    print(index_cor)
    print(row_cor)
    index_saliva = df_saliva.index[df_saliva['sample ID'] == row_cor["Sample ID"]][0]
    #if df_saliva.loc[index_saliva, 'time_salivette'].any():
    df_cortisol.loc[index_cor, 'Time_Salivette'] = df_saliva.loc[index_saliva, 'time_salivette']
    if(pd.notna(df_saliva.loc[index_saliva, 'melatonin_reanalysed'])):
        df_cortisol.loc[index_cor, 'melatonin'] = df_saliva.loc[index_saliva, 'melatonin_reanalysed']
#df_cortisol.to_csv('merged_file.csv',  sep=';')
df_cortisol.to_excel('merged_file.xlsx')
'''
for index_saliva, row_saliva in df_saliva.iterrows():
    df_saliva.loc[index_saliva, 'melatonin'] = row_saliva['melatonin (pg/mL)']
    if(len(df_cortisol.index[df_cortisol["Sample ID"] == row_saliva['sample ID']]) > 0):
        index_cor = df_cortisol.index[df_cortisol["Sample ID"] == row_saliva['sample ID']][0]
        df_saliva.loc[index_saliva, 'time_ema'] = df_cortisol.loc[index_cor, 'Time_EMA']
        df_saliva.loc[index_saliva, 'Group'] = df_cortisol.loc[index_cor, 'Group']
        if (pd.notna(row_saliva['melatonin_reanalysed'])):
            df_saliva.loc[index_saliva, 'melatonin'] = row_saliva['melatonin_reanalysed']

df_saliva.to_excel('../output_data/merged_file_reversed_1.xlsx')
