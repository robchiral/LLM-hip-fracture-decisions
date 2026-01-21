import pandas as pd
import os

openai_data = pd.read_csv('./data/openai.tsv', sep='\t')
gemini_data = pd.read_csv('./data/gemini.tsv', sep='\t')
deepseek_data = pd.read_csv('./data/deepseek.tsv', sep='\t')

data = pd.concat([openai_data, gemini_data, deepseek_data], ignore_index=True)

data = data[['model_name','surgery_id','sex','variable','replicate_id','anesthetic_plan','anesthetic_plan_justification']]
data.to_csv('./topics/justifications.csv', index=False)

data.loc[data['anesthetic_plan'].str.contains('neuraxial')]['anesthetic_plan_justification'].to_csv('./topics/neuraxial_justifications.csv', index=False)
data.loc[data['anesthetic_plan'].str.contains('general')]['anesthetic_plan_justification'].to_csv('./topics/general_justifications.csv', index=False)