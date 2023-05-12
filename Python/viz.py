import pandas as pd
import matplotlib.pyplot as py
import numpy
import os
os.getcwd()
df = pd.read_csv("overdoses.csv")


year=df['Year']
any_opioid=df['Any opioid']
prescribed=df['Commonly Prescribed Opioids (Natural & Semi-Synthetic Opioids and Methadone)']
heroin=df['Heroin']
synthetic=df['Synthetic opioid analgesics excluding methadone']
print(df)

fig, ax = py.subplots(figsize=(15,5))
ax.plot(year,any_opioid, label='Any Opioid')
ax.plot(year,prescribed, label='Commonly Prescribed opioids')
ax.plot(year,heroin, label='Heroin')
ax.plot(year,synthetic, label='Other Synthetic Opioids')

ax.set_xlabel  ('Year')
ax.set_ylabel ('Deaths per 100,000 population')
ax.set_title('Overdose death rates involving opioids, United States, 1999-2020')
leg = py.legend(loc= "upper center")

py.show()


data = pd.DataFrame({'period': [1, 2, 3, 4, 5, 6, 7, 8],
                   'team_A': [20, 12, 15, 14, 19, 23, 25, 29],
                   'team_B': [5, 7, 7, 9, 12, 9, 9, 4],
                   'team_C': [11, 8, 10, 6, 6, 5, 9, 12]})

#create area chart
import seaborn as sns [UPLOAD THIS]

#set seaborn style
sns.set_theme()

py.stackplot(data.period, data.team_A, data.team_B, data.team_C)
py.show()

py.stackplot(year,any_opioid, prescribed, heroin, synthetic,
              labels=['any_opioid','prescribed','heroin', 'synthetic'])
py.legend(loc='upper left')

py.show()