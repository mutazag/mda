#%%
import pandas as pd
import seaborn as sns
# %%
df = pd.read_csv('boston.csv')

# %%
sns.pairplot(df)

# %%
wine = pd.read_csv(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
    header=None,
    names=["Cult", "Alc", "MalAcid", "Ash", "AshAlk", "Mag", "TotPhen","Flav", "NonFlav", "Proant", "Color", "Hue", "OD280OD315", "Proline"])

#%%
wine.shape
# %%
wine.describe()

#%%
wine.head()

# %%
sns.pairplot(wine)

# %%
sns.distplot(wine)

# %%
sns.distributions.kdeplot(wine)

# %%
sns.pairplot(wine, diag_kind='kde')

# %%
