import numpy as np
import pandas as pd
from scipy import stats
from statsmodels.stats.multicomp import pairwise_tukeyhsd
from statsmodels.stats.multicomp import MultiComparison
import statsmodels.api as sm

# Criação dos dados
np.random.seed(42)
group1 = np.random.normal(20, 5, 30)
group2 = np.random.normal(24, 5, 30)
group3 = np.random.normal(22, 5, 30)

# Organização dos dados em um DataFrame
data = pd.DataFrame({
    'value': np.concatenate([group1, group2, group3]),
    'group': np.array(['group1']*30 + ['group2']*30 + ['group3']*30)
})

# ANOVA
model = sm.formula.ols('value ~ group', data=data).fit()
anova_table = sm.stats.anova_lm(model, typ=2)
print("ANOVA Resultados:")
print(anova_table)

# Teste de Tukey
tukey_result = pairwise_tukeyhsd(endog=data['value'], groups=data['group'], alpha=0.05)
print("\nResultado do Teste de Tukey HSD:")
print(tukey_result)

# Exibição do resultado com letras de comparação (CLD)
mc = MultiComparison(data['value'], data['group'])
tukey_cld = mc.tukeyhsd()
print("\nLetras de Comparação:")
print(tukey_cld.summary())

# Extração das letras de comparação
tukey_cld_letters = mc.groupsunique[np.argmax(tukey_cld.reject, axis=1)]
print("\nLetras de Comparação por Grupo:")
for i, group in enumerate(mc.groupsunique):
    print(f"{group}: {tukey_cld_letters[i]}")
