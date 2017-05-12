from bs4 import BeautifulSoup as bs
import requests as r
from pandas import DataFrame as df
import numpy as np
from difflib import get_close_matches
from pandas import DataFrame as df

def get_soup(url): return bs(r.get(url).text, 'html.parser')

soup = get_soup('https://en-marche.fr/article/communique-liste-investis')
all_candidats = soup.find('article', class_='l__wrapper--slim').find_all('p')
all_candidats_text = list(map(lambda x: x.text, all_candidats))[1:]

psdf = df.from_csv('../p17_circo.csv').reset_index()
deps = psdf[['Code du département', 'Libellé du département']].drop_duplicates().values.tolist()
deps_dict = {y.lower(): x for x, y in deps}
def proper_dep(dep): 
	if dep == "FRANCAIS-DE-L'ETRANGER": return 'etranger'
	return get_close_matches(dep.lower(), deps_dict.keys())[0]

def dep_code(dep): return deps_dict[proper_dep(dep)]

def get_dep_circ_name(x):
	x0, x1 = x.split(',')
	nom = x1.strip() #Sans l'espace du début
	x0s = x0.split(' ')
	dep = '-'.join(x0s[:-1])
	circ = ''.join(x0s[-1])
	code_dpt = dep_code(dep)
	code_dpt = ('0' + code_dpt) if (code_dpt != '2A' and code_dpt != '2B' and int(code_dpt) < 10) else code_dpt
	code_circo = str(code_dpt) + '_' + str(circ) 
	return {"code_dpt": code_dpt, "circo":str(circ), "nom":nom, "code_circo": code_circo}

macrondf = df(list(map(get_dep_circ_name,all_candidats_text))).set_index('code_circo')
macrondf.to_csv('investitures-en-marche_sans-genre.csv', index_label='code_circo')


# Comparons avec l'ancien...
anciendf = df.from_csv('../investitures-en-marche.csv')
macrondf = macrondf.reset_index().set_index('code_circo')
anciendf = anciendf.reset_index().set_index('code_circo')

print(macrondf, anciendf)

joindf = macrondf.join(anciendf, lsuffix='_new', rsuffix='_old')
joindf['Same'] = joindf['nom_new'] == joindf['nom_old']
joindf.loc[joindf['Same']== False, 'genre'] = 'X'
# Genre to X, il faudra compléter à la main
joindf['nom'] = joindf['nom_new'].combine_first(joindf['nom_old'])
# On remplace le nouveau nom par le bon
joindf['code_dpt'] = joindf['code_dpt_new'].combine_first(joindf['code_dpt_old'])
joindf['circo'] = joindf['circo_new'].combine_first(joindf['circo_old'].apply(str))
joindf = joindf.drop(['nom_new','nom_old','code_dpt_new','code_dpt_old','circo_new','circo_old', 'Same'], axis=1)

joindf.to_csv('investitures-en-marche-sexe-a-corriger.csv')
