from bs4 import BeautifulSoup as bs
import requests as r
from circo import circo_format_lfi
from pandas import DataFrame as df
import numpy as np

def get_soup(url): return bs(r.get(url).text, 'html.parser')

circo_format_normal = list(map(lambda x: x.split("_"), circo_format_lfi))

def parse_cand_ou_sup(tag):
	nom = tag.find("h4").text
	split_tag = tag.contents[2].split(' ') 
	cand_ou_sup = list(filter(None, split_tag))[1].split('\n')[0]
	return (nom, cand_ou_sup)

candidats_lfi = []
for dep, circ in circo_format_normal:
	url = 'https://legislatives2017.lafranceinsoumise.fr/departement/'+str(dep)+'/circonscription/'+str(circ)
	soup = get_soup(url)
	cand_plus_sup = soup.find_all('div', class_='col-sm-6')
	cand_plus_sup_focused = list(x.find('div', class_="nom") for x in cand_plus_sup)
	cand_plus_sup_identifié = list(map(parse_cand_ou_sup, cand_plus_sup_focused))
	# On a désormais une liste sous format (nom, 'Titulaire'/'Suppléant')
	print(dep, circ, cand_plus_sup_identifié)
	candidats_lfi.append((
		('%s_%s' % (dep, circ)),
		{y: x for x,y in cand_plus_sup_identifié}))

candidats_lfi_dict = {x:y for x,y in candidats_lfi}
lfidf = df.from_dict(candidats_lfi_dict).T
all_dep_codes = []
for index, row in lfidf.iterrows():
	all_dep_codes.append(index.split('_'))

dep_codes = np.array(all_dep_codes).T[0]
lfidf['code_dpt'] = list(map(lambda x: 999 if str(x) == '99' else x, dep_codes))
lfidf['circo'] = np.array(all_dep_codes).T[1]
lfidf = lfidf.reset_index().rename(columns={"index":"code_circo"}).dropna()
lfidf.to_csv('investitures-LFI.csv')
lfidf = lfidf.set_index('code_circo')

anciendf = df.from_csv('../investitures-LFI-avec-genre.csv')

anciendf = anciendf.drop(['code_dpt', 'circo'], axis=1)
lfidf = lfidf.drop(['code_dpt', 'circo'], axis=1)

joindf = anciendf.join(lfidf, lsuffix="_old", rsuffix="_new").dropna(how='all')
joindf['Same'] = ((joindf['Titulaire_old'] == joindf['Titulaire_new'])
	& (joindf['Suppléant_old'] == joindf['Suppléant_new']))
# Quelles sont les lignes à ne pas modifier ?

joindf.loc[joindf['Same']== False, 'SG'] = 'X'
joindf.loc[joindf['Same']== False, 'TG'] = 'X'
# Genre to X, il faudra compléter à la main

joindf['Titulaire'] = joindf['Titulaire_new'].combine_first(joindf['Titulaire_old'])
joindf['Suppléant'] = joindf['Suppléant_new'].combine_first(joindf['Suppléant_old'])
# On remplace le nouveau nom par le bon

joindf = joindf.drop(['Titulaire_old', 'Suppléant_old', 'Suppléant_new', 'Titulaire_new', 'Same', ], axis=1)
joindf.to_csv('investitures-LFI-sexe-a-corriger.csv')
