import requests
import json
from farmaco import Farmaco

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:94.0) Gecko/20100101 Firefox/94.0',
    'Accept': '*/*',
    'Accept-Language': 'it,en-GB;q=0.7,en;q=0.3',
    'Referer': 'https://farmaci.agenziafarmaco.gov.it/',
    'Connection': 'keep-alive',
    'Sec-Fetch-Dest': 'script',
    'Sec-Fetch-Mode': 'no-cors',
    'Sec-Fetch-Site': 'same-site',
    'Pragma': 'no-cache',
    'Cache-Control': 'no-cache',
}

params = (
    ('fl', 'sm_field_codice_farmaco,sm_field_descrizione_farmaco,sm_field_descrizione_ditta,sm_field_tipo_procedura'),
    ('q', 'bundle:confezione_farmaco sm_field_descrizione_farmaco:tachipirina*'),
    ('df', 'sm_field_descrizione_farmaco'),
    ('wt', 'json'),
    ('rows', '150000'),
    # ('json.wrf', 'jQuery182037101282857862883_1637071501822'),
    #('_', '1637071566697'),
)

response = requests.get('https://www.agenziafarmaco.gov.it/services/search/select', headers=headers, params=params)
list_obj = json.loads(response.text)["response"]["docs"]

for obj in list_obj:
    print(obj)

ob = list_obj[0]
f = Farmaco.fromJSON(**ob)
# print(**ob)