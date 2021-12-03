import requests
import json
import fitz
from farmaco import Farmaco
from utils import curl


def ricerca_farmaco(file, query_farmaco="**"):
    """
    Ricerca di un farmaco, e successivo salvataggio in un file json

    `file`: nome del file in cui salvare l'output

    `query_farmaco`: nome del farmaco da ricercare. Default: cerca tutti i farmaci

    """

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
        ('fl', 'sm_field_codice_farmaco,sm_field_ditta,sm_field_descrizione_ditta,sm_field_tipo_procedura,sm_field_link_fi,sm_field_link_rcp,sm_field_descrizione_atc'),
        ('q', f'bundle:confezione_farmaco sm_field_ditta:{query_farmaco}*'),
        ('df', 'sm_field_ditta'),
        ('wt', 'json'),
        ('rows', '150000'),
        # ('json.wrf', 'jQuery182037101282857862883_1637071501822'),
        # ('_', '1637071566697'),
    )
    
    response = requests.get('https://www.agenziafarmaco.gov.it/services/search/select', headers=headers, params=params)
    list_obj = json.loads(response.text)["response"]["docs"]
    f = []
    for obj in list_obj:
        f.append(Farmaco.fromJSON(**obj))

    # Eliminazione dei duplicati
    f = list(set(f))
    
    for farmaco in f:
        if farmaco.principio_attivo == "":
            f.remove(farmaco)

    # Salvataggio output in un array json
    with open(file, 'w') as outfile:
        json.dump([farmaco.asDict() for farmaco in f], fp=outfile, separators=(', ',': '), indent=4)
    
    print("Farmaci ottenuti")
    

def estrai_pdf(farmaco):
    """
    Estrae tutte le informazioni dal foglio illustrativo

    `farmaco`: il `Farmaco` di cui estrarre le informazione del foglio illustrativo 
    """
    
    pdf = curl(farmaco["url_fi"])
    try:
        doc = fitz.open("pdf", stream=pdf)
        pages = ""
        for page in doc:
            pages += page.get_text()
        farmaco['fi'] = pages
    except RuntimeError:
        farmaco['fi'] = ''
