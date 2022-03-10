import requests
import json
from farmaco import Farmaco


def ricerca_farmaco(file, query_farmaco="**"):
    """
    Ricerca di un farmaco, e successivo salvataggio in un file json

    `file`: nome del file in cui salvare l'output

    `query_farmaco`: nome del farmaco da ricercare. Default: cerca tutti i farmaci

    """

    headers = {
        "User-Agent": "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:94.0) Gecko/20100101 Firefox/94.0",
        "Accept": "*/*",
        "Accept-Language": "it,en-GB;q=0.7,en;q=0.3",
        "Connection": "keep-alive",
        "Referer": "https://farmaci.agenziafarmaco.gov.it/",
        "Sec-Fetch-Dest": "script",
        "Sec-Fetch-Mode": "no-cors",
        "Sec-Fetch-Site": "same-site",
    }

    params = (
        (
            "fl",
            "sm_field_codice_farmaco,sm_field_descrizione_farmaco,\
                sm_field_descrizione_ditta,sm_field_link_fi,sm_field_descrizione_atc",
        ),
        (
            "q",
            f"bundle:confezione_farmaco sm_field_descrizione_farmaco:{query_farmaco}*",
        ),
        ("df", "sm_field_descrizione_farmaco"),
        ("wt", "json"),
        ("rows", "150000"),
        # ('json.wrf', 'jQuery182037101282857862883_1637071501822'),
        # ('_', '1637071566697'),
    )

    print("Richiesta HTTP in corso...")
    response = requests.get(
        "https://www.agenziafarmaco.gov.it/services/search/select",
        headers=headers,
        params=params,
    )
    list_obj = json.loads(response.text)["response"]["docs"]
    lista_farmaci = []

    print("Parsing del json...")
    for obj in list_obj:
        lista_farmaci.append(Farmaco.from_json_aifa(**obj))

    # Eliminazione dei duplicati
    lista_farmaci = list(set(lista_farmaci))

    lista_farmaci[:] = [x for x in lista_farmaci if x.principio_attivo != "" and x.url_fi != ""]
    print("Salvataggio json...")
    # Salvataggio output in un array json
    with open(file, "w") as outfile:
        json.dump(
            [farmaco.as_dict() for farmaco in lista_farmaci],
            fp=outfile,
            separators=(", ", ": "),
            indent=4,
        )
    print(f"{len(lista_farmaci)} farmaci ottenuti")
