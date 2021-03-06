import ricerca
import json
from farmaco import Farmaco


def leggi_farmaci(file):
    """
    Restituisce una lista di oggetti `Farmaco` dal file json `file`
    `file`: il file da cui leggere il json
    """
    with open(file, "r") as infile:
        lista = json.load(infile)
    l = []
    for f in lista:
        l.append(Farmaco.from_json(**f))
    return l


def scrivi_farmaci(file, lista_farmaci):
    """
    Scrive `lista_farmaci` su `file`
    `file`: il file su cui scrivere
    `lista_farmaci`: la lista dei farmaci da scrivere sul file
    """
    with open(file, "w") as outfile:
        json.dump(
            [farmaco.as_dict() for farmaco in lista_farmaci],
            fp=outfile,
            separators=(", ", ": "),
            indent=4,
        )



# Ricerca di tutti i farmaci
ricerca.ricerca_farmaco("json/lista_farmaci.json")

# estrazione del contenuto dei fogli illustrativi
farmaci = leggi_farmaci("json/lista_farmaci.json")
for i, farmaco in enumerate(farmaci):
    print(i)
    farmaco.estrai_pdf()
farmaci[:] = [x for x in farmaci if x.fi != ""]
scrivi_farmaci("json/lista_farmaci_FI.json", farmaci)

# filtraggio dei FI
farmaci = leggi_farmaci("json/lista_farmaci_FI.json")
for i, farmaco in enumerate(farmaci):
    print(i)
    farmaco.rimuoviTesto()
    farmaci[:] = [x for x in farmaci if x.fi != ""]
scrivi_farmaci("json/lista_farmaci_FI_new.json", farmaci)

# modifica testo: testo tutto minuscolo e rimozione spazi in descrizione_farmaco e ditta
farmaci_FI = leggi_farmaci("json/lista_farmaci_FI_new.json")
for farmaco in farmaci_FI:
    farmaco.descrizione_farmaco = "_".join(
        farmaco.descrizione_farmaco.lower().split()
    )
    farmaco.ditta = "_".join(farmaco.ditta.lower().split())
    farmaco.principio_attivo = farmaco.principio_attivo.lower()
    farmaco.fi = farmaco.fi.lower()
scrivi_farmaci("json/lista_farmaci_FI_def.json", farmaci_FI)

# estrazione delle informazioni cruciali dai FI
farmaci = leggi_farmaci("json/lista_farmaci_FI_def.json")
for i, farmaco in enumerate(farmaci):
    farmaco.estrai_info()
    print(i)
farmaci[:] = [x for x in farmaci if x.fi != ""]
scrivi_farmaci("json/farmaci_FI_1.json", farmaci)

# Scrittura farmaci in prolog
farmaci = leggi_farmaci("json/farmaci_FI_1.json")
with open("prolog/farmaci.pl", "w") as prologFile:
    for farmaco in farmaci:
        prologFile.write(farmaco.to_prolog())
        prologFile.write("\n")
