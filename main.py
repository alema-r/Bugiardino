from ricerca import ricerca_farmaco, estrai_pdf
from utils import toPrologFact, stripBadText, uniqAndSort
import json

def readJsonFromFile(file):
    """
    Restituisce il json dal file `file`
    `file`: il file da cui leggere il json
    """
    with open(file, 'r') as infile:
        text = json.load(infile)
    return text

def writeJsonToFile(file, json_text):
    """
    Scrive `json_text` su `file`

    `file`: il file su cui scrivere
    `json_text`: il json da scrivere
    """
    with open(file, 'w') as outfile:
        json.dump(json_text, fp=outfile, separators=(', ',': '), indent=4)

def extractAndStrip():
    """
    Per ogni farmaco presente nel file `lista_farmaci` estrae il pdf, toglie
    il testo indesiderato e se il campo foglio illustrativo risulta vuoto,
    elimina il farmaco dalla lista. Al termine di questa operazione, scrive
    la lista rimanente nel file `lista_farmaci_FI`.
    """
    farmaci = readJsonFromFile("lista_farmaci")
    for (farmaco,i) in zip(farmaci,range(len(farmaci))):
        print(i)
        if farmaco["url_fi"] != '':
            estrai_pdf(farmaco)
            if farmaco['fi'] == '':
                farmaci.remove(farmaco)
            else:
                farmaco['fi'] = stripBadText(farmaco['fi']).lower()
    writeJsonToFile("lista_farmaci_FI", farmaci)

def principi_attiviToProlog(pl_file):
    """
    Scrive in coda al file `pl_file`, tutti i principi attivi in forma di fatti del tipo
    "principio_attivo(atomo)."

    `pl_file`: il file prolog su cui scrivere
    """
    farmaci_fi = readJsonFromFile("lista_farmaci_FI")
    with open(pl_file, 'a') as prologFile:
        for (farmaco,i) in zip(farmaci_fi ,range(len(farmaci_fi))):
            print(i)
            prologFile.write(toPrologFact("principio_attivo",farmaco['principio_attivo'].split()[0].lower()))
            prologFile.write("\n")

# Ricerca di tutti i farmaci
# ricerca_farmaco('lista_farmaci')

# Ricerca di "tachipirina"
# ricerca_farmaco('singolo_farmaco', query_farmaco="tachipirina")

principi_attiviToProlog("principi_attivi.pl")
uniqAndSort("principi_attivi.pl")