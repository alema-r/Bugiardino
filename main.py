import ricerca
import utils 
import json
from farmaco import Farmaco

def leggiFarmaci(file):
    """
    Restituisce una lista di oggetti `Farmaco` dal file json `file`
    `file`: il file da cui leggere il json
    """
    with open(file, 'r') as infile:
        lista = json.load(infile)
    l=[]
    for f in lista:
        l.append(Farmaco.fromJSON(**f))
    return l


def scriviFarmaci(file, lista_farmaci):
    """
    Scrive `lista_farmaci` su `file`
    `file`: il file su cui scrivere
    `lista_farmaci`: la lista dei farmaci da scrivere sul file
    """
    with open(file, 'w') as outfile:
        json.dump([farmaco.asDict() for farmaco in lista_farmaci], fp=outfile, separators=(', ',': '), indent=4)

def main():

    # Ricerca di tutti i farmaci
    # ricerca.ricerca_farmaco('json/lista_farmaci.json')

    # estrazione del contenuto dei fogli illustrativi
    # farmaci = leggiFarmaci("json/lista_farmaci.json")
    # for farmaco,i in zip(farmaci, range(len(farmaci))):
    #     print(i)
    #     farmaco.estrai_pdf()
    # farmaci[:] = [x for x in farmaci if x.fi != '']
    # scriviFarmaci('json/lista_farmaci_FI.json', farmaci)

    # filtraggio dei FI
    # farmaci = leggiFarmaci("json/lista_farmaci_FI.json")
    # for farmaco,i in zip(farmaci, range(len(farmaci))):
    #     print(i)
    #     farmaco.rimuoviTesto()
    # farmaci[:] = [x for x in farmaci if x.fi != '']
    # scriviFarmaci('json/lista_farmaci_FI_new.json', farmaci)

    # modifica testo: testo tutto minuscolo e rimozione spazi in descrizione_farmaco e ditta
    # farmaci_FI = leggiFarmaci("json/lista_farmaci_FI_new.json")
    # for farmaco in farmaci_FI:
    #     farmaco.descrizione_farmaco = '_'.join(farmaco.descrizione_farmaco.lower().split())
    #     farmaco.ditta = '_'.join(farmaco.ditta.lower().split())
    #     farmaco.principio_attivo = farmaco.principio_attivo.lower()
    #     farmaco.fi = farmaco.fi.lower()
    # scriviFarmaci("json/lista_farmaci_FI_def.json", farmaci_FI)

    # estrazione delle informazioni cruciali dai FI
    # farmaci = leggiFarmaci("json/lista_farmaci_FI_def.json")
    # for (farmaco,i) in zip(farmaci ,range(len(farmaci))):
    #     farmaco.estrai_info()
    #     print(end="\r%6.0f %%" % (i / (100 - 1) * 100))
    # farmaci[:] = [x for x in farmaci if x.fi != '']
    # scriviFarmaci('json/farmaci_FI_1.json', farmaci)

    # Scrittura farmaci in prolog
    farmaci = leggiFarmaci("json/farmaci_FI_1.json")
    with open('prolog/farmaci.pl', 'w') as prologFile:
        for farmaco in farmaci:
            farmaco.remove_stopwords()
            prologFile.write(farmaco.toProlog())
            prologFile.write("\n")

if __name__ == '__main__':
    main()