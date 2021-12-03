import subprocess
import re

def curl(url, head=False):
    """
    Funzione che usa `subprocess` per lanciare il comando `curl`. Questo perchè
    con `requests` non è possibile scaricare eventuali documenti PDF. 

    `url`: l'url da usare con `curl`
    `head`: se è True, `curl` viene lanciato con l'opzione `--head`. Inoltre, in
    questo caso l'output sarà `True` o `False` in base al codice della risposta
    HTTP.
    """
    if head:
        proc = subprocess.Popen(["curl", "--head", "-s", url], stdout=subprocess.PIPE)
        (out, _) = proc.communicate()
        out = False if out.startswith(b'HTTP/1.1 500') else True
    else:    
        proc = subprocess.Popen(["curl", "-s", url], stdout=subprocess.PIPE)
        (out, _) = proc.communicate()
    return out

def uniqAndSort(file):
    cmd = f"sort {file} | uniq > tmp && mv tmp {file}"
    subprocess.run(cmd, shell=True)
    
    
def stripBadText(s):
    """
    Rimuove il testo indesiderato estratto dal PDF del foglio illustrativo.
    
    `s`: stringa con il contenuto PDF del foglio illustrativo si vuole modificare
    """
    s = re.sub("Documento reso disponibile da AIFA il [0-9]{2}\/[0-9]{2}\/[0-9]{4}\\nEsula dalla competenza dell\\u2019AIFA ogni eventuale disputa concernente i diritti di propriet\\u00e0 industriale e la tutela brevettuale dei dati relativi all\\u2019AIC dei \\nmedicinali e, pertanto, l\\u2019Agenzia non pu\\u00f2 essere ritenuta responsabile in alcun modo di eventuali violazioni da parte del titolare dell'autorizzazione \\nall'immissione in commercio \(o titolare AIC\)\.", "", s)
    s = re.sub(r"\n", " ", s)
    s = re.sub(r"\r", "", s)
    s = re.sub(r"\t", "", s)
    # s = re.sub(r"\\u[0-9]{4}", "", s)
    return s.strip()

def toPrologFact(funtore, atomo):
    """
    Restituisce un fatto in Prolog.

    `funtore`: il funtore da specificare
    `atomo`: l'atomo da inserire all'interno del funtore
    """
    return f'{funtore}("{atomo}").'