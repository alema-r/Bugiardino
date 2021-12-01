import subprocess

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