from dataclasses import dataclass
from dataclasses import asdict
import re
from utils import curl

@dataclass(unsafe_hash=True)
class Farmaco:
    """
    Classe che rappresenta un farmaco estratto dal sito [AIFA](https://farmaci.agenziafarmaco.gov.it/bancadatifarmaci/).
    """
    codice_farmaco: str
    descrizione_farmaco: str
    ditta: str
    url_fi: str
    url_rcp: str
    procedura: str
    principio_attivo: str
    fi: str = ''

    def fromJSON(sm_field_codice_farmaco,
        sm_field_descrizione_farmaco,
        sm_field_descrizione_ditta,
        sm_field_tipo_procedura,
        sm_field_link_fi,
        sm_field_link_rcp,
        sm_field_descrizione_atc=['']):
        """
        Crea un oggetto `Farmaco` dal JSON preso dal sito [AIFA](https://farmaci.agenziafarmaco.gov.it/bancadatifarmaci/)
        """
        return Farmaco(sm_field_codice_farmaco[0],
            sm_field_descrizione_farmaco[0],
            sm_field_descrizione_ditta[0],
            re.sub("&amp;", "&", sm_field_link_fi[0]),
            re.sub("&amp;", "&", sm_field_link_rcp[0]), 
            sm_field_tipo_procedura[0],
            sm_field_descrizione_atc[0])
    
    def asDict(self):
        """
        Resituisce l'oggetto `Farmaco` in formato `dict`
        """
        return(asdict(self))

    def checkUrls(self):
        """
        Controlla se gli url del FI e del RPC sono validi, altrimenti li pone uguale a ''
        """
        # Controlli effettuati dal sito dell'AIFA
        if self.procedura != 'P':
            print(f"curl, {self.codice_farmaco}")
            # Controlla se il pdf esiste, in caso contrario cancella i link
            if not curl(self.url_fi, head=True):
                self.url_fi = ""
                self.url_rcp = ""
    