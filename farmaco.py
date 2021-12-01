from dataclasses import dataclass
from dataclasses import asdict
import json

@dataclass(unsafe_hash=True)
class Farmaco:
    """
    Classe che rappresenta un farmaco estratto dal sito [AIFA](https://farmaci.agenziafarmaco.gov.it/bancadatifarmaci/).
    """
    codice_farmaco: str
    descrizione_farmaco: str
    ditta: str

    def fromJSON(sm_field_codice_farmaco,
        sm_field_descrizione_farmaco,
        sm_field_descrizione_ditta):
        """
        Crea un oggetto `Farmaco` dal JSON preso dal sito [AIFA](https://farmaci.agenziafarmaco.gov.it/bancadatifarmaci/)
        """
        return Farmaco(sm_field_codice_farmaco[0],
        sm_field_descrizione_farmaco[0],
         sm_field_descrizione_ditta[0])
    
    def asDict(self):
        """
        Resituisce l'oggetto `Farmaco` in formato `dict`
        """
        return(asdict(self))