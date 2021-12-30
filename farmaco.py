from dataclasses import dataclass, asdict
import re
from utils import curl
import fitz

@dataclass(unsafe_hash=True)
class Farmaco:
    """
    Classe che rappresenta un farmaco estratto dal sito [AIFA](https://farmaci.agenziafarmaco.gov.it/bancadatifarmaci/).
    """
    codice_farmaco: str
    descrizione_farmaco: str
    ditta: str
    url_fi: str
    principio_attivo: str
    fi: str = ''

    def fromJSON_AIFA(sm_field_codice_farmaco,
        sm_field_descrizione_farmaco,
        sm_field_descrizione_ditta,
        sm_field_link_fi=[''],
        sm_field_descrizione_atc=['']):
        """
        Crea un oggetto `Farmaco` dal JSON preso dal sito [AIFA](https://farmaci.agenziafarmaco.gov.it/bancadatifarmaci/)
        """
        return Farmaco(sm_field_codice_farmaco[0],
            sm_field_descrizione_farmaco[0],
            sm_field_descrizione_ditta[0],
            re.sub("&amp;", "&", sm_field_link_fi[0]),
            sm_field_descrizione_atc[0])
    
    def asDict(self):
        """
        Resituisce l'oggetto `Farmaco` in formato `dict`
        """
        return(asdict(self))

    def fromJSON(codice_farmaco, descrizione_farmaco, ditta, url_fi, principio_attivo, fi=''):
        """
        Crea un oggetto `Farmaco` da un JSON
        """
        return Farmaco(codice_farmaco, descrizione_farmaco, ditta, url_fi, principio_attivo, fi)
    
    def estrai_pdf(self):
        """
        Estrae tutte le informazioni dal foglio illustrativo 
        """
        pdf = curl(self.url_fi)
        try:
            doc = fitz.open("pdf", stream=pdf)
            pages = ""
            for page in doc:
                pages += page.get_text()
            self.fi = pages
        # si ha un RuntimeError quando il PDF non è valido
        except RuntimeError:
            self.fi = ''

    def rimuoviTesto(self):
        """
        Rimuove il testo indesiderato estratto dal PDF del foglio illustrativo.
        """
        self.fi = re.sub(r"\n", " ", self.fi)
        self.fi = re.sub(r"\r", "", self.fi)
        self.fi = re.sub(r"\t", "", self.fi)
        s1 = re.sub(r"(?i).*1\.\ *c", "1. c", self.fi)
        # prendo solo i fogli illustrativi che seguono un certo schema
        if self.fi == s1:
            self.fi = ''
        else:
            self.fi = s1
            self.fi = re.sub("-", "", self.fi)
            self.fi = re.sub("Documento reso disponibile da AIFA il [0-9]{2}\/[0-9]{2}\/[0-9]{4} Esula dalla competenza dell’AIFA ogni eventuale disputa concernente i diritti di proprietà industriale e la tutela brevettuale dei dati relativi all’AIC dei  medicinali e, pertanto, l’Agenzia non può essere ritenuta responsabile in alcun modo di eventuali violazioni da parte del titolare dell'autorizzazione  all'immissione in commercio \(o titolare AIC\)\.", "", self.fi)
            self.fi = re.sub("titolare dell’autorizzazione.*", "", self.fi)
            self.fi = self.fi.replace("'"," ")

    def estrai_info(self):
        """
        Estrae il primo punto del foglio illustrativo, quello relativo alle malattie/patologie da curare
        """
        try:
            self.fi = re.findall(r"(?i).*2\.", self.fi)[0]
        # se la regex non trova risultati, il foglio illustrativo non è del formato giusto
        except IndexError:
            self.fi = ''
            print(f"Errore: {self.codice_farmaco}")

    def toProlog(self):
        """
        Stampa la rappresentazione in prolog del farmaco
        """
        return f"farmaco('{self.descrizione_farmaco}', '{self.ditta}', '{self.principio_attivo}', {str([e for e in self.fi.split()])})."