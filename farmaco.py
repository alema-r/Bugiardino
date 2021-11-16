from dataclasses import dataclass

@dataclass
class Farmaco:
    # def __init__(self, sm_field_descrizione_ditta, 
    #     sm_field_codice_farmaco,
    #     sm_field_descrizione_farmaco,
    #     sm_field_tipo_procedura):

    #     self.ditta = sm_field_descrizione_ditta
    #     self.codice = sm_field_codice_farmaco
    #     self.descrizione = sm_field_descrizione_farmaco
    #     self.tipo_procedura = sm_field_tipo_procedura
    ditta: str
    codice: int
    descrizione: str
    tipo_procedura: str

    def fromJSON(sm_field_descrizione_ditta, 
        sm_field_codice_farmaco,
        sm_field_descrizione_farmaco,
        sm_field_tipo_procedura):

        return Farmaco( sm_field_descrizione_ditta[0], 
        sm_field_codice_farmaco[0],
        sm_field_descrizione_farmaco[0],
        sm_field_tipo_procedura[0])
    