import csv

from farmaco import Farmaco
"""
Principio Attivo;
Descrizione Gruppo Equivalenza;
Denominazione e Confezione;
Prezzo al pubblico �;
Titolare AIC;
"Codice AIC";
Codice Gruppo Equivalenza;
X=in lista di trasparenza Aifa 17/5/2021;
Solo in lista di Regione:;
Metri cubi ossigeno
"""



# principio_attivo: str
# descrizione_gruppo_equivalenza: str
# confezione: str
# prezzo_pubblico: float
# titolare_AIC: str
# codice_AIC: int
# codice_gruppo_equivalenza: int
farmaci = []
with open('farmaci_classe_A.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=';')
    for row in csv_reader:
       farmaci.append(Farmaco(row[0], row[1], row[2], row[3], row[4], row[5], row[6])) 
    
for i in range(10):
    print(farmaci[i])

"""
https://farmaci.agenziafarmaco.gov.it/bancadatifarmaci/farmaco?farmaco=034208
"""