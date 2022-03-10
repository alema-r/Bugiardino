from pyswip import Prolog
from tkinter import *
from tkinter import font


def prolog_init(prolog_obj):
    """
    Consulta il file prolog/main.pl e avvia la query `inizializza()`
    """
    prolog_obj.consult("prolog/main.pl")
    _ = list(prolog_obj.query("inizializza()"))


class PrologUI:
    """
    classe per gestire l'interfaccia grafica e il collegamento al Prolog.
    """

    def __init__(self, prolog, root):
        self.prolog = prolog
        self.root = root
        self.query = StringVar()
        self.num_results = StringVar()
        self.result_list = None
        self.text_box = None
        self.tk_init(root)
        self.result = {}

    def tk_init(self, root):
        """
        Definisce la finestra per l'interfaccia grafica realizzata con Tkinter
        """
        root.title("Bugiardino")
        root.geometry("1920x980")
        root.configure(bg="#8BA5A5")

        mainframe = Frame(master=root, bg="#8BA5A5")
        mainframe.grid(column=0, row=0, sticky=(N, W, E, S))
        root.columnconfigure(0, weight=1)
        root.rowconfigure(0, weight=1)

        Label(mainframe, textvariable=self.num_results, bg="#8BA5A5").place(
            x=450.0, y=40.0, width=200, height=50
        )

        query_entry = Entry(mainframe, width=20, bg="#DAE2E2", textvariable=self.query)
        query_entry.place(x=52.0, y=50.0, width=214.0, height=29.0)

        Button(
            mainframe, text="Cerca", bg="#364141", fg="#FFFFFF", command=self.search
        ).place(x=312.0, y=50.0, width=96.0, height=31.0)

        my_font = font.Font(family="Noto Sans")
        self.result_list = Listbox(mainframe, bg="#DAE2E2", font=my_font)
        self.result_list.place(
            x=52.0, y=106.0, width=350.0, height=980.0 - 106.0 - 54.0
        )
        self.result_list.bind("<<ListboxSelect>>", self.current_selection)

        self.text_box = Text(mainframe, bg="#DAE2E2", font=f)
        self.text_box.place(
            x=454.0, y=106.0, width=1920.0 - 454.0 - 52.0, height=980.0 - 106.0 - 54.0
        )

        self.text_box["state"] = "disabled"

        query_entry.focus()
        root.bind("<Return>", self.search)

    def prolog_cerca_farmaco(self, query):
        """
        Avvia la query `cerca_farmaco` o `cerca_farmaco2` in base al numero di parole ricercate
        """
        query = query.split()
        if len(query) == 1:
            prolog_query = f"cerca_farmaco2({query[0]}, Nome, Frase)"
        else:
            prolog_query = f"cerca_farmaco({query}, Nome, Frase, _)"
        res = list(self.prolog.query(prolog_query))
        self.result = {}
        res = sorted(res, key=lambda x: x["Nome"])
        for e in res:
            if e["Nome"] not in self.result:
                self.result[e["Nome"]] = []
            self.result[e["Nome"]].append(e["Frase"])

    def prolog_info_farmaco(self, nome):
        """
        Restituisce le informazioni riguardanti la casa farmaceutica e il
        principio attivo di un determinato farmaco
        `nome`: nome del farmaco
        """
        query_farmaco = f"farmaco('{nome}', CasaFarm, Principio, _)"
        res = list(self.prolog.query(query_farmaco))
        return res[0]["CasaFarm"].replace("_", " "), res[0]["Principio"].replace(
            "_", " "
        )

    def search(self, *args):
        """
        Avvia la ricerca dei farmaci e popola la lista dei risultati
        """
        value = self.query.get()
        if value != "":
            self.result_list.delete(0, END)
            self.prolog_cerca_farmaco(value)
            self.num_results.set(f"{len(self.result)} farmaci trovati")
            for k in self.result:
                self.result_list.insert(END, k.replace("_", " "))

    def current_selection(self, evt):
        """
        In base al farmaco selezionato dalla lista dei risultati, popola il
        box con le frasi trovate dalla query prolog
        """
        value = str(self.result_list.get(ANCHOR)).replace(" ", "_")
        self.text_box["state"] = "normal"
        self.text_box.delete(1.0, END)
        casa_farmaceutica, principio_attivo = self.prolog_info_farmaco(value)
        info_farm = f"Nome farmaco:\t\t\t{value.replace('_', ' ')}\n\
            Casa farmaceutica:\t\t\t{casa_farmaceutica}\n\
            Principio attivo:\t\t\t{principio_attivo}\n"
        self.text_box.insert(END, info_farm)
        self.text_box.tag_add("info_farmaco", 1.0, 4.0)
        self.text_box.tag_configure(
            "info_farmaco", background="#364141", foreground="#ffffff"
        )
        if len(self.result[value]) == 1:
            self.text_box.insert(END, "Frase estratta dal foglio illustrativo:\n")
            self.text_box.insert(END, self.result[value][0])
        else:
            self.text_box.insert(END, "Frasi estratte dal foglio illustrativo:\n")
            for frase in self.result[value]:
                self.text_box.insert(END, frase)
                self.text_box.insert(END, "\n\n")
        self.text_box.tag_add("frase", 4.0, "4.end")
        self.text_box.tag_config(
            "frase", font=font.Font(family="Noto Sans", weight="bold")
        )
        self.text_box["state"] = "disabled"


tk_root = Tk()
prolog = Prolog()
prolog_init(prolog)
p = PrologUI(prolog, tk_root)
tk_root.mainloop()
