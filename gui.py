from pyswip import Prolog
from tkinter import *
from tkinter import ttk
from itertools import groupby

class PrologUI:
    def __init__(self, prolog, root):
        self.prolog = prolog
        self.prolog_init(self.prolog)
        self.root = root
        self.query = StringVar()
        self.num_results = StringVar()
        self.result_list = None
        self.textBox = None
        self.tk_init(root)
        self.result = {}

    def prolog_init(self, prolog):
        prolog.consult('prolog/main.pl')
        _ = list(prolog.query('inizializza()'))

    def tk_init(self, root):
        root.title('Bugiardino')
        root.geometry("1080x764")
        root.configure(bg = "#8BA5A5")

        mainframe = Frame(master=root, bg="#8BA5A5")
        mainframe.grid(column=0, row=0, sticky=(N, W, E, S))
        root.columnconfigure(0, weight=1)
        root.rowconfigure(0, weight=1)

        Label(mainframe, textvariable=self.num_results, bg="#8BA5A5").place(x=450.0, y=40.0, width=200, height= 50)

        query_entry = Entry(mainframe, width=20, bg="#DAE2E2", textvariable=self.query)
        query_entry.place(x=52.0, y=50.0, width=214.0, height=29.0)

        Button(mainframe, text="Cerca", bg="#364141", fg="#FFFFFF", command=self.search).place(x=312.0, y=50.0, width=96.0, height=31.0)

        self.result_list = Listbox(mainframe, bg="#DAE2E2")
        self.result_list.place(x=52.0, y=106.0, width=285.0, height=603.0)
        self.result_list.bind('<<ListboxSelect>>',self.current_selection)

        self.textBox = Text(mainframe, bg="#DAE2E2")
        self.textBox.place(x=408.0, y=106.0, width=619.0, height=603.0)

        self.textBox['state'] = 'disabled'

        query_entry.focus()
        root.bind("<Return>", self.search)

    def prolog_query(self, query):
        q = f"cerca_farmaco({query.split(' ')}, Nome, Frase, _)"
        res = list(self.prolog.query(q))
        self.result = {}
        res = sorted(res, key=lambda x:x['Nome'])
        for e in res:
            if e['Nome'] not in self.result.keys():
                self.result[e['Nome']] = []
            self.result[e['Nome']].append(e['Frase'])

    def search(self, *args):
        value = self.query.get()
        if value != '':
            self.result_list.delete(0, END)
            self.prolog_query(value)
            self.num_results.set(f"{len(self.result.keys())} farmaci trovati")
            for k in self.result.keys():
                self.result_list.insert(END, k.replace('_', ' '))

    def current_selection(self, evt):
        value=str(self.result_list.get(ANCHOR)).replace(' ', '_')
        self.textBox['state'] = 'normal'
        self.textBox.delete(1.0,END)
        for e in self.result[value]:
            self.textBox.insert(END, e)
            self.textBox.insert(END, '\n')
        self.textBox['state'] = 'disabled'


root = Tk()
prolog = Prolog()
p = PrologUI(prolog, root)
root.mainloop()

