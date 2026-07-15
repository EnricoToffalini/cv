# Badge open science per le pubblicazioni

Ogni record in `publications.bib` può usare due campi opzionali. Non sono
necessari finché un prodotto non ha materiale open da segnalare.

```bibtex
opendata = {true},
dataurl = {https://osf.io/xxxx},
opencode = {true},
codeurl = {https://github.com/user/repo},
preregistered = {true},
preregistrationurl = {https://osf.io/registrations}
```

Questa è la stessa struttura di `feraco`: quattro flag booleani indipendenti
(`preregistered`, `opendata`, `openmaterials`, `opencode`), ciascuno con la
propria URL facoltativa (`preregistrationurl`, `dataurl`, `materialsurl`,
`codeurl`). Il renderer mostra badge compatti e cliccabili in HTML e
collegamenti equivalenti nel PDF; la validazione blocca URL senza badge attivo
o URL non validi.
