# Sito statico del CV

Esegui `Rscript scripts/render_all.R --format both`. La directory da
pubblicare è `dist/html`:

- `index.html` è il CV completo in inglese;
- le quattro versioni HTML sono collegate dall'intestazione;
- i PDF corrispondenti sono in `pdf/` e ogni pagina linka al proprio.

Qualsiasi hosting statico può quindi pubblicare direttamente `dist/html`
(per esempio come directory di publish di GitHub Pages, Netlify o Cloudflare
Pages). Non servono server, API o routing lato backend.
