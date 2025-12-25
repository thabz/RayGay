# RayGay Blog

Dette er en Jekyll-baseret blog skrevet mens jeg udvikler min raytracer RayGay. Bloggen er desværre skrevet på dansk, men billederne er smukke på alle sprog, så nyd dem!

## Forudsætninger

- Ruby (version 2.7 eller nyere)
- Bundler

## Installation

1. Naviger til site-mappen:
```bash
cd site
```

2. Installer gem-afhængigheder:
```bash
bundle install
```

## Byg og kør sitet

### Lokal udvikling (anbefalet)

For at bygge sitet og køre det lokalt:

```bash
bundle exec jekyll serve
```

Sitet vil derefter være tilgængeligt på `http://127.0.0.1:4000/`

Jekyll vil automatisk regenerere sitet når du ændrer filer.

### Kun byg

Hvis du kun ønsker at bygge sitet uden at køre serveren:

```bash
bundle exec jekyll build
```

## Output

Det færdige, byggede site ligger i mappen:
```
site/_site/
```

Alle HTML-filer, CSS og andre statiske ressourcer er placeret her efter at have kørt `jekyll build` eller `jekyll serve`.

## Struktur

- `_posts/` - Blog-indlæg
- `_layouts/` - Side-skabeloner
- `_includes/` - Genbrugelige template-fragmenter
- `_sass/` - Stylesheet-kilder (SCSS)
- `css/` - Kompilerede stylesheets
- `assets/` - Billeder og andre mediaressourcer
- `_config.yml` - Jekyll-konfiguration

## Licens

Se LICENSE-filen i repository-roden.
