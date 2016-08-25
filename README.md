# Daily Highlights

Write down daily highlights and save them in the browser's local storage.

### Description

This is a simple Elm application allowing to save some highlights from today,
thoughts, results which are worth noting and other interesting stuff.

First, proof-of-concept version, allows saving these in the browser's local storage.
Saving and fetching stuff is implemented as separate module to easily replace storage facility with something else.

[Demo on Heroku] (https://elm-daily-highlights.herokuapp.com)

### Requirements

* Elm 0.17.1-0.18.0

### Development

```
npm run dev
```

Application will be accessible at http://localhost:8080. Code changes will be automatically reloaded.

### Production

```
npm run build
```

Check then the `dist` folder.

### Credits

Peter Morawiec, for [elm-webpack-starter](https://github.com/moarwick/elm-webpack-starter).

### License

MIT