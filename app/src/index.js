require('../styles/main.scss');

// inject bundled Elm app into div#main
var App = require('./elm/App').App;
App.embed(document.getElementById('content'), require('./elm/App.scss'));
