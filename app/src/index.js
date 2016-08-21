require('../styles/main.scss');

// inject bundled Elm app into div#main
const {App} = require('./elm/App');
App.embed(document.getElementById('content'));
