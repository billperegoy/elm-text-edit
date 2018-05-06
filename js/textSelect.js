var elmDiv = document.getElementById('elm-app');
var app = Elm.Main.embed(elmDiv);

app.ports.processSelection.subscribe(function(word) {
  var text = window.getSelection().toString();
  setTimeout(function() {
    app.ports.selectionResponse.send(text);
  }, 2000);
});
