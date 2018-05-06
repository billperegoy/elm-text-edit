const DELAY = 0;

const elmDiv = document.getElementById('elm-app');
const app = Elm.Main.embed(elmDiv);

app.ports.processSelection.subscribe((str) => {
  const selection = window.getSelection();
  const id = selection.anchorNode.parentElement.id;
  const range = selection.getRangeAt(0);
  const text = selection.toString();

  const result = {
    text: text,
    id: id,
    startOffset: range.startOffset,
    endOffset: range.endOffset
  };

  setTimeout(() => {
    app.ports.selectionResponse.send(JSON.stringify(result));
  }, DELAY);
});
