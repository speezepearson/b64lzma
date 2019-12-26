function debugLog(...xs) {
  console.log(...xs)
}

var capturePasteClass = "elm-capture-paste";
var initAutofocusId = "landing-paste-prompt";

function initMain() {
    var app = Elm.Main.init({
      node: document.getElementById('elm'),
      flags: {
        interopConstants: {
          capturePasteClass: capturePasteClass,
          initAutofocusId: initAutofocusId,
        },
      },
    });
    var landingPastePrompt = document.getElementById(initAutofocusId);
    if (landingPastePrompt) landingPastePrompt.focus();
    app.ports.decodePort.subscribe(function(encoded) {
      B64Lzma.b64lzmaDecode(encoded, (plaintext, error) => app.ports.decodedPort.send(error ? {error: error.toString()} : {plaintext, encoded}));
    });
    app.ports.encodePort.subscribe(function(plaintext, error) {
      B64Lzma.b64lzmaEncode(plaintext, (encoded, error) => app.ports.encodedPort.send(error ? {error: error.toString()} : {plaintext, encoded}));
    });

    window.addEventListener('paste', function(event) {
      if (!event.target.classList.contains(capturePasteClass)) {
          return;
      }
      var data = (event.clipboardData || window.clipboardData)
      app.ports.userPastedPort.send({
          html: data.getData('text/html') || null,
          plainText: data.getData("text/plain") || null,
      });
      event.preventDefault();
    });
}

window.addEventListener('load', initMain);
