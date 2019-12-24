function debugLog(...xs) {
  console.log(...xs)
}

var ignorePasteClass = "elm-ignore-paste";

function initMain() {
    var app = Elm.Main.init({
      node: document.getElementById('elm'),
      flags: {
        interopConstants: {
          ignorePasteClass: ignorePasteClass,
        },
      },
    });
    app.ports.decodePort.subscribe(function(encoded) {
      B64Lzma.b64lzmaDecode(encoded, (plaintext) => app.ports.decodedPort.send({plaintext, encoded}));
    });
    app.ports.encodePort.subscribe(function(plaintext) {
      B64Lzma.b64lzmaEncode(plaintext, (encoded) => app.ports.encodedPort.send({plaintext, encoded}));
    });

    window.addEventListener('paste', function(event) {
      if (event.target.classList.contains(ignorePasteClass)) {
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
