function debugLog(...xs) {
  console.log(...xs)
}
function base64ToByteArray(base64) {
  var raw = window.atob(base64);
  var rawLength = raw.length;
  var array = new Uint8Array(new ArrayBuffer(rawLength));
  for(i = 0; i < rawLength; i++) {
    array[i] = raw.charCodeAt(i);
  }
  return array;
}

function zipToString(data, callback) {
  var array = base64ToByteArray(data);
  LZMA.decompress(array, function(result, error) {
    if (!(typeof result === 'string')) result = new Uint8Array(result)
    if (error) console.error(error);
    callback(result);
  });
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
      debugLog("decoding", {encoded});
      zipToString(encoded, function(plaintext) {
          app.ports.decodedPort.send({plaintext, encoded});
          debugLog("decoded", {plaintext});
      });
    });
    app.ports.encodePort.subscribe(function(plaintext) {
      LZMA.compress(plaintext, 9, function(lzmaEncoded, error) {
        if (error) console.error(error);
        var encoded = btoa(String.fromCharCode.apply(null, new Uint8Array(lzmaEncoded)));
        app.ports.encodedPort.send({plaintext, encoded});
        debugLog("compressed", {encoded});
      });
    })

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
