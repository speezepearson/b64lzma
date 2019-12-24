window.B64Lzma = (function() {
    function base64ToByteArray(base64) {
      var raw = window.atob(base64);
      var rawLength = raw.length;
      var array = new Uint8Array(new ArrayBuffer(rawLength));
      for(i = 0; i < rawLength; i++) {
        array[i] = raw.charCodeAt(i);
      }
      return array;
    }

    function b64lzmaDecode(encoded, callback) {
        var array
        try {
            var array = base64ToByteArray(encoded);
        } catch (e) {
            callback(null, "invalid base64");
            return;
        }
        LZMA.decompress(array, function(result, error) {
          if (!(typeof result === 'string')) result = new Uint8Array(result)
          callback(result, error);
        });
    }

    function b64lzmaEncode(plaintext, callback) {
        LZMA.compress(plaintext, 9, function(lzmaEncoded, error) {
          var encoded = btoa(String.fromCharCode.apply(null, new Uint8Array(lzmaEncoded)));
          callback(encoded, error);
        });
    }

    return {
        b64lzmaDecode,
        b64lzmaEncode,
    }
})()
