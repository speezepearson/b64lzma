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
        var array = base64ToByteArray(encoded);
        LZMA.decompress(array, function(result, error) {
          if (!(typeof result === 'string')) result = new Uint8Array(result)
          if (error) console.error(error);
          callback(result);
        });
    }

    function b64lzmaEncode(plaintext, callback) {
        LZMA.compress(plaintext, 9, function(lzmaEncoded, error) {
          if (error) console.error(error);
          var encoded = btoa(String.fromCharCode.apply(null, new Uint8Array(lzmaEncoded)));
          callback(encoded);
        });
    }

    return {
        b64lzmaDecode,
        b64lzmaEncode,
    }
})()
