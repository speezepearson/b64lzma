b64lzma
=======

A shameless rip-off of <http://itty.bitty.site>, implemented as a single HTML file.



"Your page has evaporated, so I can't get the content out of my URL!"
---------------------------------------------------------------------
No worries! Just take the payload ("XQAA..." out of the URL, base64-decode it, and LZMA-decode that! You can do that by pasting the URL through one of the following command-lines:
- Linux: `sed -e 's .*#[^/]*/?*  ' | base64 -d | lzma -d`
- OS X: `sed -e 's .*#[^/]*/?*  ' | base64 -D | lzma -d`


Development
-----------

- Requires Elm 0.19 or later.
- To build the page: `make bundle`.
- To serve it up: `(cd dist/ && python3 -m http.server --bind 127.0.0.1 34348) & firefox http://localhost:34348/`
- To set up a daemon: `watchman trigger -- . build 'src/**/*' 'lib/**/*' -- bash -c '(make bundle; chime-success test $? = 0) > .watchman.log 2>&1'`
