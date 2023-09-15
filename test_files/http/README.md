# HTTP Request Test Files

## Format

Streams of HTTP requests. Multiple requests may be included in a
single file, concatenated; these will be sent together to the
parser, as in HTTP/1.1 pipelining.

As always, valid and invalid request streams should be included in
the test files.

## Directories

* `cves`: HTTP requests exemplifying particular vulverabilities.
* `handcrafted`: Handcrafted HTTP requests.
* `portswigger`: Requests from [this portswigger article](https://portswigger.net/research/browser-powered-desync-attacks).
