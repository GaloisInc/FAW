# HTTP Request FAW Distribution

For comparing parsers of HTTP requests.

## Artifact types

One artifact type is used in this distribution:

### `jsonRequests`

Each parser's artifact dir should contain a single file with a
`.json` extension, containing a serialization of requests parsed by
the parser.

The file must contain an Array of Objects, each representing a
request that the parser tried to parse.

#### Request schema

| Key | Type (Encoding) | | Required? | Meaning |
| --- | --- | --- | --- |
| `error` | Boolean | yes | `true` if the parser attempted to parse this request and failed. |
| `method` | String (base64-encoded octets) | if `error` == `false` | HTTP method, e.g. `GET`. Base64-encoded for safety (should be ASCII for a correct parser). |
| `path` | String (base64-encoded octets) | if `error` == `false` | Path to resource, e.g. `/index.html`. Base64-encoded for safety (should be ASCII for a correct parser). |
| `version` | String (base64-encoded octets) | if `error` == `false` | HTTP version, e.g. `HTTP/1.1`.Base64-encoded for safety (should be ASCII for a correct parser). |
| `headers` | Array of 2-element Arrays of String (base64-encoded octets) | if `error` == `false` | (Field name, field value) pairs. Repeated headers should not be collapsed unless that is the default behavior of the parser. Base64-encoded for safety (field names, but not values, should be ASCII for a correct parser). |
| `bodyError` | Boolean | if `error` == `false` | `true` if the parser expected a body and could not parse it. If no body is expected, should be `false`. |
| `body` | String (base64-encoded arbitrary octets) | if `error` == `false` and `bodyError` == `false` | Request body, if expected. |

#### Example

```json
[
  {
    "error": false,
    "method": "GET",
    "path": "/",
    "version": 1.1,
    "headers": [
      [
        "Host",
        "d3d3LmV4YW1wbGUuY29t"
      ],
      [
        "User-Agent",
        "TW96aWxsYS81LjA="
      ],
      [
        "Accept",
        "dGV4dC9odG1sLGFwcGxpY2F0aW9uL3hodG1sK3htbCxhcHBsaWNhdGlvbi94bWw7cT0wLjksaW1hZ2UvYXZpZixpbWFnZS93ZWJwLCovKjtxPTAuOA=="
      ],
      [
        "Accept-Language",
        "ZW4tR0IsZW47cT0wLjU="
      ],
      [
        "Accept-Encoding",
        "Z3ppcCwgZGVmbGF0ZSwgYnI="
      ],
      [
        "Connection",
        "a2VlcC1hbGl2ZQ=="
      ],
      [
        "Transfer-Encoding",
        "Y2h1bmtlZA=="
      ]
    ],
    "body": "",
    "bodyError": false
  },
  {
    "error": false,
    "method": "GET",
    "path": "/",
    "version": 1.1,
    "headers": [],
    "bodyError": false
  },
  {
    "error": true,
  }
]
```
