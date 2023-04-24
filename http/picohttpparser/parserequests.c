#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "picohttpparser.h"


const char *base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";


size_t min(const size_t x, const size_t y) {
    return x < y ? x : y;
}


void fprint_base64(FILE *fd, const char *buffer, const size_t len) {
    // Does not insert newlines every 76 chars like the spec specifies
    const char *triple;
    const char *end = buffer + len;
    int remaining_octets;
    unsigned char octets[3];
    unsigned char base64_buffer[4];
    for (triple = buffer; triple < end; triple += 3) {
        remaining_octets = end - triple;
        octets[0] = (unsigned char)triple[0];
        octets[1] = remaining_octets > 1 ? (unsigned char)triple[1] : 0;
        octets[2] = remaining_octets > 2 ? (unsigned char)triple[2] : 0;
        base64_buffer[0] = base64_chars[octets[0] >> 2];
        base64_buffer[1] = base64_chars[((octets[0] & 0b11) << 4) | (octets[1] >> 4)];
        base64_buffer[2] = remaining_octets > 1 ? base64_chars[((octets[1] & 0b1111) << 2) | (octets[2] >> 6)] : '=';
        base64_buffer[3] = remaining_octets > 2 ? base64_chars[octets[2] & 0b111111] : '=';
        fprintf(fd, "%.*s", 4, base64_buffer);
    }
}


// Returns 0 if all parses were successful, -1 otherwise
int parse_requests(char *buffer, size_t buffer_len, FILE *fd_out) {
    printf("Total stream length: %ld\n", buffer_len);

    // Set by phr_parse_request
    const char *method; // pointer to HTTP method within buffer
    size_t method_len;
    const char *path; // pointer to HTTP path (e.g. "/") within buffer
    size_t path_len;
    int minor_version; // Minor version of HTTP1 protocol
    const size_t max_headers = 100; // Could set dynamically for extra safety
    struct phr_header headers[max_headers];
    size_t num_headers;
    size_t last_len = 0; // Used when retrying on partially parsed content (N/A for our use case)

    int parse_error = 0;

    struct phr_header *transfer_encoding_header;
    struct phr_header *content_length_header;
    const char *body;
    size_t body_len;
    int body_parse_error = 0;

    int i;
    int first_request = 1;

    if (fd_out) {
        fprintf(fd_out, "[\n");
    }

    while (buffer_len) {
        // https://github.com/h2o/h2o/blob/13ba727ad12dfb2338165d2bcfb2136457e33c8a/lib/http1.c#L624
        num_headers = max_headers;
        const int request_len = phr_parse_request(
                buffer,
                buffer_len,
                &method,
                &method_len,
                &path,
                &path_len,
                &minor_version,
                headers,
                &num_headers,
                last_len);

        if (request_len == -1) {
            fprintf(stderr, "Error: Failed to parse request\n");
            parse_error = 1;
        } else if (request_len == -2) {
            // Usually this would be repeated once more data arrives, but we already have all data
            fprintf(stderr, "Error: Request was incomplete\n");
            parse_error = 1;
        }
        if (parse_error && fd_out) {
            if (!first_request) {
                fprintf(fd_out, ",\n");
            }
            fprintf(fd_out, "{\"error\": true}\n");
        }
        if (parse_error) {
            break;
        }

        // Find body length; stdout logging
        printf("Consumed %d bytes before body\n", request_len);
        printf("Method is %.*s\n", (int)method_len, method);
        printf("Path is %.*s\n", (int)path_len, path);
        printf("HTTP version is 1.%d\n", minor_version);
        printf("Headers:\n");
        content_length_header = NULL;
        transfer_encoding_header = NULL;
        for (i = 0; i != num_headers; ++i) {
            printf("  %.*s: %.*s\n", (int)headers[i].name_len, headers[i].name,
                (int)headers[i].value_len, headers[i].value);
            if (strncmp("Content-Length", headers[i].name, min(headers[i].name_len, 14)) == 0) {
                content_length_header = &headers[i];
            } else if (strncmp("Transfer-Encoding", headers[i].name, min(headers[i].name_len, 17)) == 0) {
                // This field can be parsed as a comma-separated list if
                // defined multiple times, but we only need the last
                // encoding listed (if it's chunked, we need to parse the
                // body; if not, the request is invalid).
                transfer_encoding_header = &headers[i];
            }
        }

        buffer += request_len;
        buffer_len -= request_len;

        // Consume body
        body = buffer;
        body_len = 0;
        // Note: TRACE should never have a body, but a server in practice
        // will probably read the body and discard it if there is one
        if (transfer_encoding_header) {
            // Transfer-Encoding takes precedence over Content-Length
            /*
                https://datatracker.ietf.org/doc/html/rfc9112#name-message-body-length

                If a Transfer-Encoding header field is present and the chunked
                transfer coding (Section 7.1) is the final encoding, the message
                body length is determined by reading and decoding the chunked data
                until the transfer coding indicates the data is complete.

                If a Transfer-Encoding header field is present in a response and
                the chunked transfer coding is not the final encoding, the message
                body length is determined by reading the connection until it is
                closed by the server.

                If a Transfer-Encoding header field is present in a request and the
                chunked transfer coding is not the final encoding, the message body
                length cannot be determined reliably; the server MUST respond with
                the 400 (Bad Request) status code and then close the connection.
             */
            const char *final_transfer_encoding;
            // Iterate backwards from the end of the field value to get the last word.
            // picohttpparser strips trailing whitespace already.
            for (final_transfer_encoding = transfer_encoding_header->value
                        + transfer_encoding_header->value_len - 1;
                    final_transfer_encoding > transfer_encoding_header->value;
                    --final_transfer_encoding) {
                if (*final_transfer_encoding == ' ' || *final_transfer_encoding == ',') {
                    ++final_transfer_encoding;
                    break;
                }
            }
            size_t final_transfer_encoding_len = 
                    transfer_encoding_header-> value
                    + transfer_encoding_header->value_len
                    - final_transfer_encoding;
            if (strncmp("chunked", final_transfer_encoding, min(final_transfer_encoding_len, 7)) == 0) {
                struct phr_chunked_decoder decoder = {};
                decoder.consume_trailer = 1; // Could maybe only set if a Trailers header is present,
                                             // but h2o always sets this
                body_len = buffer_len;
                // phr_decode_chunked decodes in-place, and shifts the later
                // content accordingly, leaving garbage at the end of `buffer`
                size_t undecoded_len = phr_decode_chunked(&decoder, buffer, &body_len);
                // body_len now set to length of decoded data, if successful
                if (undecoded_len == -1) {
                    fprintf(stderr, "Error: could not parse chunked data\n");
                    body_len = 0;
                    body_parse_error = 1;
                } else if (undecoded_len == -2) {
                    // Usually this would be repeated once more data arrives, but we already have all data
                    fprintf(stderr, "Error: chunked data incomplete\n");
                    body_len = 0;
                    body_parse_error = 1;
                } else {
                    printf("Body length: %ld\n", body_len);
                    buffer += body_len;
                    buffer_len = undecoded_len;
                }
            } else {
                fprintf(stderr, "Error: Ambiguous body length; defines Transfer-Encoding, but chunked is not the final encoding.");
                body_parse_error = 1;
                // return something once factored out
            }
        } else if (content_length_header) {
            char *content_length_str = malloc(content_length_header->value_len + 1);
            memcpy(content_length_str, content_length_header->value,
                    content_length_header->value_len);
            content_length_str[content_length_header->value_len] = '\0';
            errno = 0;
            body_len = strtoull(content_length_str, NULL, 10);
            free(content_length_str);
            if (errno == ERANGE) {
                fprintf(stderr, "Error: Invalid Content-Length: %.*s\n",
                        (int)content_length_header->value_len,
                        content_length_header->value);
                body_parse_error = 1;
                // return something once factored out
            } else {
                printf("Body length: %ld\n", body_len);
                // set start, end body pointers here for saving to JSON?
                if (body_len > buffer_len) {
                    fprintf(stderr, "Error: Content-Length exceeds remaining stream size");
                    body_parse_error = 1;
                } else {
                    buffer += body_len;
                    buffer_len -= body_len;
                }
            }
        } else {
            printf("No message body\n");
        }
        printf("\n");

        // JSON output
        if (fd_out) {
            // TODO maybe switch to a library for JSON serialization?
            // TODO b64encode all strings
            if (!first_request) {
                fprintf(fd_out, ",\n");
            }
            fprintf(fd_out, "{\n\"error\": false,\n");
            // TODO need to escape these, or maybe even base64-encode them?
            // Not sure how picohttp handles non-ascii (it has tests for some but not all cases)
            fprintf(fd_out, "\"method\": \"%.*s\",\n", (int)method_len, method);
            fprintf(fd_out, "\"path\": \"%.*s\",\n", (int)path_len, path);
            fprintf(fd_out, "\"version\": \"HTTP/1.%d\",\n", minor_version);
            fprintf(fd_out, "\"headers\": [\n");
            for (i = 0; i != num_headers; ++i) {
                // Header names _should_ be ascii--picohttpparser checks for this
                // Likely still want to escape them though
                fprintf(fd_out, "  [\"%.*s\", \"", (int)headers[i].name_len, headers[i].name);
                fprint_base64(fd_out, headers[i].value, headers[i].value_len);
                fprintf(fd_out, i + 1 != num_headers ? "\"],\n" : "\"]\n"); // no trailing LF
            }
            fprintf(fd_out, "],\n");
            // Header fields (and method) define if there's a body. Body len may be 0.
            if ((content_length_header || transfer_encoding_header) && !body_parse_error) {
                fprintf(fd_out, "\"bodyError\": false,\n");
                fprintf(fd_out, "\"body\": \"");
                fprint_base64(fd_out, body, body_len);
                fprintf(fd_out, "\"\n}");
            } else if (body_parse_error) {
                fprintf(fd_out, "\"bodyError\": true\n}");
            } else {
                fprintf(fd_out, "\"bodyError\": false\n}");
            }
        }
        if (body_parse_error) {
            break;
        }
        first_request = 0;
    }
    if (fd_out) {
        fprintf(fd_out, "\n]\n");
    }
    return (parse_error || body_parse_error) ? 1 : 0;
}

int main(int argc, char *argv[]) {
    if (argc != 2 && argc != 3) {
        fprintf(stderr, "Error: Wrong number of arguments (expected 1 or 2; was %d)\n", argc - 1);
        return 1;
    }
    FILE *fd_out = NULL;
    FILE *fd = fopen(argv[1], "rb");
    if (!fd) {
        fprintf(stderr, "Error: Could not open %s\n", argv[1]);
        return 1;
    }
    fseek(fd, 0, SEEK_END);
    size_t len = ftell(fd);
    if (len == -1) {
        fprintf(stderr, "Error: File not seekable\n");
        return 1;
    }
    fseek(fd, 0, SEEK_SET);
    char *buffer = malloc(len + 1); // leave space for an extra \0 just in case
    if (!buffer) {
        fprintf(stderr, "Error: Could not allocate enough memory to read file\n");
        return 1;
    }
    if (buffer) {
        fread(buffer, 1, len, fd);
        buffer[len] = '\0';
    }
    fclose(fd);

    if (argc == 3) {
        fd_out = fopen(argv[2], "w");
        if (!fd_out) {
            fprintf(stderr, "Error: Could not open %s for writing\n", argv[2]);
            return 1;
        }
    }

    int status = parse_requests(buffer, len, fd_out);

    if (fd_out) {
        fclose(fd_out);
    }
    free(buffer);
    return status;
}
