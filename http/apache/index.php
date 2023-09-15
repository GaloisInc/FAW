<?php
    $headers = getallheaders();

    $encoded_headers = array();
    foreach($headers as $field => $value) {
        array_push($encoded_headers, array(base64_encode($field), base64_encode($value)));
    }

    $request = array(
        'error' => false,
        'method' => base64_encode($_SERVER['REQUEST_METHOD']),
        'path' => base64_encode($_SERVER['REQUEST_URI']),
        'version' => base64_encode($_SERVER['SERVER_PROTOCOL']),
        'headers' => $encoded_headers,
        'bodyError' => false,
    );

    $body = file_get_contents('php://input');
    $request['body'] = base64_encode($body);
    file_put_contents(
        '/var/log/apache2-' . $_SERVER['SERVER_PORT'] . '/results.log',
        json_encode($request),
        FILE_APPEND | LOCK_EX,
    );
?>
