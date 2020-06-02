# Changelog for pdf-etl-tools
## New in 0.4

 - we now truncate these fields to 1M
   - "result.stdoutRes"
   - "result.stderrRes"

 - A line is appended to these text fields when we need to truncate:

        "\nERROR: DATA-TRUNCATED\n"

 - the schema has been extended so that the above fields are not just present when

        {"result._cons" : "GoodResult"}

   but also they are present when

        {"result._cons" : "Timeout"}

## New in 0.3

 - fields dropped from schema (ones that shouldn't have been in collection in
   first place).

        "invoker.exec": ...          // this internal data to pdf-etl-tool
        "invoker._cons": "Invoker"   // these all had same value.
        "_cons" : "Invocation"       // these all had same value

 - invokers now referred to by name (string)
 - invokers now defined in external file: invokers.cfg
 - new timeout override feature

     -t,--timeout (SECONDS|no-timeout|no-override)

## New in 0.2

 - 0.2 was used for the December demo.
 - Lots of stuff :-)

## 0.1

 - ancient history!
