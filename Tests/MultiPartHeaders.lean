import LeanIO.Request.MultiPartForm.Headers
open LeanIO
open Std.Http

-- parseParamValue: unquoted
#guard parseParamValue "abc" = some "abc"
#guard (parseParamValue "abc;" |>.get!) = "abc"
#guard (parseParamValue "abc def" |>.get!) = "abc"
#guard (parseParamValue "abc,def" |>.get!) = "abc"

-- parseParamValue: quoted
#guard (parseParamValue "\"abc\"" |>.get!) = "abc"
#guard parseParamValue "\"\"" = some ""
#guard (parseParamValue "\"abc\" " |>.get!) = "abc"

-- parseParamValue: escaped quote inside quoted
#guard (parseParamValue "\"ab\\\"c\"" |>.get!) = "ab\"c"
#guard (parseParamValue "\"\\\"\"" |>.get!) = "\""
#guard (parseParamValue "\"\\\"abc\"" |>.get!) = "\"abc"

-- parseParamValue: unterminated quote
#guard parseParamValue "\"abc" = none

-- extractParamValue: unquoted
#guard (extractParam "multipart/form-data; boundary=abc" "boundary" |>.get!) = "abc"
#guard (extractParam "multipart/form-data; boundary=abc-def" "boundary" |>.get!) = "abc-def"

-- extractParamValue: quoted
#guard (extractParam "multipart/form-data; boundary=\"abc\"" "boundary" |>.get!) = "abc"
#guard (extractParam "multipart/form-data; charset=utf-8; boundary=\"my--boundary\"" "boundary" |>.get!) = "my--boundary"
#guard (extractParam "multipart/form-data; boundary=\"a\\\"bc\"" "boundary" |>.get!) = "a\"bc"

-- extractParamValue: with spaces
#guard (extractParam "multipart/form-data; boundary=abc " "boundary" |>.get!) = "abc"
#guard (extractParam "multipart/form-data;   boundary=abc" "boundary" |>.get!) = "abc"

-- extractParamValue: empty value rejected
#guard extractParam "multipart/form-data; boundary=" "boundary" = none
#guard extractParam "multipart/form-data; boundary=\"\"" "boundary" = none

-- parseOneHeader: basic
#guard parseOneHeader "Content-Type: text/plain" = some (.contentType, Header.Value.mk "text/plain")

-- parseOneHeader: colon in value
#guard (parseOneHeader "Content-Type: text/plain; charset=utf-8" |>.map (·.2.value)) = some "text/plain; charset=utf-8"

-- parseOneHeader: no colon
#guard parseOneHeader "no colon here" = none

-- parseHeaders: single header
#guard (parseHeaders ("Content-Type: text/plain".toUTF8) |>.bind (·.get? .contentType) |>.map (·.value)) = some "text/plain"

-- parseHeaders: two headers
#guard (parseHeaders ("Content-Type: text/plain\r\nContent-Length: 42".toUTF8) |>.bind (·.get? .contentType) |>.map (·.value)) = some "text/plain"

-- parseHeaders: empty
#guard (parseHeaders ("".toUTF8) |>.get! |>.isEmpty)

-- parseHeaders: invalid header line returns none
#guard parseHeaders ("bad header".toUTF8) = none

-- nameParam
#guard (nameParam ((∅ : Headers).insert! "Content-Disposition" "form-data; name=\"myfield\"") |>.get!) = "myfield"

-- nameParam: empty rejected
#guard nameParam ((∅ : Headers).insert! "Content-Disposition" "form-data; name=") = none

-- filenameParam
#guard (filenameParam ((∅ : Headers).insert! "Content-Disposition" "form-data; name=\"f\"; filename=\"hello.txt\"") |>.get!) = "hello.txt"

-- filenameParam: empty rejected
#guard filenameParam ((∅ : Headers).insert! "Content-Disposition" "form-data; name=\"f\"; filename=") = none

-- headerContentType
#guard headerContentType ((∅ : Headers).insert! "Content-Type" "image/png") = "image/png"
#guard headerContentType (∅ : Headers) = "text/plain"
