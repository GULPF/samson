# Package

version       = "0.1.0"
author        = "Oscar Nihlgård"
description   = "JSON & JSON5 implementation"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 0.20.0"

task test, "Run tests":
  exec "nim c -d:json5InternalTesting -r tests/testrunner.nim"
  rmFile "tests/testrunner"

task unicode, "Generate Unicode ranges":
  exec "nim c -r unicode_data/generate '" &
    thisDir() & "/src/samson/private/generated/unicode_data.nim'"
  rmFile "unicode_data/generate"

task docs, "Generate docs":
  exec "nim doc -o:docs/samson.html src/samson"
  exec "nim doc -o:docs/pragmas.html src/samson/pragmas"
  exec "nim doc -o:docs/errors.html src/samson/errors"