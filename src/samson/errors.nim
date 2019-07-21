type
    JsonError* = object of CatchableError ## \
        ## Root error for other error types in the libary.

    JsonParseError* = object of JsonError ## \
        ## Raised when parsing fails.

    JsonSchemaError* = object of JsonError ## \
        ## Raised when converting to a user specified type fails.