let Either = ({ Error: (_0) => ({ tag: "Error", values: [_0] }), Result: (_0) => ({ tag: "Result", values: [_0] }) })
let Error = Either.Error
let Result = Either.Result


let x = ($tvar_β$IsString) => Result("String")