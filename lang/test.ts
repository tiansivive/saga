

const f
    : <A>(x: A) => number
    = x => x 

const fn
    : <A>(x: A) => [A, number]
    = x => [x, 1]


const foo = fn(1)

const fn2 = fn<string>