

const f
    : <A>(x: A) => number
    = x => x 

const fn
    : <A>(x: A) => [A, number]
    = x => [x, 1]


const foo = fn(1)

const fn2 = fn<string>

// data Either 
//     : Type -> Type -> Type
//     = Left: forall a. e -> Either <e a>
//     | Right: forall e. a -> Either <e a>


type Either<E, A>
    = { _tag: "Left", e: E } 
    | { _tag: "Right", a: A }

const Left = <E, A>(e: E): Either<E,A> => ({ _tag: "Left", e })
const Right = <E, A>(a: A): Either<E,A> => ({ _tag: "Right", a })