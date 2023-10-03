

let first = ($tvar_α$Num,$tvar_β$Functor) => (xs) => (() => {let add = (f) => map($tvar_β$Functor)(f)(xs);
return add((x) => (() => {return Core.add($tvar_α$Num)(x)(2)})())})()

let sum = ($tvar_α$Num) => Core.add($tvar_α$Num)(1)(2)

let test = ($tvar_α$Num) => map({map:$list_$functor_$map})((x) => Core.add($tvar_α$Num)(x)(1))([1,2,3])