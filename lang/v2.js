($tvar_α$Num, $tvar_β$Functor) => (xs) => (() => {
    let add = (f) => map($tvar_β$Functor)(f)(xs);
    return add((x) => (() => { return Core.add($tvar_α$Num)(x)(2) })())
})()