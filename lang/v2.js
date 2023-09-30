($tvar_α$Num, $tvar_β$Functor) => (xs) => (() => {
    let add = (y) => map($tvar_β$Functor)((x) => Core.add($tvar_α$Num)(x)(y))(xs);
    return add(2)
})()