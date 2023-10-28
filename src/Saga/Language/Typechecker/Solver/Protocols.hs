module Saga.Language.Typechecker.Solver.Protocols where
import           Control.Monad.Except
import qualified Data.List.NonEmpty                            as NonEmpty
import qualified Data.Map                                      as Map
import           Prelude                                       hiding (id)
import           Saga.Language.Typechecker.Environment         (CompilerState (..))
import           Saga.Language.Typechecker.Errors              (SagaError)
import           Saga.Language.Typechecker.Inference.Inference (InferM)
import           Saga.Language.Typechecker.Protocols           (Protocol (..),
                                                                ProtocolID)
import           Saga.Language.Typechecker.Qualification       (Qualified (..))
import           Saga.Language.Typechecker.Solver.Constraints  (Evidence, Item)
import           Saga.Language.Typechecker.Solver.Substitution (Subst)
import           Saga.Language.Typechecker.Solver.Unification  (Cycle)
import           Saga.Language.Typechecker.Type                (Type)



data ImplConstraint = Impl Evidence Item ProtocolID

type ProtocolResolver m = InferM (Cycle Type) m


solve :: ImplConstraint -> Subst Evidence
solve impl = _s


-- -- | Implementation Constraints solving
-- resolve :: [ImplConstraint] -> ProtocolResolver ()
-- resolve constraints = do

--   forM_ constraintsPerType (check allImplemented)


--   where

--     constraintsPerType = constraints ||> groupWith type' |> fmap pair |> Map.fromList
--     pair group = (type' $ NonEmpty.head group, protocol' <$> NonEmpty.toList group)

--     check f ids = do
--       Saga { protocols } <- ask
--       case f ids protocols of
--         Nothing -> throwError $ Fail $ "Could not resolve for protocols: " ++ show ids
--         Just _ -> return ()


--     allImplemented ids env = do

--       check <- implemented <$> sequence protocols'
--       when check (return ())


--       where

--         implemented ps = not $ null $ foldl atLeastOneType [] ps

--         atLeastOneType [] p  = extract <$> implementations p
--         atLeastOneType tys p = tys `intersect` (extract <$> implementations p)

--         extract (_ :=> (t, _)) = t
--         protocols' = ids <&> \id' -> env ||> find (\(Protocol { id }) -> id == id')


--     type' (IP ty _)      = ty
--     protocol' (IP _ p) = p



-- reduce :: [ImplConstraint] -> Solve [ImplConstraint]
-- --reduce cs | trace ("\nReducing\n\tImplementation constraint:" ++ show cs) False = undefined
-- reduce cs = mapM toHNF cs >>= simplify . concat

-- toHNF :: ImplConstraint -> Solve [ImplConstraint]
-- toHNF ip | inHNF ip   = return [ip]
--          | otherwise =  do
--             ipConstraints <- byImplementation ip
--             ipConstraints' <- mapM toHNF ipConstraints
--             return $ concat ipConstraints'

-- inHNF :: ImplConstraint -> Bool
-- inHNF (ty `IP` p) = hnf ty
--  where hnf (TVar v) = True
--        hnf _        = False

-- byImplementation :: ImplConstraint -> Solve [ImplConstraint]
-- --byImplementation impl | trace ("\nSearch by Implementation:\n\t" ++ show impl) False = undefined
-- byImplementation (ty `IP` p)    = do
--   Saga { protocols } <- ask
--   concat <$> sequence [ tryInst impl | impl <- impls p protocols ]

--   where
--     impls id' = find (\(Protocol { id }) -> id == id') |> maybe [] implementations
--     tryInst (cs :=> (ty', e)) = do
--       sub <- catchError (ty' `unify` ty) (handleErr |> throwError)
--       return $ fmap (apply sub . mkIP) cs

--     handleErr (UnificationFail t1 t2) = Fail $ "Types do not match:\n\t" ++ show t1 ++ "\n\t" ++ show t2
--     handleErr err = err
--     mkIP (ty `Implements` p) = ty `IP` p


-- simplify   :: [ImplConstraint] -> Solve [ImplConstraint]
-- -- simplify cs | trace ("\nSimplifying\n\tImplementation constraint:" ++ show cs) False = undefined
-- simplify = loop []
--  where
--   loop checked []     = return checked
--   loop checked (ipc:ipcs) = do
--     entailed <- entail (checked ++ ipcs) ipc
--     if entailed
--       then loop checked ipcs
--       else loop (ipc: checked) ipcs

-- entail :: [ImplConstraint] -> ImplConstraint -> Solve Bool
-- -- entail ipcs current | trace ("\nEntailing\n\tCurrent: " ++ show current ++ "\n\tOthers:" ++ show ipcs) False = undefined
-- entail ipcs ipConstraint = do
--   protocols <- ask
--   baseConstraints <- mapM byBase ipcs
--   --traceM $ "Checking by base constraints:\n\t" ++ show baseConstraints
--   if any (ipConstraint `elem`) baseConstraints
--     then return True
--     else checkImpls

--   where
--     checkImpls = do
--       constraints <- byImplementation ipConstraint
--       --traceM $ "Checking by implementations:\n\t" ++ show constraints
--       entailments <- mapM (entail ipcs) constraints
--       --traceM $ "Entailments:\n\t" ++ show entailments
--       return (not (null entailments) && and entailments)


-- byBase :: ImplConstraint -> Solve [ImplConstraint]
-- -- byBase impl | trace ("\nSearching base constraints of\n\t" ++ show impl) False = undefined
-- byBase impl@(ty `IP` p) = do
--     Saga { protocols } <- ask
--     impls <- sequence [ byBase (ty `IP` base) | base <- sups p protocols]
--     let all = impl : concat impls
--     --traceM $ "Found:\n\t" ++ show all
--     return all
--     where
--       sups id' = maybe [] supers . find (\(Protocol { id }) -> id == id')


