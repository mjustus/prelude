module Make (S : Type.TYPE) = State_t.Make (S) (Monad.Id)
