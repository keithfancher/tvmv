module TVMV
  ( TVMV,
    runTVMV,
    liftEither,
  )
where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Error (Error)

-- Wrap the stack!
type TVMV a = ExceptT Error IO a

runTVMV :: TVMV a -> IO (Either Error a)
runTVMV = runExceptT

-- Admittedly, I only vaguely understand why this works :')
--
-- There's a `liftEither` function in the `mtl` library as well, but I ALSO
-- don't fully get what the difference is between that and `transformers`
-- (which is what I'm using here).
liftEither :: Either Error a -> TVMV a
liftEither = ExceptT . return
