module Language.PureScript.CodeGen.Erl.Constants where

import Data.Text (Text)

-- Modules

prim :: Text
prim = "prim"

prelude :: Text
prelude = "prelude"

dataArray :: Text
dataArray = "data_array"

eff :: Text
eff = "control_monad_eff"

controlApplicative :: Text
controlApplicative = "control_applicative"

controlSemigroupoid :: Text
controlSemigroupoid = "control_semigroupoid"

controlBind :: Text
controlBind = "control_bind"

dataBounded :: Text
dataBounded = "data_bounded"

dataSemigroup :: Text
dataSemigroup = "data_semigroup"

dataHeytingAlgebra :: Text
dataHeytingAlgebra = "data_heytingAlgebra"

dataEq :: Text
dataEq = "data_eq"

dataOrd :: Text
dataOrd = "data_ord"

dataSemiring :: Text
dataSemiring = "data_semiring"

dataRing :: Text
dataRing = "data_ring"

dataEuclideanRing :: Text
dataEuclideanRing = "data_euclideanRing"

dataFunction :: Text
dataFunction = "data_function"

dataFunctionUncurried :: Text
dataFunctionUncurried = "data_function_uncurried"

dataIntBits :: Text
dataIntBits = "data_int_bits"
