module Language.PureScript.CodeGen.Erl.Constants where

import Prelude.Compat

-- Modules

prim :: String
prim = "prim"

prelude :: String
prelude = "prelude"

dataArray :: String
dataArray = "data_array"

eff :: String
eff = "control_monad_eff"

controlApplicative :: String
controlApplicative = "control_applicative"

controlSemigroupoid :: String
controlSemigroupoid = "control_semigroupoid"

controlBind :: String
controlBind = "control_bind"

dataBounded :: String
dataBounded = "data_bounded"

dataSemigroup :: String
dataSemigroup = "data_semigroup"

dataHeytingAlgebra :: String
dataHeytingAlgebra = "data_heytingAlgebra"

dataEq :: String
dataEq = "data_eq"

dataOrd :: String
dataOrd = "data_ord"

dataSemiring :: String
dataSemiring = "data_semiring"

dataRing :: String
dataRing = "data_ring"

dataEuclideanRing :: String
dataEuclideanRing = "data_euclideanRing"

dataFunction :: String
dataFunction = "data_function"

dataFunctionUncurried :: String
dataFunctionUncurried = "data_function_uncurried"

dataIntBits :: String
dataIntBits = "data_int_bits"
