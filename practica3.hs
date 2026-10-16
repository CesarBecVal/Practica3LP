type ID = String
data LNLambda = Bvar Int | Fvar ID | AbsL LNLambda | AppL LNLambda LNLambda deriving Eq