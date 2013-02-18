module Test.SmallCheck.Property.Result where

type Argument = String

data PropertySuccess
  = Exist [Argument] PropertySuccess
  | ExistUnique [Argument] PropertySuccess
  | PropertyTrue
  | Vacuously PropertyFailure
  deriving (Eq, Show)

data PropertyFailure
  = NotExist
  | AtLeastTwo [Argument] PropertySuccess [Argument] PropertySuccess
  | CounterExample [Argument] PropertyFailure
  | PropertyFalse
  deriving (Eq, Show)
