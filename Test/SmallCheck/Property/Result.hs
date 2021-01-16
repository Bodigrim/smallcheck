{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif

module Test.SmallCheck.Property.Result
  ( PropertySuccess(..)
  , PropertyFailure(..)
  , ppFailure
  , Reason
  , Argument
  ) where

import Text.PrettyPrint (Doc, empty, hsep, nest, render, text, (<+>), ($+$), ($$))

type Argument = String

-- | An explanation for the test outcome.
type Reason = String

data PropertySuccess
  = Exist [Argument] PropertySuccess
  | ExistUnique [Argument] PropertySuccess
  | PropertyTrue (Maybe Reason)
  | Vacuously PropertyFailure
  deriving (Eq, Show)

data PropertyFailure
  = NotExist
  | AtLeastTwo [Argument] PropertySuccess [Argument] PropertySuccess
  | CounterExample [Argument] PropertyFailure
  | PropertyFalse (Maybe Reason)
  deriving (Eq, Show)

class Pretty a where
  pretty :: a -> Doc

instance Pretty PropertyFailure where
  pretty NotExist = text "argument does not exist"
  pretty (AtLeastTwo args1 s1 args2 s2) =
    text "there are at least two" <+>
    plural args1 empty (text "sets of") <+>
    text "arguments satisfying the property:" $$
      formatExample args1 s1 $$ formatExample args2 s2
    where
    formatExample args s = nest ind $ text "for" <+> prettyArgs args </> pretty s
  pretty (CounterExample args f) =
    text "there" <+>
    text (plural args "exists" "exist") <+>
    prettyArgs args <+>
    text "such that"
    </> pretty f
  pretty (PropertyFalse Nothing)  = text "condition is false"
  pretty (PropertyFalse (Just s)) = text s

instance Pretty PropertySuccess where
  pretty (PropertyTrue Nothing)  = text "condition is true"
  pretty (PropertyTrue (Just s)) = text s
  pretty (Exist       args s) = existsMsg False args s
  pretty (ExistUnique args s) = existsMsg True args s
  pretty (Vacuously s) = text "property is vacuously true because" </> pretty s

ind :: Int
ind = 2

infixl 5 </>
(</>) :: Doc -> Doc -> Doc
a </> b = a $+$ nest ind b

prettyArgs :: [Argument] -> Doc
prettyArgs = hsep . map text

existsMsg :: Pretty a => Bool -> [Argument] -> a -> Doc
existsMsg unique args s =
  text "there" <+> text (plural args "exists" "exist") <+>
  (if unique then text "unique" else empty) <+>
  prettyArgs args <+>
  text "such that" </>
  pretty s

plural :: [a] -> b -> b -> b
plural lst sing pl =
  case lst of
    _:_:_ -> pl
    _ -> sing

ppFailure :: PropertyFailure -> String
ppFailure = render . pretty
