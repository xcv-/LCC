{-# LANGUAGE OverloadedStrings #-}
module Language.LCC.Pretty where

import Control.Lens ((^.), to)

import Data.Monoid (mconcat)
import Data.String
import Data.Text.Lazy.Lens (packed)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T

import Text.PrettyPrint.Leijen.Text hiding (string)
import Text.Parsec.Pos

import Language.LCC.AST


string :: String -> Doc
string = text . fromString

tshow :: Show a => a -> Doc
tshow = string . show


prettyParamList :: [Param] -> Doc
prettyParamList params =
    encloseSep lparen rparen (text ", ") (map pretty params)


instance Pretty RelativePath where
    pretty = tshow

instance Pretty RelativeVarPath where
    pretty = tshow

instance Pretty AbsolutePath where
    pretty = tshow

instance Pretty AbsoluteVarPath where
    pretty = tshow



instance Pretty UnknownType where
    pretty = tshow

instance Pretty Type where
    pretty = tshow

instance Pretty Param where
    pretty = tshow

instance (Pretty path, Pretty ret) => Pretty (Signature path ret) where
    pretty s = pretty (s^.sigPath) <> colon
           <+> encloseSep lparen rparen (text ", ") (map pretty $ s^.sigParams)
           <+> text "->"
           <+> pretty (s^.sigReturn)

instance Pretty SourcePos where
    pretty p = dquotes (sourceName p^.packed.to text)
           <+> char 'l' <> int (sourceLine p)
           <+> char 'c' <> int (sourceColumn p)

instance Pretty Annotation where
    pretty = tshow

instance (Pretty path, Pretty ret) => Pretty (Translation path ret) where
    pretty t = nest 4 $ body <$> comment
      where
        body =
          case t^.trSig.sigParams of
            [] -> nest 4 (pretty $ t^.trImpl)
            ps -> encloseSep lparen rparen (text ", ") (map pretty ps)
                    <+> nest 4 (text "->" </> pretty (t^.trImpl))

        comment = char '#' <+> pretty (t^.trSig)
              <$> char '#' <+> parens (pretty $ t^.trSourcePos)



instance Pretty path => Pretty (FuncallPath path) where
    pretty (Fn path)      = pretty path
    pretty (Builtin sig)  = text "<builtin" <+> pretty sig <> char '>'
    pretty (Input t name) = text "<input"
                            <+> string name <> char ':' <+> pretty t
                         <> char '>'

instance Pretty path => Pretty (Expr path) where
    pretty (IntL i)      = tshow i
    pretty (DoubleL d)   = tshow d
    pretty (BoolL b)     = text $ if b then "true" else "false"
    pretty (CharL c)     = tshow c
    pretty (StringL s)   = tshow s
    pretty (Array xs)    = list (map pretty xs)

    pretty (SConcat ss) =
        let prettyElem (StringL s) = text . T.pack . init . tail . show $ s
            prettyElem expr        = char '$' <> braces (pretty expr)
        in dquotes (hcat $ map prettyElem ss)

    pretty (Funcall path args) = pretty path <> if null args
                                                  then empty
                                                  else tupled (map pretty args)

    pretty (Cond c t f) = text "if" <+> pretty c
                       <> softline <> align (text "then" <+> pretty t
                       <> softline <>        text "else" <+> pretty f)



instance (Pretty tag, Pretty a) => Pretty (TaggedTree tag a) where
    pretty (Leaf xs)   = mconcat $ map pretty xs
    pretty (Subtree m) = cat . punctuate linebreak
                             . map prettyElem
                             . concatMap splitLeafs
                             $ Map.toList m

      where
        splitLeafs :: (tag, TaggedTree tag a) -> [(tag, TaggedTree tag a)]
        splitLeafs (tag, Leaf xs) = map (\x -> (tag, Leaf [x])) xs
        splitLeafs tup = [tup]

        prettyElem :: (Pretty tag, Pretty a) => (tag, TaggedTree tag a) -> Doc
        prettyElem (tag, a) = pretty tag <> char ':'
                          <+> case a of
                                Leaf [x]  -> pretty x
                                Subtree _ -> nest 4 (lbrace <$> pretty a)
                                                            <$> rbrace
                                Leaf _ ->
                                  error "Pretty(TaggedTree): non-singleton leaf"


instance (Pretty path, Pretty ret) => Pretty (Locale path ret) where
    pretty l = text "locale " <> (l^.localeName.packed.to text)
            <> encloseSep (text "  ") (char ';') (text ", ") params
            <> line
            <> line
            <> pretty (l^.localeAST)
      where
        params = map (\(LocaleInput param anns) ->
                          hsep (map pretty anns) <+> pretty param)
                     (l^.localeInputs)
