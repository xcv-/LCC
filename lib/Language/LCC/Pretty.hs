{-# LANGUAGE OverloadedStrings #-}
module Language.LCC.Pretty where

import Control.Lens ((^.))

import Data.String
import qualified Data.Map as Map

import Text.PrettyPrint.Leijen.Text
import Text.Parsec.Pos

import Language.LCC.Types


oshow :: (Show a, IsString s) => a -> s
oshow = fromString . show

otext :: IsString s => String -> Doc
otext = text . fromString

tshow :: Show a => a -> Doc
tshow = text . oshow


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
           <+> params <+> text "->"
           <+> pretty (s^.sigReturn)
      where
        params = encloseSep lbrace rbrace (text ", ") (map pretty $ s^.sigParams)

instance Pretty SourcePos where
    pretty p = dquotes (otext $ sourceName p)
           <+> char 'l' <> int (sourceLine p)
           <+> char 'c' <> int (sourceColumn p)

instance (Pretty path, Pretty ret) => Pretty (Translation path ret) where
    pretty t = prettyParams <> pretty (t^.trImpl) <> linebreak <> comment
      where
        prettyParams =
          case t^.trSig.sigParams of
            [] -> empty
            ps -> encloseSep lbrace rbrace (text ", ") (map pretty ps)
                    <+> text "->" <> softbreak

        comment = char '#'
              <+> pretty (t^.trSig)
              <+> parens (pretty $ t^.trSourcePos)



instance Pretty path => Pretty (Expr path) where
    pretty (IntL i)     = tshow i
    pretty (DoubleL d)  = tshow d
    pretty (BoolL b)    = text $ if b then "true" else "false"
    pretty (CharL c)    = tshow c
    pretty (StringL s)  = tshow s
    pretty (Builtin t)  = char '<' <> pretty t <+> text "builtin>"
    pretty (Array xs)   = list (map pretty xs)

    pretty (SConcat ss) =
        let prettyElem (StringL s) = otext . init . tail . show $ s
            prettyElem expr        = char '$' <> braces (pretty expr)
        in hcat (map prettyElem ss)

    pretty (Funcall path args) = pretty path <> tupled (map pretty args)

    pretty (Cond c t f) = text "if" <+> pretty c
                       <> softline <> text "then" <+> pretty t
                       <> softline <> text "else" <+> pretty f



instance (Pretty tag, Pretty a) => Pretty (TaggedTree tag a) where
    pretty (Leaf x)    = pretty x
    pretty (Subtree m) = encloseSep (lbrace    <> line)
                                    (linebreak <> rbrace)
                                    (linebreak <> linebreak)
                                    (map prettyElem $ Map.toList m)

      where
        prettyElem :: (Pretty tag, Pretty a) => (tag, a) -> Doc
        prettyElem (tag, a) = pretty tag <> char ':' <> softline <> pretty a
