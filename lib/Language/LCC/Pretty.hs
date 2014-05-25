{-# LANGUAGE OverloadedStrings #-}
module Language.LCC.Pretty where

import Control.Lens ((^.), to)

import Data.String
import Data.Text.Lazy.Lens (packed)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T

import Text.PrettyPrint.Leijen.Text
import Text.Parsec.Pos

import Language.LCC.Types


tshow :: Show a => a -> Doc
tshow = text . fromString . show


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
        params = encloseSep lparen rparen (text ", ") (map pretty $ s^.sigParams)

instance Pretty SourcePos where
    pretty p = dquotes (sourceName p^.packed.to text)
           <+> char 'l' <> int (sourceLine p)
           <+> char 'c' <> int (sourceColumn p)

instance (Pretty path, Pretty ret) => Pretty (Translation path ret) where
    pretty t = nest 4 $ body <$> comment
      where
        body =
          case t^.trSig.sigParams of
            [] -> nest 4 (pretty $ t^.trImpl)
            ps -> encloseSep lparen rparen (text ", ") (map pretty ps)
                    <+> (nest 4 $ text "->"
                              </> pretty (t^.trImpl))

        comment = char '#' <+> pretty (t^.trSig)
              <$> char '#' <+> parens (pretty $ t^.trSourcePos)



instance Pretty path => Pretty (Expr path) where
    pretty (IntL i)      = tshow i
    pretty (DoubleL d)   = tshow d
    pretty (BoolL b)     = text $ if b then "true" else "false"
    pretty (CharL c)     = tshow c
    pretty (StringL s)   = tshow s
    pretty (Builtin sig) = text "<builtin" <+> pretty sig <> char '>'
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
    pretty (Leaf x)    = pretty x
    pretty (Subtree m) = cat . punctuate linebreak
                             . map prettyElem
                             $ Map.toList m

      where
        prettyElem :: (Pretty tag, Pretty a) => (tag, TaggedTree tag a) -> Doc
        prettyElem (tag, a) = pretty tag <> char ':'
                          <+> case a of
                                Leaf _    -> pretty a
                                Subtree _ -> nest 4 (lbrace
                                                    <$> pretty a)
                                         <$> rbrace



instance (Pretty path, Pretty ret) => Pretty (Locale path ret) where
    pretty l = text "locale " <> l^.localeName.packed.to text
            <> line
            <> line
            <> pretty (l^.localeAST)
