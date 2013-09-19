LCC
===

This is a preview of a static localization compiler.

To compile this, you will need cabal-install.

In order to build:

```
cabal configure
cabal build
```

There's a convenience symlink to run it in the root of the repository, so
just

```
./lcc help
```

To compile an example translation to a Java class,

```
./lcc compile java TestTranslation out-dir output.package.name
```

The generated file would be out-dir/output/package/name/TestTranslation.java
