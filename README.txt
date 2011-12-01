================================================================================
=============== DVDA Verifiably Differentiates Algorithmically =================
================================================================================

Description
===========
Library for symbolic algebra using a simple expression type Expr. Expr can be
automatically differentated in forward and reverse modes. It can also be
converted to efficient C code which is automatically generated/compiled/linked
at runtime.

Pretty GraphViz plots of Expr can be created Numeric.Dvda.Vis.previewExprs


Interface
=========
Expr instances Num, Fractional, and Floating. Use functions from
Numeric.Dvda.Symbolic to construct symbolic scalars/vectors/matrices, or numeric
vector/matrices.

Expr behaves as expected with Octave-like syntax, except that all binary
operations [+,-,*,/,**,etc] are applied elementwise. 

Matrix multiplication will soon be implemented as (dot).


Additional documentation
=========================
The Hackage documentation currently covers every function in the api.

See Numeric.Dvda.Examples (dvda/Numeric/Dvda/Examples.hs) for examples of
intended use.


Build instructions
==================
Cabal build:

    $ cabal configure
    $ cabal build

Get started right away

    $ ghci
    >> :l Numeric.Dvda
    >> let x = sym "x"
    >> let y = sym "y"
    >> let z = x*y**2 + cos(x)
    >> print z
    >> let dz = rad z [x,y]
    >> print dz
    >> previewExprs [z]
    >> previewExprs dz
    >> previewExprs $ z:dz

See some example c code:

    $ ghci
    >> :l Numeric.Dvda.Examples
    >> showCCodeExample

See a graph of a huge gradient:

    $ ghci
    >> :l Numeric.Dvda.Examples
    >> radExample

or test the dynamic compilation/linker:

    $ ghci
    >> :l Numeric.Dvda.Examples
    >> codegenExample
