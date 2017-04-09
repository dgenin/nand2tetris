# Design of VM Translation

This document describes the architeture of JSON AST conversion to assembly.

## High-level Overview

**Inputs**: JSON AST with class and function definitions and statements.

**Outputs**: Hack VM assembly code

The components that will be used to achieve this are:

* Symgen - symbol table generator
* mmgen - memory map generator
* ssatran - translator of statement into VM assembly

## Symgen

Input the AST and output a function `symtab(sym, scope) -> def` that will return a symbol definition of `sym` in `scope`.  If the definition is not found, then it will raise an expension.

## Mmgen

This component will layout the different structures in memory.  It will produce a function that can map the various known symbols to a memory segment and an address within that segment.  This information will then be used to process the AST statements to create stack-based operations in the Hack VM assembly.

The function that `mmgen` will produce will be `memtab(sym, scope) -> (addr, seg)` (the VM has 8 segments, `static`, `this`, `local`, `argument`, `that`, `constant`, `pointer`, and `temp`, that can be used in pop/push operations).

## Ssatran

Will use `symtab` and `memtab` to translate the different statements from functions/methods to generate VM assembly code.