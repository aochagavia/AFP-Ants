# AFP-Ants

*By Daan Knoope, Reinier Maas, Jorrit Dorrestijn and Adolfo Ochagavía*

# Compile and run

```
> stack build
> cd .stack-work/install/5df6cd37/bin
> ./FunctionalAnts > intructions.ant 
```

The output of our executable is a text stream where each line corresponds
to an ant instruction.

Note: you can also run the unit tests with `stack test`.

# The Ant program

The ant program is contained in `Ant.hs`

# Architecture

The DSL and compiler are defined in the `Language` module:

* `Language.Examples`: example definitons of programs using our DSL.
* `Language.Instruction`: datatype definitions for the low level representation of ant instructions.
* `Language.Fragment`: datatype definitions for the high level representation of our DSL and front-end of the compiler.
* `Language.Function`: higher level constructs built on top of fragments.
* `Language.Codegen`: translation of high level programs to low level instructions.
* `Language.Optimizer`: simple optimization pass for for low level instructions.
* `Language.Compiler`: the full compiler pipeline.
