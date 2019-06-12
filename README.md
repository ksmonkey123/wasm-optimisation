# wasm-optimisation
University of Bern, SCG seminar project: _Compacting WebAssembly_

## Motivation
WebAssemly code often seems to be generated relatively inefficiently. As a consequence around 40% of all instructions generated from C source code are simple memory movements (`local.set`, `local.get`, `local.tee`). The goal of this project was to try to reduce these functionally unnecessary instructions through the use of an SSA-IR. This would allow us to generate more space-efficient WebAssembly bytecode and to compare different WebAssembly compilers.

As a result of our underestimation of the scope of the project, it was unable to be finished. This repository contains all the work that has been done as part of this project. As of the codebase allows the disassembly of arbitrary `.wasm` files and the generation of graphical control flow graphs of both the CST as well as the SSA-IR. The following sections will outline how the codebase is intended to be used.

The slides for the end presentation of this project can be found [here](http://scg.unibe.ch/download/softwarecomposition/2019-05-21-Waelchli-WASM.pdf).

## Setup
Import the codebase as a **Scala**-project into your IDE. The project has been developed using _scala-sdk-2.12.8_ and _scalatest_. It is recommended to use the same Scala version. Make sure to add the contents of the `/lib` folder (_jgraph_ and _jheaps_) as project libraries.

## Use
