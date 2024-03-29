# wasm-optimisation
University of Bern, SCG seminar project: _Compacting WebAssembly_

## Motivation
WebAssemly code often seems to be generated relatively inefficiently. As a consequence around 40% of all instructions generated from C source code are simple memory movements (`local.set`, `local.get`, `local.tee`). The goal of this project was to try to reduce these functionally unnecessary instructions through the use of an SSA-IR. This would allow us to generate more space-efficient WebAssembly bytecode and to compare different WebAssembly compilers.

As a result of our underestimation of the scope of the project, it was unable to be finished. This repository contains all the work that has been done as part of this project. As of the codebase allows the disassembly of arbitrary `.wasm` files and the generation of graphical control flow graphs of both the CST as well as the SSA-IR. The following sections will outline how the codebase is intended to be used.

The slides for the end presentation of this project can be found [here](http://scg.unibe.ch/download/softwarecomposition/2019-05-21-Waelchli-WASM.pdf).

## Setup
Import the codebase as a **Scala**-project into your IDE. The project has been developed using _scala-sdk-2.12.8_ and _scalatest_. It is recommended to use the same Scala version. Make sure to add the contents of the `/lib` folder (_jgraph_ and _jheaps_) as project libraries. For rendering of the graphical control flow graphs the [graphviz dot](https://www.graphviz.org/) command must be installed.

## Use
Note: Our code makes use of Scalas ability to create a fluent interface through implicit classes and methods. Most of these are defined in the two package objects `ch.awae.wasm.ast` and `ch.awae.wasm.io`.

To facilitate the use of the code we do provide an example WebAssembly binary (`change.wasm`) originally provided by the Mozilla Developer Network (MDN) under the Creative Commons Zero v1.0 Universal license. The original file can be found [here](https://github.com/mdn/webassembly-examples/tree/master/wasm-sobel).

We provide two main classes as entry points into the codebase: `ch.awae.wasm.Main` and `ch.awae.wasm.Demo`.

### Main
This class provides an example of the basic steps that can be performed by the codebase.
We'll outline the functionality here:

```scala
val module = "change.wasm".file.ast.module
```
This line loads the file with the name `change.wasm` as a byte stream (`.file`), then disassembles it (`.ast`) into a sort of concrete syntax tree (CST). Finally this CST is slightly reshuffled to provide easier access to function definitions (`.module`). WebAssembly distributes type signatures, function definitions, function imports and function bodies over different sections. These are collected into a single synthetic `funcs` section.
```scala
val functions = module.funcs.filter(_.isInstanceOf[DeclaredFunction]).map(_.asInstanceOf[DeclaredFunction])
```
We differentiate between two types of functions: _imported_ and _declared_ functions. We can only work with the function body of declared functions as imported functions are not defined inside the WebAssembly binary. Here we create a list of all functions that are declared within this binary.
```scala
val f = function(16)
```
Here we choose the 17th function for our subsequent steps.
```scala
val flow = cfg.Builder.build(f, module)
Dot(flow.dot, "cfg-raw")
```
Here we create a raw control flow graph (CFG) from the selected function. This graph still contains a lot of unnecessary blocks. We then take this CFG and render it using _dot_. The resulting PDF file will be stored in the `dot/` directory with the prefix _cfg-raw_.

Note: at the head of each block there will be a number. This number denotes the _stack-frame_ of the block. All blocks with the same number share their stack. If the number is wrapped in colons (e.g. `:: 2 ::`) it is a normal block. If however hashes are used (e.g. `## 3 ##`) the block is a loop header. Loop headers are always the first blocks generated by the `loop` command. Sometimes blocks also provide type information (e.g. `(i32)` or `(void)`). These are blocks where at least one predecessor may provide an return value on the stack. This is relevant for all transitions _entering_ the block from another block with a _higher_ stack-frame number. On such transitions the top value on the stack of the previous block is pushed on the top of the stack of the entered block. If the type information indicates `(void)`, no value is pushed onto the stack.
```scala
flow.prune()
Dot(flow.dot, "cfg-min")
```
Here we prune the CFG, i.e. remove unnecessary blocks and merge others where applicable. Then we generate another PDF for this pruned graph.
```scala
val ssa = new SsaParser(flow).parse()
Dot(ssa.dot, "ssa-raw")
```
Here we generate a CFG in a raw [SSA-IR](https://en.wikipedia.org/wiki/Static_single_assignment_form) and render it. Here the `START` node indicates the initial SSA symbols assigned to each of the function parameters. In the raw form a PHI-operation is inserted wherever a block posseses multiple predecessors. Many of these are redundant. (e.g. `0024 := PHI(0000, 0000)`).
```scala
ssa.prune()
Dot(ssa.dot, "ssa-min")
```
Finally we reduce the SSA form by recursively eliminating all redundant PHI operations (e.g. `0024 := PHI(0000, 0000)` and pure assignments (e.g. `0003 := 0001`). Depending on the size of the function this may require several minutes to complete! After that we generate another PDF, this time with the minimized SSA CFG.

### Demo
This is a simpler class making use of the simple DSL implemented in the file `ch/awae/wasm/DemoDSL.scala`. This is the class that was used in the live demo during the seminar presentation. We'll go through the functionality here:

```scala
val module = load module "change.wasm"
```
This line is basically equivalent to the first line of the `Main` class in that it loads a WebAssembly binary. It also counts all the instructions in all the functions and provides a count of both the total number of instructions and the number of memory instructions (i.e. `local.get`, `local.set`, `local.tee`).
```scala
module.module.functions foreach println
```
Here we print an indexed list of all functions to the console.
```scala
module process 16
```
This line takes the 17th _declared_ function and processes it as was done in the `Main` class. Also instruction counts for the selected function are provided.
