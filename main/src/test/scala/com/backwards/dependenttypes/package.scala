package com.backwards

/**
 * [[https://stepik.org/course/49181/syllabus Programming with Dependent Types]]
 *
 * [[https://stepik.org/course/2294/info Programming with Dependent Types Original Course]]
 *
 * [[https://github.com/siddhartha-gadgil/ProvingGround.git Source code for course]]
 * and associated website
 * [[https://siddhartha-gadgil.github.io/ProvingGround/index.html Proving Ground Website]]
 *
 * When cloning the course use "recursive" as it has dependent submodules e.g.
 * {{{
 *  git clone --recursive https://github.com/siddhartha-gadgil/ProvingGround.git
 * }}}
 *
 * If you don't use "recursive", then delete the submodule and do:
 * {{{
 *  git submodule sync
 *  git submodule init
 *  git submodule update
 * }}}
 *
 * Also, the course project uses Mill, so install with:
 * {{{
 *  brew install mill
 * }}}
 *
 * We can compile the course source code with:
 * {{{
 *  mill mantle.compile
 * }}}
 *
 * Start Mill REPL with:
 * {{{
 *  mill -i mantle.repl
 * }}}
 *
 * and then try out the REPL:
 * {{{
 *  Welcome to the Ammonite Repl 2.3.8-65-0f0d597f (Scala 2.13.3 Java 15.0.2)
 *
 *  @ import provingground.HoTT._
 *  import provingground.HoTT._
 *
 *  @ val A = "A" :: Type
 *  A: Typ[Term] = SymbTyp(name = Name(name = "A"), level = 0)
 *
 *  @ val a = "a" :: A
 *  a: Term = SymbObj(name = Name(name = "a"), typ = SymbTyp(name = Name(name = "A"), level = 0))
 *
 *  // And what is now the type of val a?
 *  @ a !: A
 *  res3: Term = SymbObj(name = Name(name = "a"), typ = SymbTyp(name = Name(name = "A"), level = 0))
 * }}}
 *
 * Build a fat JAR:
 * {{{
 *  mill mantle.assembly
 * }}}
 *
 * We can then copy the JAR (under <provingground root>/out/mantle/assembly/dest/out.jar) to any other sbt project under <root>/lib (and maybe rename).
 * Though for a multi-module project, use a "lib" directory under the required module.
 * We'll do that in this project to have <root>/main/lib/proving-ground.jar (though it's a bit big so won't be in this project's Git repository).
 * Plus, we will need the following added to build.sbt:
 * {{{
 *  exportJars := true
 * }}}
 */
package object dependenttypes {

}