{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import Language.C.DSL

plus =
  fun [intTy] "myPlus"[int "a", int "b"] $ hBlock [
    creturn ("a" + "b")
  ]
minus =
  fun [intTy] "myMinus"[int "a", int "b"] $ hBlock [
    creturn ("a" - "b")
  ]

notNull =
  fun [voidTy] "isNULL"[intPtr "a"] $ hBlock [
    cif ("a" ==: 0) $ hBlock [
       liftE $ "printf"#[str"Is NULL\n"],
       cvoidReturn
    ],
    liftE $ "printf"#[str"Isn't NULL\n"]
  ]

toplevel = transUnit [export plus, export minus, export notNull]
