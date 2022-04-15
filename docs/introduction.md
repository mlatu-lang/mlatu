---
layout: default
title: Introduction
nav_order: 1
permalink: /
redirect_from:
  - /introduction
---

# Introduction to mlatu

mlatu is a very simple programming language, at its core. All you need to understand are terms and rules.

A mlatu term is either a word or a quotation. A word is a bit of text without whitespace that acts as an identifier. A quotation is a series of terms wrapped in parentheses, and opaquely groups the terms together.

A mlatu rule is a left and right side, each of which contain a series of terms. Including a rule tells the mlatu implementation to replace all instances of the left side (the pattern) with the right side (the replacement). mlatu rules can be used as functions, predicates, or macros.

This conceptually unified system is computationally universal, which offers similar advantages to those of Lisp - homoiconic code-data manipulation in a simple syntax - with the addition of concatenativity: any series of terms can be concatenated with any other series of terms to have an output equivalent to the concatenation of their inputs.

[Onward, to the six primitives of mlatu!](primitives.md)

(Since this tutorial is unfinished, you can find a briefer one [here](tutorial.md)]