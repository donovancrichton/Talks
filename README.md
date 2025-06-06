# Talks


This repo contains code and pdfs for the talks I give on
an ad-hoc or one-off basis. In order from the most recent 
to least recent. I started collating my talks during the
middle of my PhD in early 2023.

## Contents
### 2025
  * [Explicit Substitutions Part 1: STLC and Background](#explicit-substitutions-part-1:-stlc-and-background)

### 2024
  * [Enforcing a discipline of Total Functional Programming
    through Dependent Types](#enforcing-a-discipline-of-total-functional-programming-through-dependent-types)
### 2023
  * [Why is so much of FP about Types instead of
    Functions?](#why-is-so-much-of-fp-about-types-instead-of-functions)
  * [Towards Palatable Functional Programming With
     Dependent Type
     Theories](#towards-palatable-functional-programming-with-dependent-type-theories)
  * [Introduction to Idris and GADTs](#introduction-to-idris-and-gadts)

## Talk Links
The link to each talk is usually the first word of the 
description.

### Explicit Substituions Part 2
   During Part 1 we covered the syntax for expresison grammars, 
   typing Rules, and the simply typed lambda calculus, 
   finishing with the syntax for explicit Subsitutions. 
   This talk will flesh our extended lambda calculus by providing
   intution into the new substituion combinators, evaluation
   semantics, and finally extending to a dependently typed
   calculus in the vein of modern type theoretic presentations.

### Explicit Substituions Part 1: STLC and Background
  [This](/BFPG/2025/IntroToPLTAndSTLC/Talk) was a short talk given to the 
  [Brisbane Functional Programming Group](https://lu.ma/bfpg) which introduces basic syntax from programming language theory and applies this to the simply typed lambda calculus. We cover how to read grammars and typing rules, α-equivalence, free and bound variables, substituion, and de-bruin indices. We then show how to extend STLC with more types and terms, and finally: explicit substituions.

### Enforcing a discipline of Total Functional Programming through Dependent Types
  [This](/BFPG/2024/TotalFPThroughDepTytpes/Talk) was a short talk
  given to the [Brisbane Functional Programming
  Group](https://www.meetup.com/brisbane-functional-programming-group) about how dependent types can be used to practice total functional programming without wrapping all our types in eithers/maybes.

### Why is so much of FP about Types instead of Functions?
  [This](/BFPG/2023/WhyIsFPAboutTypes/Talk) was a short and 
  informal introductory talk given to the [Brisbane Functional Programming
  Group](https://www.meetup.com/brisbane-functional-programming-group) which introduces the notion of a function and 
  describes their limitations, and why types tend to take
  up a surprising amount of the discussion space in
  functional programming communities. I cover Sets,
  Relations, Functions, Algebraic Data
  Types, Type Classes, and Dependent Types.

### Towards Palatable Functional Programming With Dependent Type Theories
  [This](/BFPG/2023/PalatableFunctionalProgrammingWithDTT/Talk)
  was a short and informal talk given to the 
  [Brisbane Functional Programming Group](https://www.meetup.com/brisbane-functional-programming-group) which served as
  a reintroduction of myself to the group after an
  approximate three year absence while I resided in
  Canberra. The talk gives a gentle introduction to the 
  limitations of syntactic totality checking in Idris at 
  the time of writing, and motivates my broad PhD topic: The
  need to investigate type-based approaches to totality and
  guarded recursion, and how this necessitates the proposal
  of a construction of a guarded, observational, linear, 
  dependent type theory to allow us to experiment with
  improving dependently typed languages for programmers.

### Introduction To Idris and GADTS
  [This](/COMP1130/IntroToIdrisAndGADTS/Talk) 
  was a short guest lecture given to the 2023 Semester 1
  students of the Australian National University's COMP1130
  course. I introduce Generalised Algebraic Data Type (GADT)
  syntax in Haskell, motivate them, and then describe why
  the move to a friendlier language (Idris) is necessary.
  I finish with some examples of dependent types and GADTs
  in Idris.
