# ppdc-haskell
Assignments and solution for the Uni Course Programming Paradigms and Compiler Construction SoSe25
# Programming Paradigms & Compiler (PPDC)

This repository contains my weekly Haskell assignments for the PPDC course.

Programmierparadigmen und Compilerbau (PPDC) - Haskell Assignments
University: Goethe-Universität Frankfurt am Main

Semester: Sommersemester 2025

## Language
- Haskell
  
Course Lead: Prof. Visvanathan Ramesh

Student: Mohanad Al-Ramessi

📖 Course Overview
This repository contains my solutions for the PPDC course assignments. The curriculum focuses on functional programming paradigms using Haskell, moving from basic recursion to advanced type inference and combinator design patterns.

💡 Important Note: For every week, I highly recommend reading the provided Assignment PDF located within each folder. These documents contain the specific constraints, mathematical definitions, and theoretical background required to understand the code logic.

📂 Repository Structure
📁 Week 1: Haskell Foundations
Topics: Function definitions, basic expressions, pattern matching, and simple recursion.

Key Files: Set1.hs, Set2a.hs.

Highlight: Introduction to the Haskell MOOC structure.

📁 Week 2: Advanced Recursion & Lists
Topics: Mathematical functions (Binomial coefficients), Guards, and "Low-level" list manipulation.

Key Files: Set2b.hs, Set3a.hs, Set3b.hs.

Highlight: Set3b focuses on implementing list functions without using the standard library.

📁 Week 3: Type Classes & Folds
Topics: Type classes, foldr/foldl implementation, and Algebraic Datatypes (ADT).

Key Files: Set4a.hs, Set4b.hs, Set5a.hs.

Highlight: Defining custom recursive datatypes like Vehicle and ShoppingEntry.

📁 Week 4: Type Inference & Combinators
Topics: The 5-step Type Inference Algorithm and the Validation Combinator pattern.

Key Files: Assignment4.hs, Assignment4_Exercise1.pdf.

Highlight: A theoretical derivation of generic types and implementing compositional validation logic.

🛠 Technical Setup
All assignments are managed using the Haskell Stack build tool.

Navigate to a specific week: cd WeekX

Build the project: stack build

Run Tests: stack runhaskell SetXTest.hs (Check specific READMEs for exact test filenames).

Interactive Mode: stack ghci SetX.hs
