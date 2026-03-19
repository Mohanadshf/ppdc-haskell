PPDC Assignment 1: Haskell Basics
Course: Programmierparadigmen und Compilerbau

Semester: Sommersemester 2025

Instructor: Prof. Visvanathan Ramesh

TAs: Gamze Akyol, Alperen Kantarci

Deadline: May 21, 2025

📌 Project Overview
This assignment covers the fundamental concepts of functional programming in Haskell, adapted from the University of Helsinki Haskell MOOC. The focus is on function definition, recursion, pattern matching, and basic data types.

Exercises Included:
Set1.hs: Basic expressions, pattern matching, and recursion (Min. 6/11 for credit).

Set2a.hs: Guards, Lists, Maybe, and Either (Min. 6/11 for credit).

🛠 Setup & Requirements
Ensure you have the Haskell Stack tool installed on your system.

How to Edit
Only modify the todo sections in the provided files. Do not change the function signatures or the rest of the file structure.

Haskell
-- Example of what to change:
sum :: Integer -> Integer -> Integer
sum x y = x + y  -- Replaced 'todo' with the solution
Running Tests
To verify your solutions, run the following commands in your terminal from the Week1 directory:

Build the project:

DOS
stack build
Test Set 1:

DOS
stack runhaskell Set1Test.hs
Test Set 2a:

DOS
stack runhaskell Set2aTest.hs
Interactive Debugging:
If you want to test a specific function manually in GHCi:

DOS
stack ghci Set1.hs