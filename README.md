# RandomGroup – Random Team and Place Generator (Scheme / Racket)

## Overview

RandomGroup is a program written in **Scheme (Racket)** that randomly assigns positions or generates teams from a list of people.

The program was designed to help with situations such as:

- assigning seats randomly in an exam room or stadium
- creating random teams for games or activities

The user enters a list of names and the program randomly distributes them.

---

## Features

The program offers two main functionalities:

### 1️⃣ Random Place Assignment
Assigns each person a random position.

Example:

Input: <br>
Names: A,B,C <br>
Places: 1,2,3 <br>

Output (example):(A . 2),(B . 1),(C . 3)


---

### 2️⃣ Random Team Generator

Creates teams randomly from a list of people.

Example:

Input: <br>
Names: A,B,C,D,E,F <br>
Team size: 2 <br>


Output (example):
Team1: A B <br>
Team2: C D <br>
Team3: E F <br>


---

### 3️⃣ Save Results

The program can optionally **save the generated results into a file**.

Example path:C:/PATH/TO/FOLDER/file.txt



---

## Technologies

- Scheme
- Racket

Concepts used:

- recursion
- list manipulation
- random selection
- file writing

---

## How to Run

### 1️⃣ Install Racket

Download from:

https://racket-lang.org/

---

### 2️⃣ Run the program

Open the file in **DrRacket** or run it using:

```bash
racket projetPos.rkt
