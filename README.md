# knight-Tour
program for my AI assignment.   
Provides solution to the knight's tour problem.  
The program is capable of providing solution for 1x1 chessboard to 8x8 full-sized chessboard.  
The size of the chessboard and starting position of the knight piece are defined by the user.  

## About knight tour problem

A knight's tour is a sequence of moves of a knight on a chessboard such that the knight visits every square only once.  
Source: https://en.wikipedia.org/wiki/Knight%27s_tour

### Prerequisites

Install clisp via https://clisp.sourceforge.io/

For Mac OS:
```
brew install clisp
```
For Windows i recommend to take the cygwin approach.

### Installing

```
git clone https://github.com/weishenho/knight-tour
```


## Running the program

start clisp by entering "clisp" into the command prompt/terminal.

Enter the following:
```
(load "aima/aima.lisp")
(load "knight-tour.lisp")
```

Finally, call function (knights-tour-greedy x y m n)<br/>
replace x, y, m, n with the approriate values<br/>
x, y - position of the knight piece<br/>
m, n - size of the board i.e m x n<br/>
Example:
```
(knights-tour-greedy 5 5 8 8)
```

Output:
```
   |---|---|---|---|---|---|---|---|
 7 |46 |33 |12 |17 |48 |31 | 2 |15 |
   |---|---|---|---|---|---|---|---|
 6 |11 |18 |47 |32 |13 |16 |59 |30 |
   |---|---|---|---|---|---|---|---|
 5 |34 |45 |40 |49 |58 | 1 |14 | 3 |
   |---|---|---|---|---|---|---|---|
 4 |19 |10 |57 |54 |41 |50 |29 |60 |
   |---|---|---|---|---|---|---|---|
 3 |44 |35 |42 |39 |62 |55 | 4 |25 |
   |---|---|---|---|---|---|---|---|
 2 | 9 |20 |53 |56 |51 |26 |61 |28 |
   |---|---|---|---|---|---|---|---|
 1 |36 |43 |22 | 7 |38 |63 |24 | 5 |
   |---|---|---|---|---|---|---|---|
 0 |21 | 8 |37 |52 |23 | 6 |27 |64 |
   |---|---|---|---|---|---|---|---|
     0   1   2   3   4   5   6   7 
#((5 . 5) (6 . 7) (7 . 5) (6 . 3) (7 . 1) (5 . 0) (3 . 1) (1 . 0) (0 . 2) (1 . 4) (0 . 6) (2 . 7) (4 . 6) (6 . 5) (7 . 7) (5 . 6) (3 . 7) (1 . 6) (0 . 4) (1 . 2) (0 . 0) (2 . 1)
  (4 . 0) (6 . 1) (7 . 3) (5 . 2) (6 . 0) (7 . 2) (6 . 4) (7 . 6) (5 . 7) (3 . 6) (1 . 7) (0 . 5) (1 . 3) (0 . 1) (2 . 0) (4 . 1) (3 . 3) (2 . 5) (4 . 4) (2 . 3) (1 . 1) (0 . 3)
  (1 . 5) (0 . 7) (2 . 6) (4 . 7) (3 . 5) (5 . 4) (4 . 2) (3 . 0) (2 . 2) (3 . 4) (5 . 3) (3 . 2) (2 . 4) (4 . 5) (6 . 6) (7 . 4) (6 . 2) (4 . 3) (5 . 1) (7 . 0)) 
```


### Sample Results
```
> (knights-tour-greedy 4 4 5 5)
   |---|---|---|---|---|
 4 | 3 |16 |11 |22 | 1 |
   |---|---|---|---|---|
 3 |10 |21 | 2 |17 |12 |
   |---|---|---|---|---|
 2 |15 | 4 |23 | 8 |25 |
   |---|---|---|---|---|
 1 |20 | 9 | 6 |13 |18 |
   |---|---|---|---|---|
 0 | 5 |14 |19 |24 | 7 |
   |---|---|---|---|---|
     0   1   2   3   4 
#((4 . 4) (2 . 3) (0 . 4) (1 . 2) (0 . 0) (2 . 1) (4 . 0) (3 . 2) (1 . 1) (0 . 3) (2 . 4) (4 . 3) (3 . 1) (1 . 0) (0 . 2) (1 . 4) (3 . 3) (4 . 1) (2 . 0) (0 . 1) (1 . 3) (3 . 4)
  (2 . 2) (3 . 0) (4 . 2)) 
```

```
> (knights-tour-greedy 1 1 8 8)
   |---|---|---|---|---|---|---|---|
 7 | 4 | 7 |60 |43 |58 | 9 |64 |41 |
   |---|---|---|---|---|---|---|---|
 6 |47 |22 | 5 | 8 |61 |42 |57 |10 |
   |---|---|---|---|---|---|---|---|
 5 | 6 | 3 |48 |59 |44 |55 |40 |63 |
   |---|---|---|---|---|---|---|---|
 4 |21 |46 |23 |38 |49 |62 |11 |56 |
   |---|---|---|---|---|---|---|---|
 3 | 2 |17 |34 |45 |54 |39 |50 |27 |
   |---|---|---|---|---|---|---|---|
 2 |33 |20 |37 |24 |35 |28 |53 |12 |
   |---|---|---|---|---|---|---|---|
 1 |16 | 1 |18 |31 |14 |51 |26 |29 |
   |---|---|---|---|---|---|---|---|
 0 |19 |32 |15 |36 |25 |30 |13 |52 |
   |---|---|---|---|---|---|---|---|
     0   1   2   3   4   5   6   7 
#((1 . 1) (0 . 3) (1 . 5) (0 . 7) (2 . 6) (0 . 5) (1 . 7) (3 . 6) (5 . 7) (7 . 6) (6 . 4) (7 . 2) (6 . 0) (4 . 1) (2 . 0) (0 . 1) (1 . 3) (2 . 1) (0 . 0) (1 . 2) (0 . 4) (1 . 6)
  (2 . 4) (3 . 2) (4 . 0) (6 . 1) (7 . 3) (5 . 2) (7 . 1) (5 . 0) (3 . 1) (1 . 0) (0 . 2) (2 . 3) (4 . 2) (3 . 0) (2 . 2) (3 . 4) (5 . 3) (6 . 5) (7 . 7) (5 . 6) (3 . 7) (4 . 5)
  (3 . 3) (1 . 4) (0 . 6) (2 . 5) (4 . 4) (6 . 3) (5 . 1) (7 . 0) (6 . 2) (4 . 3) (5 . 5) (7 . 4) (6 . 6) (4 . 7) (3 . 5) (2 . 7) (4 . 6) (5 . 4) (7 . 5) (6 . 7)) 
```


## Acknowledgments

* AIMA (Artificial Intelligence - A Modern Approach)
