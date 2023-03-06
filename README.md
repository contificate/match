Implementation of the match compilation algorithm described in Maranget's
[Compiling Pattern Matching to Good Decision Trees](http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf)
with DAG construction inspired by Pettersson's
[A Term Pattern-Match Compiler Inspired by Finite Automata Theory](https://www.classes.cs.uchicago.edu/archive/2011/spring/22620-1/papers/pettersson92.pdf).

---

The algorithm processes (typed) patterns into a pattern matrix and continually specialises the matrix based on the 
constructors present in the left-most column. Each specialisation corresponds to adding an edge in the decision tree 
(labelled with the associated constructor's tag). To avoid duplication, the decision "tree" is really constructed as a DAG by 
means of hash consing.
