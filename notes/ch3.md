# Ch 3

## 3.3

### 3.3.4

Depth and size inductions can be derived from structural induction. Thus, let's just talk about structural induction.

We introduce a relationship $<'$ for terms: $s <' t$ if $s$ is a subterm of $t$. Then we can take the reflexive and transitive closure of $<'$: $\le$. $\le$ is a partial order on terms. $P(x)$ is preserved by $<'$, thus it's preserved by $\le$.

Since S is the smallest set generated using subterms, for any term $t \in S$ there is a subterm relationship from the constants in S to t. Thus for any term $t \in S$, at least one constant $c$ has $c \le t$. Thus $P(x)$ for all $t \in S$. QED.
