# Struggling With My Drinker's Problem

I've recently stumbled upon the [Drinker's Problem](https://en.wikipedia.org/wiki/Drinker_paradox) accidentally:

> The **drinker paradox** (also known as the **drinker's theorem**, the **drinker's principle**, or the **drinking principle**) is a [theorem](https://en.wikipedia.org/wiki/Theorem) of [classical](https://en.wikipedia.org/wiki/Classical_logic) [predicate logic](https://en.wikipedia.org/wiki/Predicate_logic) which can be stated as "There is someone in the pub such that, if he is drinking, then everyone in the pub is drinking." It was popularised by the [mathematical logician](https://en.wikipedia.org/wiki/Mathematical_logician) [Raymond Smullyan](https://en.wikipedia.org/wiki/Raymond_Smullyan), who called it the "drinking principle" in his 1978 book *What Is the Name of this Book?*

Lovely book name. For people who love Greek letters, here's the formal problem definition:
$$
\exists{x \in P}. \left[D(x) \in P. D(y)\right].
$$

The non-formal verbal proof is remarkably simple to understand. Here's the quote from Wikipedia:

> The proof begins by recognizing it is true that either everyone in the pub is drinking, or at least one person in the pub is not drinking. Consequently, there are two cases to consider:[[1\]](https://en.wikipedia.org/wiki/Drinker_paradox#cite_note-Smullyan-1)[[2\]](https://en.wikipedia.org/wiki/Drinker_paradox#cite_note-Images_of_SMC_Research_1996-2)
>
> 1. Suppose everyone is drinking. For any particular person, it cannot be wrong to say that *if that particular person is drinking, then everyone in the pub is drinking* — because everyone is drinking. Because everyone is drinking, then that one person must drink because when *that person* drinks *everybody*drinks, everybody includes that person.
> 2. Otherwise at least one person is not drinking. For any nondrinking person, the statement *if that particular person is drinking, then everyone in the pub is drinking* is formally true: its [antecedent](https://en.wikipedia.org/wiki/Antecedent_(logic)) ("that particular person is drinking") is false, therefore the statement is true due to the nature of [material implication](https://en.wikipedia.org/wiki/Material_conditional#Definition) in formal logic, which states that "If P, then Q" is always true if P is false.[[1\]](https://en.wikipedia.org/wiki/Drinker_paradox#cite_note-Smullyan-1)[[2\]](https://en.wikipedia.org/wiki/Drinker_paradox#cite_note-Images_of_SMC_Research_1996-2) (These kinds of statements are said to be [vacuously true](https://en.wikipedia.org/wiki/Vacuous_truth).)

In this post, I will:

- articulate a bit on the "paradoxical" part of the problem
- describe my way of proving it constructively in [cubicaltt](https://github.com/mortberg/cubicaltt)
- mention few things which were discovered as a pleasant surprise as I went along

## Paradoxiness

There are few peculiar things about this paradox. First, it isn't really a paradox, there's only a paradox-ish part about it. The part is that linguistically, it seems like the person is causing everyone else to drink somehow, whereas in reality, what is stated is that you can always find a person, such that it just so happens that everyone else is drinking along. To make a similar statement not involving conscious agents: "every day, I can find a person, such that, if that person is up, then the sun is up". This doesn't mean that the sun follows some person's actions. What we see is a principle named [Correlation does not imply causation](https://en.wikipedia.org/wiki/Correlation_does_not_imply_causation).

## Stating it constructively

Let's start with the hardest part, getting the theorem statement precisely and correctly:

```haskell
drinker
  (A : U)         -- [1]
  (D : A -> U)    -- [2]
  (x : A)         -- [3]
  : ( (x : A)     -- [4]
    * ((y : D x)  -- [5]
       -> (z : A) -- [6]
       -> D z     -- [7]
      )
    )
  = undefined
```

- **[1]**: `A` is the type of all drinkers
- **[2]**: `D` is the property meaning "is drinking"
- **[3]**: I didn't have the `x : A` present at first, it was needed later as I went along, but it makes total sense, since we need the bar to have at least one person, otherwise the theorem wouldn't make sense. In other words, our type must be **inhabited**
- **[4]**: I will return you a specific person
- **[5]**: and a function, which states, that given a fact that person is drinking
- **[6]**: for any person in the bar
- **[7]**: I will prove that that person is drinking

## Proving it: All or Counter-example

Not knowing what else can I do, I decided to follow the verbal proof, stated in the beginning. So, we need to begin by stating "either everyone is drinking, or there's someone who is not":

```haskell
data N0 =      -- [1]

-- [2]
neg (A : U) : U = A -> N0

-- [3]
data or (A B : U)
  = inl (a : A)
  | inr (b : B)

-- [4]
allOrCounterex
  (A : U)
  (D : A -> U)
  : (or ((x : A) -> D x)    -- [5]
        ((x : A) * (neg (D x))) -- [6]
    )
  = undefined
```

- **[1]**: the "impossible", or "absurd" type, we cannot construct its value. Copied from prelude.ctt
- **[2]**: negation in constructive mathematics is done via giving a function that returns absurd given that value. Copied from prelude.ctt
- **[3]**: good old `Either` type. Copied from prelude.ctt
- **[4]**: notice how all this function needs is the type and the property, no "real values" (it should work for non-inhabited types)
- **[5]** you either get `inl` with "for all `x:A`, I'll prove they're drinking"
- **[6]**: or you get `inr` with specific person `x:A`, and a proof they aren't (`neg (D x)`)

We'll get back to the actual implementation later, let us first see if we can actually use this for the full proof.

## Finishing the drinker

We need to case-split on the `allOrCounterex` result, let's create a continuation doing that:

```haskell
-- [1]
efq (A : U) : N0 -> A = split {}

-- [2]
drinkerC1
  (A : U)
  (D : A -> U)
  (x : A)
  : (or ((q : A) -> D q)         -- [3]
        ((q : A) * (neg (D q))))
    ->
    ( (x : A)
    * ((y : D x)
       -> (z : A)
       -> D z
      )
    )
  = split
  inl allD ->     -- [4]
    ( x           -- [5]
    , (\(_ : D x) ->  -- [6]
         \(z : A) ->  -- [7]
           allD z))
  inr pair ->     -- [8]
    ( pair.1      -- [9]
    , \(y : D pair.1) ->
        \(z : A) ->
          efq (D z) (pair.2 y))  -- [10]

drinker ... = drinkerC1 A D x (allOrCounterex A D)
```

- **[1]**: We'll need this in our proof. Given an absurd value (which can never be constructed!), I can prove anything. This is akin to saying "which is impossible becuase earlier we've shown the opposite xyz, leading to contradiction" in your verbal proof. Copied from prelude.ctt
- **[2]**: function's signature is copied from `drinker`, and we only add the first parameter in its return type
- **[3]**: here's that parameter, which is an alpha-renamed result from `allOrConterex`
- **[4]**: branch when everybody's drinking
- **[5]**: then any person can suite as a proof, so let's take the inhabitant
- **[6]**: we're not interested in the fact he is drinking (we know that)
- **[7]**: we're just passing the `allD` to prove that everyone's drinking
- **[8]**: in "not everyone's drinking" case, where we get a specific person and a proof they're not drinking
- **[9]**: we return that person as "the answer"
- **[10]**: most interesting part. We need to prove "if that person's drinking, everybody else does", but we've got our "they are not drinking" value, so we pass "that person is drinking" (`y`) to the "that person is **not** drinking" function (`pair.2`), and get an absurd, from which we can prove anything (via `efq`)

## De Morgan

This was satisfying already, but now there is another problem that's bugging: the whole "either everyone is drinking, or somebody is not" sounds terrifically obvious to the listener, but is this a fundamental claim? Doesn't sound like an axiom or a law to me, it feels like we can break this down a bit more.

I started searching for the drinker's solution on the internet, and came up to the post called [Seemingly impossible constructive proofs](http://math.andrej.com/2014/05/08/seemingly-impossible-proofs/), which had another one, called [Seemingly impossible functional programs](http://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/) before it. It seemed unrelated at first, but I decided to give the "functional programs" one a go before I switch to the main thing. It turned out rewarding, indeed.

In that post, something that caught my eye was this:

```haskell
> forevery p = not(forsome(\a -> not(p a)))
```

An implementation of "for every", using "for some". It also explicitly links the [De Morgan's Laws](https://en.wikipedia.org/wiki/De_Morgan's_laws) page.

Now, I was aware of the whole De Morgan thing related to conjunctions, disjunctions and negations, but completely forgot (or did I not know?) about its use for the quantifiers! Here are the laws for conjunction and disjunction:
$$
\neg(P \land Q) \vdash (\neg P \lor \neg Q).\\
\neg(P \lor Q) \vdash (\neg P \land \neg Q).
$$
And here they are for quantifiers:
$$
\forall x \, P(x) \equiv \neg [ \exists x \, \neg P(x)]\\
\exists x \, P(x) \equiv \neg [ \forall x \, \neg P(x)]
$$
And it makes total sense when you think about it. "All things hold" is the same as "not (some thing doesn't)". "Some thing holds" is the same as "not (all thing do)".

```haskell
deMorgan
  (A : U)
  (D : A -> U)
  : (neg ((x : A) -> D x)) -- [1]
    -> ( (x : A)           -- [2]
       * (neg (D x)))
  = undefined
```

- **[1]**: if we were to have a "not everyone's drinking" statement
- **[2]**: then we could turn it into a "there exists a person, for whom we'd give a proof they're not drinking"

This gives us a hint on how to get from "not everyone's drinking" branch to a useful dependent pair, but not a complete solution yet. What's missing?

## Law of Excluded Middle

What's missing is a way to get something out of thin air. The notoriouf law of excluded middle, a constructivist's nightmare:

```haskell
lem (A : U)
  : or A (neg A)
  = undefined
```

Using `lem`, we can made our first case-splitting continuation on `allOrCounterex` and finish the proof:

```haskell
allOrCounterexC1
  (A : U)
  (D : A -> U)
  : (lem : or ((x : A) -> D x)
              (neg ((x : A) -> D x)))
  -> (or ((x : A) -> D x)
         ((x : A) * (neg (D x))))
  = split
  inl f -> inl f  -- [1]
  inr f -> inr (deMorgan A D f)  -- [2]

allOrCounterex ...
  = allOrCounterexC1 A D 
      (lem ((x : A) -> D x)) -- [3]
```

- **[1]**: "everyone's drinking" branch fits ideally as is
- **[2]**: "not (everyone's drinking)" is translated via `deMorgan` introduced earlier, precisely matching what we need
- **[3]**: we use `lem`, passing a type meaning "everyone's drinking", which constructs the `or` we split upon

Voila!

## Summary

In this post:

- we've seen the usage of absurd type `N0`, the notion of negation (`neg`), "prove by absurdity" (`efq`), good old `or` type
- introduced an explicit use of LEM only when we needed it
- introduced De Morgan's law for quantifiers, jumping from "forall" statement to "there exists" one
- had an exercise in proving things constructively and formally

Well, that's one drinking problem less in my life. Pass that scotch&beer, let's celebrate! 🥃🍺

Full source code can be found in [drinker.ctt](./drinker/drinker.ctt).

Please send your feedback in Issues or PRs in [this blog's repo](https://github.com/k-bx/k-bx.github.io).