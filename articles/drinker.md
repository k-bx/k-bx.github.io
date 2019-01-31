# Struggling With My Drinker's Problem

I've recently stumbling upon the [Drinker's Problem](https://en.wikipedia.org/wiki/Drinker_paradox) accidentally:

> The **drinker paradox** (also known as the **drinker's theorem**, the **drinker's principle**, or the **drinking principle**) is a [theorem](https://en.wikipedia.org/wiki/Theorem) of [classical](https://en.wikipedia.org/wiki/Classical_logic) [predicate logic](https://en.wikipedia.org/wiki/Predicate_logic) which can be stated as "There is someone in the pub such that, if he is drinking, then everyone in the pub is drinking." It was popularised by the [mathematical logician](https://en.wikipedia.org/wiki/Mathematical_logician) [Raymond Smullyan](https://en.wikipedia.org/wiki/Raymond_Smullyan), who called it the "drinking principle" in his 1978 book *What Is the Name of this Book?*

For people who love Greek letters, here's the formal problem definition:
$$
\exists{x \in P}. \left[D(x) \in P. D(y)\right].
$$

In this post, I will:

- articulate a bit on the "paradoxical" part of the problem
- describe my way of proving it constructively in [cubicaltt](https://github.com/mortberg/cubicaltt)
- mention few things which were discovered as a pleasant surprise as I went along

## Paradoxiness

There are few peculiar things about this paradox. First, it isn't really a paradox, there's only a paradox-ish part about it. The part is that linguistically, it seems like the person is causing everyone else to drink somehow, whereas in reality, what is stated is that you can always find a person, such that it just so happens that everyone else is drinking along. To make a similar statement not involving conscious agents: "every day, I can find a person, such that, if that person is up, then the sun is up". This doesn't mean that the sun follows some person's actions. What we see is a principle named [Correlation does not imply causation](https://en.wikipedia.org/wiki/Correlation_does_not_imply_causation).

## Proving it constructively

Let's start with the hardest part, getting the theorem statement precisely and correctly:

```haskell
drinker (A : U) (D : A -> U) (x : A)
  : ((x : A) * ((y : D x) -> (z : A) -> D z))
  = undefined
```

Notes:

- `A` is the type of all drinkers
- `D` is the property meaning "is drinking"
- I didn't have the `x : A` present at first, it was needed later as I went along, but it makes total sense, since we need the bar to have at least one person, otherwise the theorem wouldn't make sense. In other words, our type must be **inhabited**
- 

Please send your feedback in Issues or PRs in [this blog's repo](https://github.com/k-bx/k-bx.github.io).