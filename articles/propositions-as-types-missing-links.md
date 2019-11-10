# Propositions as Types: Some Missing Links

I've recently started studying Topology, which turned out to be a topic filled with set theory and theorems around it. Unfortunately, my brain was well-poisoned by Type Theory by the time I've started, so I couldn't help  but itch my hands in attempt of encoding things in MLTT.

In this blog post, I will list the things I consider missing (or rarely mentioned) when people are talking about Propositions-as-Types with regards to proving set theory theorems in Type Theory, and will show a simple proof of a theorem from the book "Topology Without Tears", one merely about Topology but rather a set-theoretic exercise in proofs.

## The Missing Links

List of things I don't see mentioned very often:

**Equality of Sets**. If sets are types, then what would their equality look like? I think there are two options here: first is to use the notion of Isomorphism, the second is to assume Univalence and get equality-via-isomorphism "for free". In this post's example, I'll take the former approach. So if we are required to prove that sets `A` and `B` are equal, we will be building an element of type `A ≃ B`. Just to remind ourselves what an Isomorphism is. For some types `A` and `B`, an isomorphism between them is defined as:

```agda
record _≃_ (...) where
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
```

An Isomorphism is a record of four fields:

- a function taking us from `A` to `B`
- a function bringing us backwards
- an element of type `from (to x) ≡ x`, proving that if we go from `A` to `B` and back using our implementation, we'd get the same element we started from
- and a similar law `to (from y) ≡ y` going the other direction

So, for example, we could then provide an isomorphism between numbers and their string representations via encoding them as Roman Numbers, for example.

**Subsets**. There are actually two distinct approaches I've encountered with regard to subsets, and we're going to use both. One is a.k.a. the HoTT book, Chapter 3.5, encoding a subset of some set `A` of elements satisfying some predicate `P` (`{ x ∈ A | P(x)}`) as:
$$
\Sigma_{x : A}P(x)
$$
So, a type describing a pair of element `x : A` and a proof that that element satisfies some predicate `P` is describing all the values which are members of this subset.

The second notion I decided to use is Embedding, something very much like Isomorphism, but without one law:

```agda
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
```

So, for example, we can prove the embedding of `Bool`s to `Nat`s by saying that `True` will map to `1` and `False` will map to `0`. Then `to(from(x)) ≡ x` will hold for both, `x` being `True` or `False`, but the opposite `from(to(x))` would clearly fail for anything `> 1` (`from(to(3)) ≡ 1`).

**Set of subsets.** An interesting one to crack to me was the notion of a "set of subsets" back from when I've studied what a Topology is. So, if a subset is a pair of element of `X` and a proof that it satisfies some predicate `P`, then set of subsets must be 

- Unions, Intersections. Finite and infinite. If we want to prove things from set theory, we need to describe a way to encode finite and infinite unions and intersections. In this post I will only show the encoding of infinite union
- "Dynamic Membership Checking". Proofs in set theory often go on and say "if x is a member of X then ...". I'm not touching examples of these proofs here, but I think this should be mentioned explicitly. Most situation like this either translate in a "there exists an `x : X`", or involve the Law of Excluded Middle
- Subsets. We need to talk about subsets (finite and infinite)
- Sameness of an element. It's often said that "if an element x is a member if U, it's also a member of V", but if U and V are distinct types, `x` can't have both types. In Type Theory, this has to have a bit more structure. For example, 

TODO:

- union truncation
- the need for levels in isomorphism definition
- rename Set to Type