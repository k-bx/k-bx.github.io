# Validating Form Data via Applicative Functors

I've had an interesting mini-journey while I was in search of a way to validate input data in Haskell recently, and ended up implementing one myself, and then got an interesting [comment on reddit](https://www.reddit.com/r/haskell/comments/ad65gz/is_there_a_validationt_like_this_somewhere_or/eddyxvb/), which I will unwrap in this post.

## Initial goal

At first, all I wanted to get was a way to validate my input data structure into another, resulting one. Something which can be done via `ExceptT` like this ([exceptt.hs](./Validating-Form-Data-via-Applicative-Functors/exceptt.hs)):

```haskell
#!/usr/bin/env stack
-- stack --resolver=lts-13.1 script --package text,transformers
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Except
import qualified Data.Text as T
import Data.Text (Text)

newtype ValidUrl =
  ValidUrl Text
  deriving (Show, Eq)

data InputForm = InputForm
  { inpUsername :: Text
  , inpHomepage :: Text
  } deriving (Show, Eq)

data OutputForm = OutputForm
  { outUsername :: Text
  , outPassword :: ValidUrl
  } deriving (Show, Eq)

lengthBetween :: Monad m => Int -> Int -> Text -> ExceptT Text m Text
lengthBetween n m txt =
  if T.length txt < n || T.length txt > m
    then throwE
           ("Length must be between " <> T.pack (show n) <> " and " <>
            T.pack (show m))
    else return txt

validateUrl :: Monad m => Text -> ExceptT Text m ValidUrl
validateUrl txt
  -- exercise to a reader
 = throwE "Invalid URL"

main :: IO ()
main = do
  let inpForm = InputForm "usr" "httpbadurl"
  outForm <-
    runExceptT $
    OutputForm <$> lengthBetween 4 20 (inpUsername inpForm) <*>
    validateUrl (inpHomepage inpForm)
  print outForm
```

The problem here is that `ExceptT` exits quickly, as soon as it encounters the first error:

```
$ ./exceptt.hs
Left "Length must be between 4 and 20"
```

What I want instead is to gather all the error messages with their field names, so that my front-end could show them all nicely.

## The search

I was looking for a type that is similar to `ExceptT`, I would assume the name to be `ValidateT`, so this was what I hoogled for. Surprisingly, all I could find was the `Validation` type, but not its transformer version.

So, at first I've implemented my own `ValidationT` and made [a PR](https://github.com/ekmett/either/pull/58/files) which I encourage you to read. I won't quote the implementation, only the test case scenario:

```haskell
...
    , testCase "combine two ValidateT" $ do
        v1 <- runValidationT $
                ((,) <$> ValidationT (pure (Failure ["first"]))
                     <*> ValidationT (pure (Failure ["second"])))
          :: IO (Validation [String] ((), ()))
        assertEqual "errors get accumulated"
          v1
          (Failure ["first", "second"])
...
```

As you can see, it does what I want. So, what could be the problem? I had a gut feeling that there must be one, so I've asked reddit about it!

## The problem

That's where the wonderful comment by Samuel Gélineau came in:

> Notice that [`Validation`](http://hackage.haskell.org/package/validation-1/docs/Data-Validation.html#t:Validation) intentionally doesn't have a Monad instance! For this reason, it doesn't make much sense to make it into a Monad transformer. An Applicative transformer would make more sense, but since Applicatives compose using [`Compose`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor-Compose.html#t:Compose), there is no need to define both a regular and a transformer version of `Validation`.

Ok, there are few things to unpack here!

## No Monad instance

- `Validation` intentionally has no `Monad` instance

I missed that, because I used `ExceptT` as the base for my implementation, which indeed has one. So, why exactly does it not have one? The docs mention it, actually:

> An `Validation` is either a value of the type `err` or `a`, similar to `Either`. However, the `Applicative` instance for `Validation` *accumulates* errors using a `Semigroup` on `err`. In contrast, the `Applicative` for `Either` returns only the first error.
>
> A consequence of this is that `Validation` has no `Bind` or `Monad` instance. This is because such an instance would violate the law that a Monad's `ap` must equal the `Applicative`'s `<*>`
>
> An example of typical usage can be found [here](https://github.com/qfpl/validation/blob/master/examples/src/Email.hs).

Interesting. I've never seen the `Bind` class before. [`Data.Functor.Bind`](http://hackage.haskell.org/package/semigroupoids-5.3.1/docs/Data-Functor-Bind.html#v:Bind) has not only this `Bind`, but also `Apply` (which is similar to `Bind` but for `Applicative`'s `<*>`). Good to know :)

So, the `ValidationT` type I've implemented in my PR violates the law that its `<*>` must be the same as the `ap` from `Monad`. But what if I were to only implement an `Applicative` instance? Turns out there is no need to do so! That's what the second part of the comment is telling:

## Compose

- An Applicative transformer would make more sense, but since Applicatives compose using [`Compose`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor-Compose.html#t:Compose), there is no need to define both a regular and a transformer version of `Validation`.

Right, so let's try it out and see if we can just use an existing thing called `Compose` to build our `ValidationT`-like functionality. Compose looks like this:

```haskell
newtype Compose f g a = 
  Compose {
    getCompose :: f (g a)
  }
```

Compare it with `ExceptT`:

```haskell
newtype ExceptT e m a =
  ExceptT (m (Either e a))
```

In our usage, we will put `IO` under `f`, and `Validation` under `g`. Here are the new relevant parts of our code. I've kept `throwE` reimplemented under the same name outside for clarity. Full code is at [compose.hs](./Validating-Form-Data-via-Applicative-Functors/compose.hs):

```haskell
throwE :: Applicative m => err -> Compose m (Validation err) a
throwE err = Compose (pure (Failure err))

lengthBetween ::
     Applicative m => Int -> Int -> Text -> Compose m (Validation [Text]) Text
lengthBetween n m txt =
  if T.length txt < n || T.length txt > m
    then throwE
           [ "Length must be between " <> T.pack (show n) <> " and " <>
             T.pack (show m)
           ]
    else pure txt

validateUrl :: Applicative m => Text -> Compose m (Validation [Text]) ValidUrl
validateUrl txt
  -- exercise to a reader
 = throwE ["Invalid URL"]

main :: IO ()
main = do
  let inpForm = InputForm "usr" "httpbadurl"
  outForm <-
    getCompose $
    OutputForm <$> lengthBetween 4 20 (inpUsername inpForm) <*>
    validateUrl (inpHomepage inpForm)
  print outForm
```

It outputs this:

```
./compose.hs
Failure ["Length must be between 4 and 20","Invalid URL"]
```

All right, I've learned few new things, I hope you have as well, this was fun!

Please send your feedback in Issues or PRs in [this blog's repo](https://github.com/k-bx/k-bx.github.io).
