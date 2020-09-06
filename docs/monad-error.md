# MonadError

We'll follow the blog post [MonadError for the rest of us](https://blog.codacy.com/monad-error-for-the-rest-of-us/).

In order to propagate and handle application errors, Scala developers, especially those inclined to use functional programming, are accustomed to using monadic data types (Option, Try, Either, etc.) instead of exceptions or returning null.