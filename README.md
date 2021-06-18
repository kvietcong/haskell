# haskell
Just a small repo for me to do Haskell experimentation

`LeJSON.hs` and `Comonads.hs` are probably the more interesting things I've done with Haskell

Read my long winded rant below if you're interested in my experience learning it XD

## Comments On My Time Spent with Haskell
Haskell is a really elegant language. This was my first Functional Programming
Language and the paradigm is really interesting. Having functions so central
to the process made function composition and modularity seem trivial at times.
The fact that Haskell is also pure was awe inspiring at times as it started
becoming more "math-like" in its wonder. It seemed like this cool combination
of math and programming. An example from this would be infinite data structures
and how they can be used. They just make solutions so clean at times and
allow you to separate your "data" from "logic". Haskell also had features that
made programming so much nicer like pattern matching and the best type system
I've used.

Admittedly, Haskell was really hard to truly understand at first. The syntax
and making some beginner programs were fairly simple, but what was hard (but the
coolest part) of learning Haskell was actually the Category theory. Now,
I didn't go the deepest into it, but learning about Monoids, Functors, Applicatives,
and Monads was actually really enlightening. I adored the fact that Haskell
leveraged these concepts to abstract programming patterns while still retaining
the elegance of a pure functional programming language. The fact that the type
system of Haskell is able to express these ideas (AND MORE) so easily
honestly blows my mind.

However, I had three main gripes with Haskell. The lack of "the Haskell way", Windows Support,
and the ecosystem.
- While there is some idiomatic Haskell, I found that there were
    still so many ways to do things and that there was often legacy stuff that made learning
    difficult. One example would be how to deal with Strings. Especially when you
    start getting into using actual libraries, there are like 3 different String
    standards and so many different language extensions you need to know to actually
    used things. It becomes annoying thinking you can do something with a library
    and then they hit you with "nah we need you to activate 5 different Haskell
    Language extensions that most tutorials don't show and use Lazy Bytestring
    Strings." Also, having things like fmap always irked me. I feel like
    they should have removed the old map and generalized the Functor class to have
    a `map` method rather than having a specific fmap thing. (Also liftA vs liftM or return vs pure)
- Windows support was also quite lacking. The install was okay but many things simply did
    not work on Windows for me. The most annoying being input. I feel like Windows IO with
    Haskell is absolutely impossible
- Sadly, at least to me it felt like it, the Haskell ecosystem was the most user friendly. I appreciate
    the vast amount of volunteer work that has gone into making some really great libraries but the
    ecosystem as a whole lacks a lot of clear documentation and good beginner guides. It was quite hard
    to learn some things (like I didn't even know about Lenses until I started using libraries) and
    it feels like going further in Haskell takes a lot of individual effort. While there's nothing wrong
    with putting in effort, it would've been a lot nicer if there were more user friendly things in the
    Haskell ecosystem. (I'm hopeful the Haskell Foundation will help fix this)

Overall, I really like Haskell. While I don't think I'll be using it to create any bigger projects
anytime soon, I find it a really nice language.

