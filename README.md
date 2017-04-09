# Auto96 - automatic 9th grader

This is a minimal proof of concept for a really simple idea: compilers should perform trivial algebra for programmers.
Anybody who has used [Wolfram Alpha](https://www.wolframalpha.com/) knows that computers are great at solving equations.
Why, then, do the most commonly used programming languages require that programmers always manually rearrange formulas
to compute different variables?

![example gif](https://raw.githubusercontent.com/rspeele/Auto9G/master/demo.gif)

Example: we have a system selling products with a fixed markup. The price we sell a product at is always the
cost of the product plus a markup factor (say, 0.1), like so:

```fsharp
let priceOfOrder cost markup =
    cost + cost * markup
```

Occasionally a client asks for a product we don't already know the cost of, and a salesperson quotes them a custom price.
After we fulfill their order, we want to know whether we went below our usual markup, so we can generate an alert if a
salesperson is missing the markup on too many orders.

To determine the effective markup of a custom price we might write code like:

```fsharp
let markupOfOrder cost price =
    (price - cost) / cost
```

But this violates [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) because really, it's the same formula.
We just did some basic algebra to move the variables around.

If the requirements change so that we always have a base markup of 1 dollar, we have to change both formulas:

```fsharp
let priceOfOrder cost markup =
    cost + cost * markup + 1

let markupOfOrder cost price =
    (price - 1 - cost) / cost
```

Using this type provider, you would only change one place:

```fsharp
type MarkupFormula = Formula<"price = cost + cost * markup + 1">

let priceOfOrder cost markup = MarkupFormula.price(cost = cost, markup = markup)
let markupOfOrder cost price = MarkupFormula.markup(cost = cost, price = price)
```

# What it doesn't do

Since this is just a proof of concept, the following things are currently unsupported:

* Any equation that is more complex than a quadratic polynomial. This is why it's called auto 9th-grader, not auto
  college-student. (Please do not contact me telling me that when you were in 9th grade you could do X, Y, or Z,
  because I don't care). I have very little experience in CAS programming (consisting of this project's commit history),
  so I am sure somebody else could improve on the basic concept with a bit more experience in that domain!

* Decimal literals like 1.5. This is just a limitation of the parser, not the solver. You can easily work around
  this by using a variable for those constants and supplying the variable value at runtime.

* Calculations using types other than `System.Decimal`. I felt this was a reasonable default as it's what most .NET
  programmers would use for business logic calculations. However, the solver produces a formula tree which could be
  translated to a calculation on `float`s or an arbitrary precision `Rational` type, so any of those should be possible.
