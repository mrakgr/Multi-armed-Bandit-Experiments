// 3/21/2017:

// The issue that I am having right now is that I am trying too hard to mesh the parser combinator pattern
// while still taking advantage of F#'s innate pattern matching capabilities.

// Before today I thought I did a brilliant job, but if it was so brilliant, making a single modification would
// not be so difficult.

// Sure I know that what I have written is right so there is brilliance there, but the convoluted code made
// me completely miss the fact that I needed to add array indexing to MSet.

// Besides that, pattern matching in functional language is kind of like a DSL and what I am painfully starting to realize,
// it does not quite have the composability that I'd really want. Well, before jumping to any conclussions, let me try
// out a few ideas.