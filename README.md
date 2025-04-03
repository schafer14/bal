# bal -- Write Better 

Banner's Authoring Language, or bal, is a (informal) specification 
for a language that emphesises a better writing experience. Everyone has different
writing tools and techniques; this is a tool for people comfortable
writing in a text editor who want to maximise the flexibility of the
rendering languages. To accomplish this BAL focuses on three goals.

**The Goals of bal:**

1. Keep the syntax as the language as close to English as possible.
2. Maximise the ability to express domain specific concepts in the language.
3. Allow for arbitrary rendering targets.

## What bal looks like

```bal
|> Meta
  author = Banner B. Schafer
  category = documentation
  topics = bal, example
  
  This is a demonstration of bal's syntax. We can *make text bold*,
  or make text /italics/. You can add [custom directives]{ highlight | color = green }.

  |> Section
    title = blocks
    
    You can create custom blocks of content using the '\|>' symbol. These blocks can
    have syntax and symantics relevant to your domain language.

    |> Subsection
      title = nested blocks
      
      Blocks can also be nested into arbitrary structures.
```

## How bal works

Unlike languages you may be familiar with bal uses separate programs to parse, type check,
and render a document. This means you have full control over the keywords of the language.

This allows you to create HTML like language that are very generic:

```bal
|> div
  |> h1
    title = HTML flavoured bal
  |> p
    
    This is a demonstration of HTML flavoured bal
```

But you can also write incredibly specific languages.

```bal
|> Team
  name = Tottenham Hotspur
  stadium = White Hart Lane
  
  |> Players
    
    |> Player
      name = Son Heung-min
      
      Captain of the Tottenham Hotspur, club hero and national icon.
      
    |> Player
      name = Harry Kane
      
      Club hero.
```

## Project status

A reference implementation of the parser is available in a pre-production status.

The specification of the langauge is described, informally, as a series of unit
tests. See [tests](./test/Spec.hs)

Future work:

- building end to end examples of type checkers and parsers.
- build a cli
