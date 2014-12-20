# Sakuyamon

Be in charge of [gyoudmon.org](http://gyoudmon.org). <p align="center">
<img src="/stone/brainstorm.png" /> </p>

## The **Digivice** Scripts
* [**sakuyamon-realize.rkt**](digivice/sakuyamon-realize.rkt): Launch
  the Web Server.

## Project Conventions

How to build a _Digital World_? Okay, we don't start with the _File
Island_, but we own some concepts from the _Digimon Series_.

### Hierarchy

**Note** Project or Subprojects are organized as the _digimons_, and
each of them may be separated into several repositories.
* **village** is the birth place of _digimons_. Namely it works like
  `src`.
* **digitama** is the egg of _digimons_. Namely it works like
  `libraries` or `frameworks`.
* **digivice** is the interface for users to talk with _digimons_.
  Namely it works like `bin`.
* **tamer** is the interface for developers to train the _digimons_.
  Namely it works like `test`.
* **island** manages guilds of _digimons_. Hmm... Sounds weird,
  nonetheless, try `htdocs` or
  `webroot`:stuck\_out\_tongue\_winking\_eye:.
* **board** stores the configuration instructions that a _digimon_
  should follow. Sounds like `etc`.
* **stone** stores the immutable meta-information or ancient sources to
  be translated. Yes, it' the _Rosetta Stone_.

### Version

Obviousely, our _digimons_ have their own life cycle.
* **Baby I**: The 1st stage of _digimon evolution_ hatching straighty
  from her _digitama_. Namely it's the `Alpha Version`.
* [ ] **Baby II**: The 2nd stage of _digimon evolution_ evolving quickly
  from **Baby I**. Namely it's the `Beta Version`.
* [ ] **Child**: The 3rd stage of _digimon evolution_ evolving from
  **Baby II**. At the time _digimons_ are strong enough to live on their
  own.
