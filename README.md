# [🏡<sub>🐈</sub>](http://gyoudmon.org/~wargrey/.sakuyamon)Tamer's Handbook: Sakuyamon

> _**Sakuyamon**_ is the Queen of [gyoudmon.org](http://gyoudmon.org).

Please do not ask me why I choose **Racket** to build my website.
Meanwhile I don't know more than you about whether it is suitable for
this kind of tasks since its real world applications are damn harder to
be found than to be heard of. So you get it, I mean I like it and just
for fun, maybe also paid for it.

Due to my poor knowledge on **Racket**, infrasturctures are designed as
lightweight as possible. Instances include almost all batteries with
zero configuration, and they should communicate with each other easily.
Keep it simple but also ready for large-scale extension.

---

> + 📚Behaviors and Features
>     + 📖
[sakuyamon.rktl](http://gyoudmon.org/~wargrey/.sakuyamon/sakuyamon.rktl)
>       + 📑Sakuyamon, Realize!
>         - 💚 1 - realize --port 8443 [fresh]
>         - 💚 2 - realize --port 8443 [already in use]
>     + 📖
[dispatch.rktl](http://gyoudmon.org/~wargrey/.sakuyamon/dispatch.rktl)
>       + 📑Main Terminus
>         - 💚 1 - /trick/.digimon/readme.t
>         + 📑Function URLs
>           - 💚 1 - 200: /d-arc/collect-garbage@::1
>           - 💚 2 - 403: /d-arc/collect-garbage@127
>           - 💚 3 - 200: /d-arc/refresh-servlet@::1
>           - 💚 4 - 403: /d-arc/refresh-servlet@127
>       + 📑Per-User Terminus
>         - 💚 1 - /~wargrey/readme.t
>         + 📑Function URLs
>           - 💚 1 - 200: /~wargrey/d-arc/refresh-servlet@::1
>           - 💚 2 - 403: /~wargrey/d-arc/refresh-servlet@127
>       + 📑Per-Digimon Terminus
>         - 💚 1 - /~wargrey/.sakuyamon/readme.t
>         + 📑Rewrite URL
>           - 💚 1 - 302: !/../.
>           - 💚 2 - 302: ./t/h.lp.rktl
>           - 💚 3 - 418: ../../tamer.rkt
>         + 📑Basic Authentication
>           - 💚 1 - 200: guest@::1
>           - 💚 2 - 401: guest@127
>           - 💚 3 - 200: tamer@127
>
> 📌17 examples, 0 failures, 0 errors, 100.00% Okay.
>
>
[🐈<sub>🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾</sub>](http://gyoudmon.org/~wargrey/.sakuyamon)
