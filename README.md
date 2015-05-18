# [🏡](http://gyoudmon.org/~wargrey:sakuyamon)[<sub>🐈</sub>](http://gyoudmon.org/~wargrey:digignome)Tamer's Handbook: Sakuyamon

> _**Sakuyamon**_ is the Queen of [gyoudmon.org](http://gyoudmon.org)  as
> well as the _**digital world**_.

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
[sakuyamon.rktl](http://gyoudmon.org/~wargrey:sakuyamon/sakuyamon.rktl)
>       + 📑Sakuyamon, Realize!
>         - 💚 1 - realize --port 8443 [fresh]
>         - 💚 2 - realize --port 8443 [already in use]
>       + 📑Keep Realms Safe!
>         - 💚 1 - realm --in-place
>     + 📖
[dispatch.rktl](http://gyoudmon.org/~wargrey:sakuyamon/dispatch.rktl)
>       + 📑Main Terminus
>         - 💚 1 - HTTP OPTIONS
>         + 📑Function URLs
>           - 💚 1 - 200: /d-arc/collect-garbage@::1
>           - 💚 2 - 403: /d-arc/collect-garbage@127
>           - 💚 3 - 200: /d-arc/refresh-servlet@::1
>           - 💚 4 - 403: /d-arc/refresh-servlet@127
>       + 📑Per-Tamer Terminus
>         - 💚 1 - HTTP OPTIONS
>         + 📑Function URLs
>           - 💚 1 - 200: /~wargrey/d-arc/refresh-servlet@::1
>           - 💚 2 - 403: /~wargrey/d-arc/refresh-servlet@127
>         + 📑Digest Authentication
>           - 💚 1 - 200: guest@::1:digest
>           - 💚 2 - 401: guest@127:digest
>           - 💚 3 - 200: wargrey@127:digest
>       + 📑Per-Digimon Terminus
>         - 💚 1 - HTTP OPTIONS
>         + 📑Basic Authentication
>           - 💚 1 - 200: guest@::1:basic
>           - 💚 2 - 401: guest@127:basic
>           - 💚 3 - 200: wargrey@127:basic
>     + 📖
[seo.rktl](http://gyoudmon.org/~wargrey:sakuyamon/seo.rktl)
>       + 📑Server Side Redirections
>         + 📑dir -> dir/
>           - 💚 1 - 302: .
>         + 📑expand ~:
>           - 💚 1 - 302: ~
>           - 💚 2 - 302: :
>           - 💚 3 - 302: ~:
>           - 💚 4 - 302: :sakuyamon
>           - 💚 5 - 302: ~wargrey:
>         + 📑rktl -> html
>           - 💚 1 - 302: ./t/h.lp.rktl
>     + 📖
[security.rktl](http://gyoudmon.org/~wargrey:sakuyamon/security.rktl)
>       + 📑Bad Users
>         - 💚 1 - 418: ../tamer/.realm.rktd
>
> 📌26 examples, 0 failures, 0 errors, 100.00% Okay.
>
>
[🐈<sub>🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾</sub>](http://gyoudmon.org/~wargrey:sakuyamon)
