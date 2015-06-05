# [🏡](http://gyoudmon.org/~wargrey:sakuyamon)[<sub>🐈</sub>](http://gyoudmon.org/~wargrey:digignome)Tamer's Handbook: Sakuyamon

> _**Sakuyamon**_ is the Queen of [gyoudmon.org](http://gyoudmon.org)  as
> well as the _[**digital
> world**](https://github.com/digital-world/DigiGnome#digital-world)_.

Please do not ask me why I choose **Racket**. Meanwhile I don’t know
more than you about whether it is suitable for this kind of tasks since
its real world applications are damn harder to be found than to be heard
of. So you get it, I mean I like it and just for fun, maybe also paid
for it.

It is intentionally designed not to work with `raco setup`, nor any
other installing processes. Another good news is that, you are safe to
run it as **root**, after making as usual it will work properly.

_**Have tested with MacOSX, OpenIndiana and Ubuntu.**_

---

> + 📚Behaviors and Features
>     + 📖
[sakuyamon.rkt](http://gyoudmon.org/~wargrey:sakuyamon/sakuyamon.rkt)
>       + 📑Sakuyamon, Realize!
>         - 💚 1 - realize?
>       + 📑Keep Realms Safe!
>         - 💚 1 - realm --in-place
>     + 📖
[dispatch.rkt](http://gyoudmon.org/~wargrey:sakuyamon/dispatch.rkt)
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
[seo.rkt](http://gyoudmon.org/~wargrey:sakuyamon/seo.rkt)
>       + 📑/robots.txt
>         - 💚 1 - 200|503: /robots.txt
>         - 💚 2 - 200|503: /~wargrey/robots.txt
>         - 💚 3 - 200|503: /~wargrey:sakuyamon/robots.txt
>       + 📑Server Side Redirections
>         + 📑dir -> dir/
>           - 💚 1 - 302: .
>           - 💚 2 - 302: ~:
>           - 💚 3 - 302: stone
>         + 📑rkt -> html
>           - 💚 1 - 302: seo.rkt
>           - 💚 2 - 302: dir/dot.lp.rkt
>     + 📖
[security.rkt](http://gyoudmon.org/~wargrey:sakuyamon/security.rkt)
>       + 📑Bad Users
>         - 💚 1 - 418: ../tamer/.realm.rktd
>
> 📌26 examples, 0 failures, 0 errors, 100.00% Okay.
