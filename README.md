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
[sakuyamon.rkt](http://gyoudmon.org/~wargrey/.sakuyamon/sakuyamon.rkt)
>       + 📑Sakuyamon, Realize!
>         - 💚 1 - realize --port 8443 [fresh]
>         - 💚 2 - realize --port 8443 [already in use]
>     + 📖
[dispatch.rkt](http://gyoudmon.org/~wargrey/.sakuyamon/dispatch.rkt)
>       + 📑Main Terminus
>         - 💚 1 - .digimon/~user/readme.t
>         + 📑Function URLs
>           - 💚 1 - [::1]/d-arc/collect-garbage
>           - 💚 2 - [127]/d-arc/collect-garbage
>           - 💚 3 - [::1]/d-arc/refresh-servlet
>           - 💚 4 - [127]/d-arc/refresh-servlet
>       + 📑Per-User Terminus
>         - 💚 1 - readme.t
>         + 📑Function URLs
>           - 💚 1 - [::1]/d-arc/refresh-servlet
>           - 💚 2 - [127]/d-arc/refresh-servlet
>       + 📑Per-Digimon Terminus
>         - 💚 1 - readme.t
>         + 📑Rewrite URL
>           - 💚 1 - 302: !/../.
>           - 💚 2 - 302: !/../dispatch.rkt
>           - 💚 3 - 302: dir/lp.rkt
>           - 💚 4 - 418: ./../../tamer.rkt
>         + 📑Basic Authentication
>           - 💚 1 - 200: [::1]guest
>           - 💚 2 - 401: [127]guest
>           - 💚 3 - 200: [127]tamer
>
> 📌18 examples, 0 failures, 0 errors, 100.00% Okay.
>
>
[🐈<sub>🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾</sub>](http://gyoudmon.org/~wargrey/.sakuyamon)
