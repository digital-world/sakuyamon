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
>       + 📑Typed Racket Libraries!
>         - 💚 1 - Web Application
>     + 📖
[dispatch.rkt](http://gyoudmon.org/~wargrey/.sakuyamon/dispatch.rkt)
>       + 📑Main Terminus
>         - 💚 1 - /
>         - 💚 2 - /~
>         - 💚 3 - /user/.digimon
>         - 💚 4 - /.digimon/~user
>         + 📑Function URLs
>           - 💚 1 - [::1]/d-arc/collect-garbage
>           - 💚 2 - [127.0.0.1]/d-arc/collect-garbage
>       + 📑Per-User Terminus
>         - 💚 1 - /~user
>         - 💚 2 - /~user/.
>       + 📑Per-Digimon Terminus
>         - 💚 1 - /~user/.digimon
>         + 📑Rewrite URL
>           - 💚 1 - 200: ?/../
>           - 💚 2 - 418: ../index.html
>           - 💚 3 - 418: ../../tamer.rkt
>           - 💚 4 - 200: !/../dispatch.rkt
>
> 📌16 examples, 0 failures, 0 errors, 100.00% Okay.
>
>
[🐈<sub>🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾</sub>](http://gyoudmon.org/~wargrey/.sakuyamon)
