# [🏡<sub>🐈</sub>](http://gyoudmon.org/~wargrey/.sakuyamon)Tamer's Handbook: Sakuyamon

> _Sakuyamon_ is the Queen of [gyoudmon.org](http://gyoudmon.org).

Please do not ask me why I choose **Racket** to build my website.
Meanwhile I don't know more than you about whether it is suitable for
this kind of tasks since its real world applications are damn harder to
be found than heard of. So you get it, I mean I like it and just for
fun, maybe also paid for it.

Due to my poor knowledge on **Racket**, infrasturctures are designed as
lightweight as possible. Instances include almost all batteries with
zero configuration, and they should communicate with each other easily.
Keep it simple but also ready for large-scale extension.

Each instance has 3 types of _termini_, or _htdocs_ or _webroot_:

* _**Main Terminus**_: The major and default one shipped with the
  instance.

* _**Per-User Terminus**_: The one made for system users locates in
  their home directories.

* _**Per-Digimon Terminus**_: The one made for project wikis like
  [Github
  Pages](https://help.github.com/articles/what-are-github-pages/).

---

> + 📚Behaviors and Features
>     + 📖
[sakuyamon.rkt](http://gyoudmon.org/~wargrey/.sakuyamon/sakuyamon.rkt)
>       + 📑Sakuyamon, Realize!
>         - 💚 1 - realize --port 8080 [fresh]
>         - 💚 2 - realize --port 8080 [already in use]
>         + 📑Dispatch Rules!
>           + 📑Main Terminus
>             - 💚 1 - /
>             - 💚 2 - /~
>             - 💚 3 - /error.css
>             - 💚 4 - /user/.digimon
>             - 💚 5 - /.digimon/~user
>           + 📑Per-User Terminus
>             - 💚 1 - /~user
>             - 💚 2 - /~user/.
>             - 💚 3 - /~user/style.css
>           + 📑Per-Digimon Terminus
>             - 💚 1 - /~user/.digimon
>             - 💚 2 - /~user/.digimon/404.html
>           + 📑Function URLs
>             - 💚 1 - [::1]/conf/collect-garbage
>             - 💚 2 - [127.0.0.1]/conf/collect-garbage
>       + 📑Typed Racket Libraries!
>         - 💚 1 - Web Application
>
> 📌15 examples, 0 failures, 0 errors, 100.00% Okay.
>
>
[🐈<sub>🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾🐾</sub>](http://gyoudmon.org/~wargrey/.sakuyamon)
