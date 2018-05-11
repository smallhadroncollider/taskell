# Contributing

## Bug Reports/Feature Requests

Please check the [roadmap.md](https://github.com/smallhadroncollider/taskell/blob/develop/roadmap.md) before adding any bugs/feature requests to Issues.

## Code

Anyone is welcome to contribute to the project. Check out [roadmap.md](https://github.com/smallhadroncollider/taskell/blob/develop/roadmap.md) for bugs and planned features if you're not sure where to start.

If you're planning on doing a big chunk of work it's probably best to [contact me](mailto:mark@smallhadroncollider.com) first to make sure someone isn't already working on it.

You'll probably want to get up to scratch with [Brick](https://github.com/jtdaugherty/brick) before working on any of the UI code.

### Style

This is my first Haskell project, so the style is often a bit inconsistent. I'm also probably not doing things in the most Haskelly manner (I still haven't used an Applicative anywhere). So, feel free to use whatever style you prefer. We can always refactor.

I'm also very bad at comments: but please don't follow my example on that.

### Git

Please use the [git-flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model. You should only commit to `feature/*` branches: **do not commit to `develop` or `master` directly**. This will keep commit history simpler and make handling pull requests easier.

Please also use the `--rebase` and `--ff-only` options when running `git pull` - although if you're on your own `feature/*` branch this shouldn't make any difference.

### Testing

I'm trying to add more unit tests to the codebase, although coverage is still pretty poor. At a minimum any pull requests should make sure all tests pass (use `stack test` before you commit anything). If you're adding/changing a significant amount of code then adding appropriate unit tests would be much appreciated.
