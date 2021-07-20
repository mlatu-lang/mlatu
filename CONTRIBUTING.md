Thank you for taking the time to contribute to Mlatu!

Contributions are best done through GitHub issues and GitHub pull requests.

To get started contributing, follow these steps (which assume you have `hub`, the GitHub command-line API installed, available [here](https://github.com/github/hub#installation):

```sh
hub clone mlatu-lang/mlatu
cd mlatu
git checkout -b descriptive_branch_name new
# do stuff, make some changes
git commit -am "done with descriptive_branch_name"
hub fork --remote-name origin
git push origin descriptive_branch_name
hub pull-request
```
(where `descriptive_branch_name` is a descriptive branch name of your feature contributions)
At that point, a pull request will be created on GitHub by `hub` and I will be able to review your changes.

You will need to have `nimble` installed to contribute to Mlatu (other than documentation changes). You can download both `nimble` and `nim` [here](https://nim-lang.org/install.html).

To build, run `nimble build`.
To build and start the repl, run `nimble run`.