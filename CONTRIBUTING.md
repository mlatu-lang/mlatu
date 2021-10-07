# Thank you

Thank you for contributing to mlatu! It is very important to us that the community is involved in the codebase and changes made; we don't want to fall into a "Benevolent Dictator for Life" model.

## Introduction and Overview

Pull requests are very welcome. For major changes, please open an issue first to discuss what you would like to change. You can also join the Discord server listed on the README for help setting up and making changes, as well as asking questions about the codebase.

## Setup

To create a local copy of mlatu with all necessary dependencies, you will need to install Rust and Swi-Prolog. To install Rust, follow the instructions at <https://rustup.rs>. To install Swi-Prolog, you can find a download at <https://www.swi-prolog.org/Download.html>.

To contribute to mlatu, it's easiest to install the GitHub CLI tool (instructions are at <https://github.com/cli/cli#installation>). After installation, authenticate yourself by running `gh auth login`. Then, fork and download a local copy of the repository by running `gh repo fork mlatu-lang/mlatu`.

## Making changes

Now, make any changes you wish to include in the PR. Make sure to run `cargo fmt` and `cargo clippy` to make sure formatting is correct and there are no lint warnings. To commit your changes, run `git add *` and `git commit -m "Commit message"`. Don't worry too much about the commit message being descriptive, as we will squash the commits before we merge. Then run `gh repo sync` to sync with the online version.

## Creating a pull request

Once you are satisfied with your changes, create a pull request by running `gh pr create`. This will prompt you for the title and the body of the pull request. Please make the title short and descriptive of the changes you've made. The description can be longer and should note any issues this pull request should close, any remaining issues, and anything the mlatu-lang team should know. This will output a URL where you can view the changes you've made and any comments made on that pull request.

Once the pull request is created, one of the mlatu-lang team members will come take a look at your changes and approve or request changes. If the pull request is approved, it will be merged into `main`. That's success! If there are changes requested, we'd ask that you do your best to fix them (asking clarifying questions or for help if you need it is great), and then we will review again.
