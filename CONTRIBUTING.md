# Contributing

## Branch Naming Guidelines
1. Try to use **short**, **less verbose** branch names
2. They should use one of the following start descriptors:

    | Descriptor | When it should be used       |
    | ---------- | ---------------------------- |
    | `impl`     | An addition (implementation) |
    | `del`      | A deletion                   |

3. Then, add a **[section descriptor](./TAGS.md)**, for example `typecheck` or `CI`
4. Finally, add a very short **dash-separated** name related to the branch, for example `function-call` or `type-inference`

Some examples of what a branch could look like:
* `impl/typecheck/variable-assign`
* `del/typecheck/verbose-error`
* `impl/codegen/function-definition`

## How Can I Help?!?!
### Writing Code
See the [trello](https://trello.com/b/5gxtFXun/fluo) for a list of things that need to be done. Open a [pull request](https://github.com/MonliH/fluo/pulls) and explain your changes.

### Bug reports and suggestions
Feel free to open an [issue](https://github.com/MonliH/fluo/issues)
