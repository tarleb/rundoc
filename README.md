# Rundoc

Rundoc is a tool to run code from Pandoc's code blocks and to insert the
results back into the document.  It aims to provide functionality similar to
Emacs' Org-Babel library.

## Basics

Rundoc is intended to be used as a filter for
[Pandoc](http://johnmacfarlane.net/pandoc).  It recognizes code blocks marked
as runable, evaluates them and re-inserts the result into the document.
Blocks are marked as runable with the special class `rundoc-block`, the
interpreter for the block is chosen via an `rundoc-language` option.  The
following is a simple example in markdown:

```markdown
  ```{.rundoc-block rundoc-language=sh rundoc-exports=result}
  printf 'The answer to the Ultimate Question is %d.' $((6 * 7))
  ```
```

Assuming the above snippet is safed in `rundoc-example.md`, then running

    pandoc --filter rundoc --from markdown --to html rundoc-example.md

will produce

```html
<p><code>The answer to the Ultimate Question is 42.</code></p>
```

Rundoc has multiple options as to what is inserted back into the document.
This can be controlled via the `rundoc-exports` option.  Recognized options
are `code`, `result`, `both` or `none`.  Changing the respective setting in
above example to `rundoc-exports=both` will result in

```html
<div>
<p><code>printf 'The answer to the Ultimate Question is %d.' $((6 * 7))</code></p>
<p><code>The answer to the Ultimate Question is 42.</code></p>
</div>
```


## License

Rundoc is free software: you can redistribute it and/or modify it under the
terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.


Rundoc is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
